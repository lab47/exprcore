// Copyright 2017 The Bazel Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package syntax

// This file defines a recursive-descent parser for Starlark.
// The LL(1) grammar of Starlark and the names of many productions follow Python 2.7.
//
// TODO(adonovan): use syntax.Error more systematically throughout the
// package.  Verify that error positions are correct using the
// chunkedfile mechanism.

import (
	"log"
)

// Enable this flag to print the token stream and log.Fatal on the first error.
const debug = false

// A Mode value is a set of flags (or 0) that controls optional parser functionality.
type Mode uint

const (
	RetainComments Mode = 1 << iota // retain comments in AST; see Node.Comments
)

// Parse parses the input data and returns the corresponding parse tree.
//
// If src != nil, ParseFile parses the source from src and the filename
// is only used when recording position information.
// The type of the argument for the src parameter must be string,
// []byte, or io.Reader.
// If src == nil, ParseFile parses the file specified by filename.
func Parse(filename string, src interface{}, mode Mode) (f *File, err error) {
	in, err := newScanner(filename, src, mode&RetainComments != 0)
	if err != nil {
		return nil, err
	}
	p := parser{in: in}
	defer p.in.recover(&err)

	p.nextToken() // read first lookahead token
	f = p.parseFile()
	if f != nil {
		f.Path = filename
	}
	p.assignComments(f)
	return f, nil
}

// ParseCompoundStmt parses a single compound statement:
// a blank line, a def, for, while, or if statement, or a
// semicolon-separated list of simple statements followed
// by a newline. These are the units on which the REPL operates.
// ParseCompoundStmt does not consume any following input.
// The parser calls the readline function each
// time it needs a new line of input.
func ParseCompoundStmt(filename string, readline func() ([]byte, error)) (f *File, err error) {
	in, err := newScanner(filename, readline, false)
	if err != nil {
		return nil, err
	}

	p := parser{in: in}
	defer p.in.recover(&err)

	p.nextToken() // read first lookahead token

	var stmts []Stmt
	switch p.tok {
	case DEF, IF, FOR, WHILE:
		stmts = p.parseStmt(stmts)
	case NEWLINE:
		// blank line
	default:
		stmts = append(stmts, p.parseStmt2())
		// stmts = p.parseSimpleStmt(stmts, false)
		// Require but don't consume newline, to avoid blocking again.
		// if p.tok != SEMI {
		// p.in.errorf(p.in.pos, "invalid syntax")
		// }
	}

	return &File{Path: filename, Stmts: stmts}, nil
}

// ParseExpr parses a Starlark expression.
// A comma-separated list of expressions is parsed as a tuple.
// See Parse for explanation of parameters.
func ParseExpr(filename string, src interface{}, mode Mode) (expr Expr, err error) {
	in, err := newScanner(filename, src, mode&RetainComments != 0)
	if err != nil {
		return nil, err
	}
	p := parser{in: in}
	defer p.in.recover(&err)

	p.nextToken() // read first lookahead token

	// Use parseExpr, not parseTest, to permit an unparenthesized tuple.
	expr = p.parseExpr(false)

	// A following newline (e.g. "f()\n") appears outside any brackets,
	// on a non-blank line, and thus results in a NEWLINE token.
	if p.tok == SEMI {
		p.nextToken()
	}

	if p.tok != EOF {
		p.in.errorf(p.in.pos, "got %#v after expression, want EOF", p.tok)
	}
	p.assignComments(expr)
	return expr, nil
}

type parser struct {
	in     *scanner
	tok    Token
	tokval tokenValue
}

// nextToken advances the scanner and returns the position of the
// previous token.
func (p *parser) nextToken() Position {
	oldpos := p.tokval.pos
	p.tok = p.in.nextToken(&p.tokval)
	// enable to see the token stream
	if debug {
		log.Printf("nextToken: %-20s%+v\n", p.tok, p.tokval.pos)
	}
	return oldpos
}

func (p *parser) expectSemi() {
	if p.tok != RBRACE && p.tok != RPAREN {
		switch p.tok {
		case SEMI:
			p.nextToken()
		default:
			p.in.errorf(p.in.pos, "got %#v, expected SEMI", p.tok)
		}
	}
}

// file_input = (NEWLINE | stmt)* EOF
func (p *parser) parseFile() *File {
	var stmts []Stmt
	for p.tok != EOF {
		stmts = append(stmts, p.parseStmt2())
	}
	return &File{Stmts: stmts}
}

func (p *parser) parseStmt(stmts []Stmt) []Stmt {
	if p.tok == DEF {
		return append(stmts, p.parseDefStmt())
	} else if p.tok == IF {
		return append(stmts, p.parseIfStmt())
	} else if p.tok == FOR {
		return append(stmts, p.parseForStmt())
	} else if p.tok == WHILE {
		return append(stmts, p.parseWhileStmt())
	}
	return p.parseSimpleStmt(stmts, false)
}

func (p *parser) parseStmt2() Stmt {
	var stmt Stmt

	switch p.tok {
	case DEF:
		stmt = p.parseDefStmt()
	case IF:
		stmt = p.parseIfStmt()
	case FOR:
		stmt = p.parseForStmt()
	case WHILE:
		stmt = p.parseWhileStmt()
	case RETURN:
		pos := p.nextToken() // consume RETURN
		var result Expr
		if p.tok != EOF && p.tok != NEWLINE && p.tok != SEMI {
			result = p.parseExpr(false)
		}
		stmt = &ReturnStmt{Return: pos, Result: result}

	case BREAK, CONTINUE, PASS:
		tok := p.tok
		pos := p.nextToken() // consume it
		stmt = &BranchStmt{Token: tok, TokenPos: pos}

	case LOAD:
		stmt = p.parseLoadStmt()

	default:
		// Assignment
		x := p.parseExpr(false)
		switch p.tok {
		case EQ, PLUS_EQ, MINUS_EQ, STAR_EQ, SLASH_EQ, SLASHSLASH_EQ, PERCENT_EQ, AMP_EQ, PIPE_EQ, CIRCUMFLEX_EQ, LTLT_EQ, GTGT_EQ:
			op := p.tok
			pos := p.nextToken() // consume op
			rhs := p.parseExpr(false)
			stmt = &AssignStmt{OpPos: pos, Op: op, LHS: x, RHS: rhs}

		default:
			// Expression statement (e.g. function call, doc string).
			stmt = &ExprStmt{X: x}
		}
	}

	p.expectSemi()

	return stmt
}

func (p *parser) parseDefStmt() Stmt {
	defpos := p.nextToken() // consume DEF
	id := p.parseIdent()
	p.consume(LPAREN)
	params := p.parseParams()
	p.consume(RPAREN)
	body := p.parseSuite()
	return &DefStmt{
		Def:    defpos,
		Name:   id,
		Params: params,
		Body:   body,
	}
}

func (p *parser) parseIfStmt() Stmt {
	ec := exprContext{}
	ifpos := p.nextToken() // consume IF
	cond := p.parseTest(ec)
	body := p.parseSuite()
	ifStmt := &IfStmt{
		If:   ifpos,
		Cond: cond,
		True: body,
	}
	tail := ifStmt
	for p.tok == ELIF {
		elifpos := p.nextToken() // consume ELIF
		cond := p.parseTest(ec)
		body := p.parseSuite()
		elif := &IfStmt{
			If:   elifpos,
			Cond: cond,
			True: body,
		}
		tail.ElsePos = elifpos
		tail.False = []Stmt{elif}
		tail = elif
	}
	if p.tok == ELSE {
		tail.ElsePos = p.nextToken() // consume ELSE
		tail.False = p.parseSuite()
	}
	return ifStmt
}

func (p *parser) parseForStmt() Stmt {
	forpos := p.nextToken() // consume FOR
	vars := p.parseForLoopVariables()
	p.consume(IN)
	x := p.parseExpr(false)
	body := p.parseSuite()
	return &ForStmt{
		For:  forpos,
		Vars: vars,
		X:    x,
		Body: body,
	}
}

func (p *parser) parseWhileStmt() Stmt {
	ec := exprContext{}
	whilepos := p.nextToken() // consume WHILE
	cond := p.parseTest(ec)
	body := p.parseSuite()
	return &WhileStmt{
		While: whilepos,
		Cond:  cond,
		Body:  body,
	}
}

// Equivalent to 'exprlist' production in Python grammar.
//
// loop_variables = primary_with_suffix (COMMA primary_with_suffix)* COMMA?
func (p *parser) parseForLoopVariables() Expr {
	ec := exprContext{}
	// Avoid parseExpr because it would consume the IN token
	// following x in "for x in y: ...".
	v := p.parsePrimaryWithSuffix(ec)
	if p.tok != COMMA {
		return v
	}

	list := []Expr{v}
	for p.tok == COMMA {
		p.nextToken()
		if terminatesExprList(p.tok) {
			break
		}
		list = append(list, p.parsePrimaryWithSuffix(ec))
	}
	return &TupleExpr{List: list}
}

// simple_stmt = small_stmt (SEMI small_stmt)* SEMI? NEWLINE
// In REPL mode, it does not consume the NEWLINE.
func (p *parser) parseSimpleStmt(stmts []Stmt, consumeNL bool) []Stmt {
	for {
		stmts = append(stmts, p.parseSmallStmt())
		p.expectSemi()
		if p.tok != SEMI {
			break
		}
		p.nextToken() // consume SEMI
		if p.tok == RBRACE || p.tok == NEWLINE || p.tok == EOF {
			break
		}
	}
	// EOF without NEWLINE occurs in `if x: pass`, for example.
	if p.tok != EOF && consumeNL {
		p.consume(NEWLINE)
	}

	return stmts
}

// small_stmt = RETURN expr?
//            | PASS | BREAK | CONTINUE
//            | LOAD ...
//            | expr ('=' | '+=' | '-=' | '*=' | '/=' | '%=' | '&=' | '|=' | '^=' | '<<=' | '>>=') expr   // assign
//            | expr
func (p *parser) parseSmallStmt() Stmt {
	switch p.tok {
	case RETURN:
		pos := p.nextToken() // consume RETURN
		var result Expr
		if p.tok != EOF && p.tok != NEWLINE && p.tok != SEMI {
			result = p.parseExpr(false)
		}
		return &ReturnStmt{Return: pos, Result: result}

	case BREAK, CONTINUE, PASS:
		tok := p.tok
		pos := p.nextToken() // consume it
		return &BranchStmt{Token: tok, TokenPos: pos}

	case LOAD:
		return p.parseLoadStmt()
	}

	// Assignment
	x := p.parseExpr(false)
	switch p.tok {
	case EQ, PLUS_EQ, MINUS_EQ, STAR_EQ, SLASH_EQ, SLASHSLASH_EQ, PERCENT_EQ, AMP_EQ, PIPE_EQ, CIRCUMFLEX_EQ, LTLT_EQ, GTGT_EQ:
		op := p.tok
		pos := p.nextToken() // consume op
		rhs := p.parseExpr(false)
		return &AssignStmt{OpPos: pos, Op: op, LHS: x, RHS: rhs}
	}

	// Expression statement (e.g. function call, doc string).
	return &ExprStmt{X: x}
}

// stmt = LOAD '(' STRING {',' (IDENT '=')? STRING} [','] ')'
func (p *parser) parseLoadStmt() *LoadStmt {
	ec := exprContext{}
	loadPos := p.nextToken() // consume LOAD
	lparen := p.consume(LPAREN)

	if p.tok != STRING {
		p.in.errorf(p.in.pos, "first operand of load statement must be a string literal")
	}
	module := p.parsePrimary(ec).(*Literal)

	var from, to []*Ident
	for p.tok != RPAREN && p.tok != EOF {
		p.consume(COMMA)
		if p.tok == RPAREN {
			break // allow trailing comma
		}
		switch p.tok {
		case STRING:
			// load("module", "id")
			// To name is same as original.
			lit := p.parsePrimary(ec).(*Literal)
			id := &Ident{
				NamePos: lit.TokenPos.add(`"`),
				Name:    lit.Value.(string),
			}
			to = append(to, id)
			from = append(from, id)

		case IDENT:
			// load("module", to="from")
			id := p.parseIdent()
			to = append(to, id)
			if p.tok != EQ {
				p.in.errorf(p.in.pos, `load operand must be "%[1]s" or %[1]s="originalname" (want '=' after %[1]s)`, id.Name)
			}
			p.consume(EQ)
			if p.tok != STRING {
				p.in.errorf(p.in.pos, `original name of loaded symbol must be quoted: %s="originalname"`, id.Name)
			}
			lit := p.parsePrimary(ec).(*Literal)
			from = append(from, &Ident{
				NamePos: lit.TokenPos.add(`"`),
				Name:    lit.Value.(string),
			})

		case RPAREN:
			p.in.errorf(p.in.pos, "trailing comma in load statement")

		default:
			p.in.errorf(p.in.pos, `load operand must be "name" or localname="name" (got %#v)`, p.tok)
		}
	}
	rparen := p.consume(RPAREN)

	if len(to) == 0 {
		p.in.errorf(lparen, "load statement must import at least 1 symbol")
	}
	return &LoadStmt{
		Load:   loadPos,
		Module: module,
		To:     to,
		From:   from,
		Rparen: rparen,
	}
}

// suite is typically what follows a COLON (e.g. after DEF or FOR).
// suite = simple_stmt | NEWLINE INDENT stmt+ OUTDENT
func (p *parser) parseSuiteOld() []Stmt {
	p.consume(LBRACE)
	var stmts []Stmt
	for p.tok != RBRACE && p.tok != EOF {
		stmts = p.parseStmt(stmts)
		if p.tok == SEMI {
			p.consume(SEMI)
		}
	}
	p.consume(RBRACE)
	return stmts
}

func (p *parser) parseSuite() []Stmt {
	p.consume(LBRACE)
	var stmts []Stmt
	for p.tok != RBRACE && p.tok != EOF {
		stmts = append(stmts, p.parseStmt2())
	}
	p.consume(RBRACE)
	return stmts
}

func (p *parser) parseIdent() *Ident {
	if p.tok != IDENT {
		p.in.error(p.in.pos, "not an identifier")
	}
	id := &Ident{
		NamePos: p.tokval.pos,
		Name:    p.tokval.raw,
	}
	p.nextToken()
	return id
}

func (p *parser) consume(t Token) Position {
	if p.tok != t {
		p.in.errorf(p.in.pos, "got %#v, want %#v", p.tok, t)
	}
	return p.nextToken()
}

// params = (param COMMA)* param COMMA?
//        |
//
// param = IDENT
//       | IDENT EQ test
//       | STAR
//       | STAR IDENT
//       | STARSTAR IDENT
//
// parseParams parses a parameter list.  The resulting expressions are of the form:
//
//      *Ident                                          x
//      *Binary{Op: EQ, X: *Ident, Y: Expr}             x=y
//      *Unary{Op: STAR}                                *
//      *Unary{Op: STAR, X: *Ident}                     *args
//      *Unary{Op: STARSTAR, X: *Ident}                 **kwargs
func (p *parser) parseParams() []Expr {
	ec := exprContext{}
	var params []Expr
	for p.tok != RPAREN && p.tok != COLON && p.tok != EOF {
		if len(params) > 0 {
			p.consume(COMMA)
		}
		if p.tok == RPAREN {
			break
		}

		// * or *args or **kwargs
		if p.tok == STAR || p.tok == STARSTAR {
			op := p.tok
			pos := p.nextToken()
			var x Expr
			if op == STARSTAR || p.tok == IDENT {
				x = p.parseIdent()
			}
			params = append(params, &UnaryExpr{
				OpPos: pos,
				Op:    op,
				X:     x,
			})
			continue
		}

		// IDENT
		// IDENT = test
		id := p.parseIdent()
		if p.tok == EQ { // default value
			eq := p.nextToken()
			dflt := p.parseTest(ec)
			params = append(params, &BinaryExpr{
				X:     id,
				OpPos: eq,
				Op:    EQ,
				Y:     dflt,
			})
			continue
		}

		params = append(params, id)
	}
	return params
}

// parseExpr parses an expression, possible consisting of a
// comma-separated list of 'test' expressions.
//
// In many cases we must use parseTest to avoid ambiguity such as
// f(x, y) vs. f((x, y)).
func (p *parser) parseExpr(inParens bool) Expr {
	ec := exprContext{}
	x := p.parseTest(ec)
	if p.tok != COMMA {
		return x
	}

	// tuple
	exprs := p.parseExprs([]Expr{x}, exprContext{allowTrailingComma: inParens})
	return &TupleExpr{List: exprs}
}

type exprContext struct {
	allowTrailingComma bool
	allowParams        bool
}

// parseExprs parses a comma-separated list of expressions, starting with the comma.
// It is used to parse tuples and list elements.
// expr_list = (',' expr)* ','?
func (p *parser) parseExprs(exprs []Expr, ec exprContext) []Expr {
	for p.tok == COMMA {
		pos := p.nextToken()
		if terminatesExprList(p.tok) {
			if !ec.allowTrailingComma {
				p.in.error(pos, "unparenthesized tuple with trailing comma")
			}
			break
		}
		exprs = append(exprs, p.parseTest(ec))
	}
	return exprs
}

// parseTest parses a 'test', a single-component expression.
func (p *parser) parseTest(ec exprContext) Expr {
	if p.tok == LAMBDA {
		return p.parseLambda(ec, true)
	}

	x := p.parseTestPrec(ec, 0)

	// conditional expression (t IF cond ELSE f)
	if p.tok == IF {
		ifpos := p.nextToken()
		cond := p.parseTestPrec(ec, 0)
		if p.tok != ELSE {
			p.in.error(ifpos, "conditional expression without else clause")
		}
		elsepos := p.nextToken()
		else_ := p.parseTest(ec)
		return &CondExpr{If: ifpos, Cond: cond, True: x, ElsePos: elsepos, False: else_}
	}

	return x
}

// parseTestNoCond parses a a single-component expression without
// consuming a trailing 'if expr else expr'.
func (p *parser) parseTestNoCond(ec exprContext) Expr {
	if p.tok == LAMBDA {
		return p.parseLambda(ec, false)
	}
	return p.parseTestPrec(ec, 0)
}

// parseLambda parses a lambda expression.
// The allowCond flag allows the body to be an 'a if b else c' conditional.
func (p *parser) parseLambda(ec exprContext, allowCond bool) Expr {
	lambda := p.nextToken()
	var params []Expr
	if p.tok != COLON {
		params = p.parseParams()
	}
	p.consume(COLON)

	var body Expr
	if allowCond {
		body = p.parseTest(ec)
	} else {
		body = p.parseTestNoCond(ec)
	}

	return &LambdaExpr{
		Lambda: lambda,
		Params: params,
		Body:   body,
	}
}

func (p *parser) skipWS() {
	for p.tok == NEWLINE || p.tok == INDENT || p.tok == OUTDENT {
		p.nextToken()
	}
}

func (p *parser) parseTestPrec(ec exprContext, prec int) Expr {
	if prec >= len(preclevels) {
		return p.parsePrimaryWithSuffix(ec)
	}

	// expr = NOT expr
	if p.tok == NOT && prec == int(precedence[NOT]) {
		pos := p.nextToken()
		x := p.parseTestPrec(ec, prec)
		return &UnaryExpr{
			OpPos: pos,
			Op:    NOT,
			X:     x,
		}
	}

	return p.parseBinopExpr(ec, prec)
}

// expr = test (OP test)*
// Uses precedence climbing; see http://www.engr.mun.ca/~theo/Misc/exp_parsing.htm#climbing.
func (p *parser) parseBinopExpr(ec exprContext, prec int) Expr {
	x := p.parseTestPrec(ec, prec+1)
	for first := true; ; first = false {
		if p.tok == NOT {
			p.nextToken() // consume NOT
			// In this context, NOT must be followed by IN.
			// Replace NOT IN by a single NOT_IN token.
			if p.tok != IN {
				p.in.errorf(p.in.pos, "got %#v, want in", p.tok)
			}
			p.tok = NOT_IN
		}

		// Binary operator of specified precedence?
		opprec := int(precedence[p.tok])
		if opprec < prec {
			return x
		}

		// Comparisons are non-associative.
		if !first && opprec == int(precedence[EQL]) {
			p.in.errorf(p.in.pos, "%s does not associate with %s (use parens)",
				x.(*BinaryExpr).Op, p.tok)
		}

		op := p.tok
		pos := p.nextToken()
		y := p.parseTestPrec(ec, opprec+1)
		x = &BinaryExpr{OpPos: pos, Op: op, X: x, Y: y}
	}
}

// precedence maps each operator to its precedence (0-7), or -1 for other tokens.
var precedence [maxToken]int8

// preclevels groups operators of equal precedence.
// Comparisons are nonassociative; other binary operators associate to the left.
// Unary MINUS, unary PLUS, and TILDE have higher precedence so are handled in parsePrimary.
// See https://github.com/google/starlark-go/blob/master/doc/spec.md#binary-operators
var preclevels = [...][]Token{
	{OR},                                   // or
	{AND},                                  // and
	{NOT},                                  // not (unary)
	{EQL, NEQ, LT, GT, LE, GE, IN, NOT_IN}, // == != < > <= >= in not in
	{PIPE},                                 // |
	{CIRCUMFLEX},                           // ^
	{AMP},                                  // &
	{LTLT, GTGT},                           // << >>
	{MINUS, PLUS},                          // -
	{STAR, PERCENT, SLASH, SLASHSLASH},     // * % / //
}

func init() {
	// populate precedence table
	for i := range precedence {
		precedence[i] = -1
	}
	for level, tokens := range preclevels {
		for _, tok := range tokens {
			precedence[tok] = int8(level)
		}
	}
}

// primary_with_suffix = primary
//                     | primary '.' IDENT
//                     | primary slice_suffix
//                     | primary call_suffix
func (p *parser) parsePrimaryWithSuffix(ec exprContext) Expr {
	x := p.parsePrimary(ec)
	for {
		switch p.tok {
		case DOT:
			dot := p.nextToken()
			id := p.parseIdent()
			x = &DotExpr{Dot: dot, X: x, Name: id}
		case LBRACK:
			x = p.parseSliceSuffix(ec, x)
		case LPAREN:
			x = p.parseCallSuffix(ec, x)
		default:
			return x
		}
	}
}

// slice_suffix = '[' expr? ':' expr?  ':' expr? ']'
func (p *parser) parseSliceSuffix(ec exprContext, x Expr) Expr {
	lbrack := p.nextToken()
	var lo, hi, step Expr
	if p.tok != COLON {
		y := p.parseExpr(false)

		// index x[y]
		if p.tok == RBRACK {
			rbrack := p.nextToken()
			return &IndexExpr{X: x, Lbrack: lbrack, Y: y, Rbrack: rbrack}
		}

		lo = y
	}

	// slice or substring x[lo:hi:step]
	if p.tok == COLON {
		p.nextToken()
		if p.tok != COLON && p.tok != RBRACK {
			hi = p.parseTest(ec)
		}
	}
	if p.tok == COLON {
		p.nextToken()
		if p.tok != RBRACK {
			step = p.parseTest(ec)
		}
	}
	rbrack := p.consume(RBRACK)
	return &SliceExpr{X: x, Lbrack: lbrack, Lo: lo, Hi: hi, Step: step, Rbrack: rbrack}
}

// call_suffix = '(' arg_list? ')'
func (p *parser) parseCallSuffix(ec exprContext, fn Expr) Expr {
	lparen := p.consume(LPAREN)
	var rparen Position
	var args []Expr

	if p.tok == RPAREN {
		rparen = p.nextToken()
	} else {
		args = p.parseArgs(ec)
		rparen = p.consume(RPAREN)
	}
	return &CallExpr{Fn: fn, Lparen: lparen, Args: args, Rparen: rparen}
}

// parseArgs parses a list of actual parameter values (arguments).
// It mirrors the structure of parseParams.
// arg_list = ((arg COMMA)* arg COMMA?)?
func (p *parser) parseArgs(ec exprContext) []Expr {
	var args []Expr
	for p.tok != RPAREN && p.tok != EOF {
		if len(args) > 0 {
			p.consume(COMMA)
		}
		if p.tok == RPAREN {
			break
		}

		// *args or **kwargs
		if p.tok == STAR || p.tok == STARSTAR {
			op := p.tok
			pos := p.nextToken()
			x := p.parseTest(ec)
			args = append(args, &UnaryExpr{
				OpPos: pos,
				Op:    op,
				X:     x,
			})
			continue
		}

		// We use a different strategy from Bazel here to stay within LL(1).
		// Instead of looking ahead two tokens (IDENT, EQ) we parse
		// 'test = test' then check that the first was an IDENT.
		x := p.parseTest(ec)

		if p.tok == EQ {
			// name = value
			if _, ok := x.(*Ident); !ok {
				p.in.errorf(p.in.pos, "keyword argument must have form name=expr")
			}
			eq := p.nextToken()
			y := p.parseTest(ec)
			x = &BinaryExpr{
				X:     x,
				OpPos: eq,
				Op:    EQ,
				Y:     y,
			}
		}

		if p.tok == SEMI {
			p.nextToken()
		}

		args = append(args, x)
	}
	return args
}

//  primary = IDENT
//          | INT | FLOAT
//          | STRING
//          | '[' ...                    // list literal or comprehension
//          | '{' ...                    // dict literal or comprehension
//          | '(' ...                    // tuple or parenthesized expression
//          | ('-'|'+'|'~') primary_with_suffix
func (p *parser) parsePrimary(ec exprContext) Expr {
	p.skipWS()

	switch p.tok {
	case IDENT:
		id := p.parseIdent()

		if p.tok == ARROW {
			pos := p.nextToken()
			return p.parseArrow(pos, []Expr{id})
		}

		return id
	case INT, FLOAT, STRING:
		var val interface{}
		tok := p.tok
		switch tok {
		case INT:
			if p.tokval.bigInt != nil {
				val = p.tokval.bigInt
			} else {
				val = p.tokval.int
			}
		case FLOAT:
			val = p.tokval.float
		case STRING:
			val = p.tokval.string
		}
		raw := p.tokval.raw
		pos := p.nextToken()
		return &Literal{Token: tok, TokenPos: pos, Raw: raw, Value: val}

	case LBRACK:
		return p.parseList()

	case LBRACE:
		return p.parseDict(ec)

	case STARSTAR:
		if !ec.allowParams {
			p.in.errorf(p.in.pos, "got %#v, want primary expression", p.tok)
		}

		op := p.tok
		pos := p.nextToken()
		x := p.parseIdent()

		return &UnaryExpr{
			OpPos: pos,
			Op:    op,
			X:     x,
		}
	case STAR:
		if !ec.allowParams {
			p.in.errorf(p.in.pos, "got %#v, want primary expression", p.tok)
		}

		op := p.tok
		pos := p.nextToken()
		x := p.parseIdent()

		return &UnaryExpr{
			OpPos: pos,
			Op:    op,
			X:     x,
		}

	case LPAREN:
		lparen := p.nextToken()
		if p.tok == RPAREN {
			// empty tuple
			rparen := p.nextToken()

			if p.tok == ARROW {
				pos := p.nextToken()
				return p.parseArrow(pos, nil)
			}
			return &TupleExpr{Lparen: lparen, Rparen: rparen}
		}
		// e := p.parseExpr(true) // allow trailing comma
		ec := exprContext{
			allowParams:        true,
			allowTrailingComma: true,
		}

		e := p.parseTest(ec)
		exprs := []Expr{e}

		if p.tok == COMMA {
			// tuple
			exprs = p.parseExprs(exprs, ec)
			e = &TupleExpr{List: exprs}
		}

		rparen := p.consume(RPAREN)

		if p.tok == ARROW {
			pos := p.nextToken()

			return p.parseArrow(pos, exprs)
		} else {
			return &ParenExpr{
				Lparen: lparen,
				X:      e,
				Rparen: rparen,
			}
		}

	case ARROW:
		pos := p.nextToken()
		return p.parseArrow(pos, nil)

	case MINUS, PLUS, TILDE: // unary
		tok := p.tok
		pos := p.nextToken()
		x := p.parsePrimaryWithSuffix(ec)
		return &UnaryExpr{
			OpPos: pos,
			Op:    tok,
			X:     x,
		}
	}
	p.in.errorf(p.in.pos, "got %#v, want primary expression", p.tok)
	panic("unreachable")
}

func (p *parser) parseArrowSuite() []Stmt {
	if p.tok == LBRACE {
		p.nextToken() // consume LBRACE
		// p.consume(INDENT)
		var stmts []Stmt
		for p.tok != RBRACE && p.tok != EOF {
			stmts = append(stmts, p.parseStmt2())
		}
		p.consume(RBRACE)
		return stmts
	}

	body := p.parseTest(exprContext{})
	return []Stmt{&ExprStmt{X: body}}
}

func (p *parser) parseArrowStmt(stmts []Stmt) []Stmt {
	if p.tok == DEF {
		return append(stmts, p.parseDefStmt())
	} else if p.tok == IF {
		return append(stmts, p.parseIfStmt())
	} else if p.tok == FOR {
		return append(stmts, p.parseForStmt())
	} else if p.tok == WHILE {
		return append(stmts, p.parseWhileStmt())
	}

	return p.parseSimpleStmt(stmts, false)
}

func (p *parser) parseArrow(pos Position, exprArgs []Expr) Expr {
	body := p.parseArrowSuite()

	// Add a return to the end. If the final statement is also
	// an expression, then return it's value. Otherwise return None.

	last := len(body) - 1

	start, _ := body[last].Span()
	if expr, ok := body[last].(*ExprStmt); ok {
		body[last] = &ReturnStmt{Return: start, Result: expr.X}
	} else {
		body = append(body, &ReturnStmt{Return: start})
	}

	return &LambdaExpr{
		Lambda: pos,
		Params: exprArgs,
		Stmts:  body,
	}
}

// list = '[' ']'
//      | '[' expr ']'
//      | '[' expr expr_list ']'
//      | '[' expr (FOR loop_variables IN expr)+ ']'
func (p *parser) parseList() Expr {
	ec := exprContext{}
	lbrack := p.nextToken()
	if p.tok == RBRACK {
		// empty List
		rbrack := p.nextToken()
		return &ListExpr{Lbrack: lbrack, Rbrack: rbrack}
	}

	x := p.parseTest(ec)

	if p.tok == FOR {
		// list comprehension
		return p.parseComprehensionSuffix(lbrack, x, RBRACK)
	}

	exprs := []Expr{x}
	if p.tok == COMMA {
		// multi-item list literal
		ec := exprContext{
			allowTrailingComma: true,
		}
		exprs = p.parseExprs(exprs, ec) // allow trailing comma
	}

	rbrack := p.consume(RBRACK)
	return &ListExpr{Lbrack: lbrack, List: exprs, Rbrack: rbrack}
}

// dict = '{' '}'
//      | '{' dict_entry_list '}'
//      | '{' dict_entry FOR loop_variables IN expr '}'
func (p *parser) parseDict(ec exprContext) Expr {
	lbrace := p.nextToken()
	if p.tok == RBRACE {
		// empty dict
		rbrace := p.nextToken()
		return &DictExpr{Lbrace: lbrace, Rbrace: rbrace}
	}

	x := p.parseDictEntry(ec)

	if p.tok == FOR {
		// dict comprehension
		return p.parseComprehensionSuffix(lbrace, x, RBRACE)
	}

	entries := []Expr{x}
	for p.tok == COMMA {
		p.nextToken()
		if p.tok == RBRACE {
			break
		}
		entries = append(entries, p.parseDictEntry(ec))
	}

	rbrace := p.consume(RBRACE)
	return &DictExpr{Lbrace: lbrace, List: entries, Rbrace: rbrace}
}

// dict_entry = test ':' test
func (p *parser) parseDictEntry(ec exprContext) *DictEntry {
	k := p.parseTest(ec)
	colon := p.consume(COLON)
	v := p.parseTest(ec)
	return &DictEntry{Key: k, Colon: colon, Value: v}
}

// comp_suffix = FOR loopvars IN expr comp_suffix
//             | IF expr comp_suffix
//             | ']'  or  ')'                              (end)
//
// There can be multiple FOR/IF clauses; the first is always a FOR.
func (p *parser) parseComprehensionSuffix(lbrace Position, body Expr, endBrace Token) Expr {
	ec := exprContext{}
	var clauses []Node
	for p.tok != endBrace {
		if p.tok == FOR {
			pos := p.nextToken()
			vars := p.parseForLoopVariables()
			in := p.consume(IN)
			// Following Python 3, the operand of IN cannot be:
			// - a conditional expression ('x if y else z'),
			//   due to conflicts in Python grammar
			//  ('if' is used by the comprehension);
			// - a lambda expression
			// - an unparenthesized tuple.
			x := p.parseTestPrec(ec, 0)
			clauses = append(clauses, &ForClause{For: pos, Vars: vars, In: in, X: x})
		} else if p.tok == IF {
			pos := p.nextToken()
			cond := p.parseTestNoCond(ec)
			clauses = append(clauses, &IfClause{If: pos, Cond: cond})
		} else if p.tok == SEMI {
			// indicates a semi before a final brace
			p.nextToken()
			break
		} else {
			p.in.errorf(p.in.pos, "got %#v, want '%s', for, or if", p.tok, endBrace)
		}
	}
	rbrace := p.nextToken()

	return &Comprehension{
		Curly:   endBrace == RBRACE,
		Lbrack:  lbrace,
		Body:    body,
		Clauses: clauses,
		Rbrack:  rbrace,
	}
}

func terminatesExprList(tok Token) bool {
	switch tok {
	case EOF, NEWLINE, EQ, RBRACE, RBRACK, RPAREN, SEMI:
		return true
	}
	return false
}

// Comment assignment.
// We build two lists of all subnodes, preorder and postorder.
// The preorder list is ordered by start location, with outer nodes first.
// The postorder list is ordered by end location, with outer nodes last.
// We use the preorder list to assign each whole-line comment to the syntax
// immediately following it, and we use the postorder list to assign each
// end-of-line comment to the syntax immediately preceding it.

// flattenAST returns the list of AST nodes, both in prefix order and in postfix
// order.
func flattenAST(root Node) (pre, post []Node) {
	stack := []Node{}
	Walk(root, func(n Node) bool {
		if n != nil {
			pre = append(pre, n)
			stack = append(stack, n)
		} else {
			post = append(post, stack[len(stack)-1])
			stack = stack[:len(stack)-1]
		}
		return true
	})
	return pre, post
}

// assignComments attaches comments to nearby syntax.
func (p *parser) assignComments(n Node) {
	// Leave early if there are no comments
	if len(p.in.lineComments)+len(p.in.suffixComments) == 0 {
		return
	}

	pre, post := flattenAST(n)

	// Assign line comments to syntax immediately following.
	line := p.in.lineComments
	for _, x := range pre {
		start, _ := x.Span()

		switch x.(type) {
		case *File:
			continue
		}

		for len(line) > 0 && !start.isBefore(line[0].Start) {
			x.AllocComments()
			x.Comments().Before = append(x.Comments().Before, line[0])
			line = line[1:]
		}
	}

	// Remaining line comments go at end of file.
	if len(line) > 0 {
		n.AllocComments()
		n.Comments().After = append(n.Comments().After, line...)
	}

	// Assign suffix comments to syntax immediately before.
	suffix := p.in.suffixComments
	for i := len(post) - 1; i >= 0; i-- {
		x := post[i]

		// Do not assign suffix comments to file
		switch x.(type) {
		case *File:
			continue
		}

		_, end := x.Span()
		if len(suffix) > 0 && end.isBefore(suffix[len(suffix)-1].Start) {
			x.AllocComments()
			x.Comments().Suffix = append(x.Comments().Suffix, suffix[len(suffix)-1])
			suffix = suffix[:len(suffix)-1]
		}
	}
}
