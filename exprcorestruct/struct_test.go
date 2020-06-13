// Copyright 2018 The Bazel Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package exprcorestruct_test

import (
	"fmt"
	"path/filepath"
	"testing"

	"github.com/lab47/exprcore/exprcore"
	"github.com/lab47/exprcore/exprcorestruct"
	"github.com/lab47/exprcore/exprcoretest"
	"github.com/lab47/exprcore/resolve"
)

func init() {
	// The tests make extensive use of these not-yet-standard features.
	resolve.AllowLambda = true
	resolve.AllowNestedDef = true
	resolve.AllowFloat = true
	resolve.AllowSet = true
}

func Test(t *testing.T) {
	testdata := exprcoretest.DataFile("exprcorestruct", ".")
	thread := &exprcore.Thread{Load: load}
	exprcoretest.SetReporter(thread, t)
	filename := filepath.Join(testdata, "testdata/struct.star")
	predeclared := exprcore.StringDict{
		"struct": exprcore.NewBuiltin("struct", exprcorestruct.Make),
		"gensym": exprcore.NewBuiltin("gensym", gensym),
	}
	if _, err := exprcore.ExecFile(thread, filename, nil, predeclared); err != nil {
		if err, ok := err.(*exprcore.EvalError); ok {
			t.Fatal(err.Backtrace())
		}
		t.Fatal(err)
	}
}

// load implements the 'load' operation as used in the evaluator tests.
func load(thread *exprcore.Thread, module string) (exprcore.StringDict, error) {
	if module == "assert.star" {
		return exprcoretest.LoadAssertModule()
	}
	return nil, fmt.Errorf("load not implemented")
}

// gensym is a built-in function that generates a unique symbol.
func gensym(thread *exprcore.Thread, _ *exprcore.Builtin, args exprcore.Tuple, kwargs []exprcore.Tuple) (exprcore.Value, error) {
	var name string
	if err := exprcore.UnpackArgs("gensym", args, kwargs, "name", &name); err != nil {
		return nil, err
	}
	return &symbol{name: name}, nil
}

// A symbol is a distinct value that acts as a constructor of "branded"
// struct instances, like a class symbol in Python or a "provider" in Bazel.
type symbol struct{ name string }

var _ exprcore.Callable = (*symbol)(nil)

func (sym *symbol) Name() string          { return sym.name }
func (sym *symbol) String() string        { return sym.name }
func (sym *symbol) Type() string          { return "symbol" }
func (sym *symbol) Freeze()               {} // immutable
func (sym *symbol) Truth() exprcore.Bool  { return exprcore.True }
func (sym *symbol) Hash() (uint32, error) { return 0, fmt.Errorf("unhashable: %s", sym.Type()) }

func (sym *symbol) CallInternal(thread *exprcore.Thread, args exprcore.Tuple, kwargs []exprcore.Tuple) (exprcore.Value, error) {
	if len(args) > 0 {
		return nil, fmt.Errorf("%s: unexpected positional arguments", sym)
	}
	return exprcorestruct.FromKeywords(sym, kwargs), nil
}
