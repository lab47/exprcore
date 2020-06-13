// Copyright 2017 The Bazel Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Package exprcoretest defines utilities for testing exprcore programs.
//
// Clients can call LoadAssertModule to load a module that defines
// several functions useful for testing.  See assert.star for its
// definition.
//
// The assert.error function, which reports errors to the current Go
// testing.T, requires that clients call SetTest(thread, t) before use.
package exprcoretest // import "github.com/lab47/exprcore/exprcoretest"

import (
	"fmt"
	"go/build"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"sync"

	"github.com/lab47/exprcore/exprcore"
	"github.com/lab47/exprcore/exprcorestruct"
)

const localKey = "Reporter"

// A Reporter is a value to which errors may be reported.
// It is satisfied by *testing.T.
type Reporter interface {
	Error(args ...interface{})
}

// SetReporter associates an error reporter (such as a testing.T in
// a Go test) with the exprcore thread so that exprcore programs may
// report errors to it.
func SetReporter(thread *exprcore.Thread, r Reporter) {
	thread.SetLocal(localKey, r)
}

// GetReporter returns the exprcore thread's error reporter.
// It must be preceded by a call to SetReporter.
func GetReporter(thread *exprcore.Thread) Reporter {
	r, ok := thread.Local(localKey).(Reporter)
	if !ok {
		panic("internal error: exprcoretest.SetReporter was not called")
	}
	return r
}

var (
	once      sync.Once
	assert    exprcore.StringDict
	assertErr error
)

// LoadAssertModule loads the assert module.
// It is concurrency-safe and idempotent.
func LoadAssertModule() (exprcore.StringDict, error) {
	once.Do(func() {
		predeclared := exprcore.StringDict{
			"error":   exprcore.NewBuiltin("error", error_),
			"catch":   exprcore.NewBuiltin("catch", catch),
			"matches": exprcore.NewBuiltin("matches", matches),
			"module":  exprcore.NewBuiltin("module", exprcorestruct.MakeModule),
			"_freeze": exprcore.NewBuiltin("freeze", freeze),
		}
		filename := DataFile("exprcoretest", "assert.star")
		thread := new(exprcore.Thread)
		assert, assertErr = exprcore.ExecFile(thread, filename, nil, predeclared)
	})
	return assert, assertErr
}

// catch(f) evaluates f() and returns its evaluation error message
// if it failed or None if it succeeded.
func catch(thread *exprcore.Thread, _ *exprcore.Builtin, args exprcore.Tuple, kwargs []exprcore.Tuple) (exprcore.Value, error) {
	var fn exprcore.Callable
	if err := exprcore.UnpackArgs("catch", args, kwargs, "fn", &fn); err != nil {
		return nil, err
	}
	if _, err := exprcore.Call(thread, fn, nil, nil); err != nil {
		return exprcore.String(err.Error()), nil
	}
	return exprcore.None, nil
}

// matches(pattern, str) reports whether string str matches the regular expression pattern.
func matches(thread *exprcore.Thread, _ *exprcore.Builtin, args exprcore.Tuple, kwargs []exprcore.Tuple) (exprcore.Value, error) {
	var pattern, str string
	if err := exprcore.UnpackArgs("matches", args, kwargs, "pattern", &pattern, "str", &str); err != nil {
		return nil, err
	}
	ok, err := regexp.MatchString(pattern, str)
	if err != nil {
		return nil, fmt.Errorf("matches: %s", err)
	}
	return exprcore.Bool(ok), nil
}

// error(x) reports an error to the Go test framework.
func error_(thread *exprcore.Thread, _ *exprcore.Builtin, args exprcore.Tuple, kwargs []exprcore.Tuple) (exprcore.Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("error: got %d arguments, want 1", len(args))
	}
	buf := new(strings.Builder)
	stk := thread.CallStack()
	stk.Pop()
	fmt.Fprintf(buf, "%sError: ", stk)
	if s, ok := exprcore.AsString(args[0]); ok {
		buf.WriteString(s)
	} else {
		buf.WriteString(args[0].String())
	}
	GetReporter(thread).Error(buf.String())
	return exprcore.None, nil
}

// freeze(x) freezes its operand.
func freeze(thread *exprcore.Thread, _ *exprcore.Builtin, args exprcore.Tuple, kwargs []exprcore.Tuple) (exprcore.Value, error) {
	if len(kwargs) > 0 {
		return nil, fmt.Errorf("freeze does not accept keyword arguments")
	}
	if len(args) != 1 {
		return nil, fmt.Errorf("freeze got %d arguments, wants 1", len(args))
	}
	args[0].Freeze()
	return args[0], nil
}

// DataFile returns the effective filename of the specified
// test data resource.  The function abstracts differences between
// 'go build', under which a test runs in its package directory,
// and Blaze, under which a test runs in the root of the tree.
var DataFile = func(pkgdir, filename string) string {
	return filepath.Join("..", pkgdir, filename)

	// Check if we're being run by Bazel and change directories if so.
	// TEST_SRCDIR and TEST_WORKSPACE are set by the Bazel test runner, so that makes a decent check
	testSrcdir := os.Getenv("TEST_SRCDIR")
	testWorkspace := os.Getenv("TEST_WORKSPACE")
	if testSrcdir != "" && testWorkspace != "" {
		return filepath.Join(testSrcdir, "net_exprcore_go", pkgdir, filename)
	}

	return filepath.Join(build.Default.GOPATH, "src/github.com/lab47/exprcore", pkgdir, filename)
}
