// Copyright 2018 The Bazel Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package exprcore_test

import (
	"bytes"
	"io/ioutil"
	"path/filepath"
	"strings"
	"testing"

	"github.com/lab47/exprcore/exprcore"
	"github.com/lab47/exprcore/exprcoretest"
)

func Benchmark(b *testing.B) {
	defer setOptions("")

	testdata := exprcoretest.DataFile("exprcore", ".")
	thread := new(exprcore.Thread)
	for _, file := range []string{
		"testdata/benchmark.star",
		// ...
	} {

		filename := filepath.Join(testdata, file)

		src, err := ioutil.ReadFile(filename)
		if err != nil {
			b.Error(err)
			continue
		}
		setOptions(string(src))

		// Evaluate the file once.
		globals, err := exprcore.ExecFile(thread, filename, src, nil)
		if err != nil {
			reportEvalError(b, err)
		}

		// Repeatedly call each global function named bench_* as a benchmark.
		for _, name := range globals.Keys() {
			value := globals[name]
			if fn, ok := value.(*exprcore.Function); ok && strings.HasPrefix(name, "bench_") {
				b.Run(name, func(b *testing.B) {
					for i := 0; i < b.N; i++ {
						_, err := exprcore.Call(thread, fn, nil, nil)
						if err != nil {
							reportEvalError(b, err)
						}
					}
				})
			}
		}
	}
}

// BenchmarkProgram measures operations relevant to compiled programs.
// TODO(adonovan): use a bigger testdata program.
func BenchmarkProgram(b *testing.B) {
	// Measure time to read a source file (approx 600us but depends on hardware and file system).
	filename := exprcoretest.DataFile("exprcore", "testdata/paths.star")
	var src []byte
	b.Run("read", func(b *testing.B) {
		for i := 0; i < b.N; i++ {
			var err error
			src, err = ioutil.ReadFile(filename)
			if err != nil {
				b.Fatal(err)
			}
		}
	})

	// Measure time to turn a source filename into a compiled program (approx 450us).
	var prog *exprcore.Program
	b.Run("compile", func(b *testing.B) {
		for i := 0; i < b.N; i++ {
			var err error
			_, prog, err = exprcore.SourceProgram(filename, src, exprcore.StringDict(nil).Has)
			if err != nil {
				b.Fatal(err)
			}
		}
	})

	// Measure time to encode a compiled program to a memory buffer
	// (approx 20us; was 75-120us with gob encoding).
	var out bytes.Buffer
	b.Run("encode", func(b *testing.B) {
		for i := 0; i < b.N; i++ {
			out.Reset()
			if err := prog.Write(&out); err != nil {
				b.Fatal(err)
			}
		}
	})

	// Measure time to decode a compiled program from a memory buffer
	// (approx 20us; was 135-250us with gob encoding)
	b.Run("decode", func(b *testing.B) {
		for i := 0; i < b.N; i++ {
			in := bytes.NewReader(out.Bytes())
			if _, err := exprcore.CompiledProgram(in); err != nil {
				b.Fatal(err)
			}
		}
	})
}
