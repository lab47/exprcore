package exprcore

import (
	"fmt"

	"github.com/lab47/exprcore/internal/compile"
	"golang.org/x/crypto/blake2b"
)

func hashFuncode(f *compile.Funcode) ([]byte, error) {
	h, _ := blake2b.New256(nil)

	fmt.Fprintf(h, "names")
	for _, n := range f.Prog.Names {
		fmt.Fprintf(h, "%s\n", n)
	}

	fmt.Fprintf(h, "constants")
	for _, n := range f.Prog.Constants {
		fmt.Fprintf(h, "%s\n", n)
	}

	for _, subidx := range f.SubFunctions {
		sub := f.Prog.Functions[subidx]

		sh, err := hashFuncode(sub)
		if err != nil {
			return nil, err
		}
		h.Write(sh)
	}

	fmt.Fprintf(h, "code")
	h.Write(f.Code)

	fmt.Fprintf(h, "cells")
	for _, cell := range f.Cells {
		fmt.Fprintf(h, "%d", cell)
	}

	fmt.Fprintf(h, "\nargs %d/%d/%d/%v/%v\n",
		f.MaxStack, f.NumParams, f.NumKwonlyParams, f.HasKwargs, f.HasVarargs)

	return h.Sum(nil), nil
}

func (fn *Function) HashCode() ([]byte, error) {
	return hashFuncode(fn.funcode)
}
