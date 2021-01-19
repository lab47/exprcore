package exprcore

func (fn *Function) HashCode() ([]byte, error) {
	return fn.funcode.Signature, nil
}
