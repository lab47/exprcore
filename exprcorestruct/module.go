package exprcorestruct

import (
	"fmt"

	"github.com/lab47/exprcore/exprcore"
)

// A Module is a named collection of values,
// typically a suite of functions imported by a load statement.
//
// It differs from Struct primarily in that its string representation
// does not enumerate its fields.
type Module struct {
	Name    string
	Members exprcore.StringDict
}

var _ exprcore.HasAttrs = (*Module)(nil)

func (m *Module) Attr(name string) (exprcore.Value, error) { return m.Members[name], nil }
func (m *Module) AttrNames() []string                      { return m.Members.Keys() }
func (m *Module) Freeze()                                  { m.Members.Freeze() }
func (m *Module) Hash() (uint32, error)                    { return 0, fmt.Errorf("unhashable: %s", m.Type()) }
func (m *Module) String() string                           { return fmt.Sprintf("<module %q>", m.Name) }
func (m *Module) Truth() exprcore.Bool                     { return true }
func (m *Module) Type() string                             { return "module" }

// MakeModule may be used as the implementation of a exprcore built-in
// function, module(name, **kwargs). It returns a new module with the
// specified name and members.
func MakeModule(thread *exprcore.Thread, b *exprcore.Builtin, args exprcore.Tuple, kwargs []exprcore.Tuple) (exprcore.Value, error) {
	var name string
	if err := exprcore.UnpackPositionalArgs(b.Name(), args, nil, 1, &name); err != nil {
		return nil, err
	}
	members := make(exprcore.StringDict, len(kwargs))
	for _, kwarg := range kwargs {
		k := string(kwarg[0].(exprcore.String))
		members[k] = kwarg[1]
	}
	return &Module{name, members}, nil
}
