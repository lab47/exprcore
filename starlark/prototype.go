package starlark

import (
	"errors"
	"fmt"
	"sort"
	"strings"

	"go.starlark.net/syntax"
)

// Make is the implementation of a built-in function that instantiates
// an immutable struct from the specified keyword arguments.
//
// An application can add 'struct' to the Starlark environment like so:
//
// 	globals := starlark.StringDict{
// 		"struct":  starlark.NewBuiltin("struct", starlarkstruct.Make),
// 	}
//
func Make(_ *Thread, _ *Builtin, args Tuple, kwargs []Tuple) (Value, error) {
	if len(args) > 0 {
		return nil, fmt.Errorf("struct: unexpected positional arguments")
	}
	return FromKeywords(Root, kwargs), nil
}

// FromKeywords returns a new struct instance whose fields are specified by the
// key/value pairs in kwargs.  (Each kwargs[i][0] must be a String.)
func FromKeywords(p *Prototype, kwargs []Tuple) *Prototype {
	s := &Prototype{
		constructor: p.constructor,
		parents:     []parent{{"parent", p}},
		entries:     make(entries, 0, len(kwargs)),
	}
	for _, kwarg := range kwargs {
		k := string(kwarg[0].(String))
		v := kwarg[1]
		s.entries = append(s.entries, protoSlot{k, v})
	}
	sort.Sort(s.entries)
	return s
}

// FromStringDict returns a whose elements are those of d.
// The constructor parameter specifies the constructor; use Default for an ordinary struct.
func FromStringDict(p *Prototype, d StringDict) *Prototype {
	s := &Prototype{
		constructor: p.constructor,
		parents:     []parent{{"parent", p}},
		entries:     make(entries, 0, len(d)),
	}
	for k, v := range d {
		s.entries = append(s.entries, protoSlot{k, v})
	}
	sort.Sort(s.entries)
	return s
}

func FromBuiltins(p *Prototype, m map[string]*Builtin) *Prototype {
	s := &Prototype{
		constructor: p.constructor,
		parents:     []parent{{"parent", p}},
		entries:     make(entries, 0, len(m)),
	}
	for k, v := range m {
		s.entries = append(s.entries, protoSlot{k, v})
	}
	sort.Sort(s.entries)
	return s
}

type parent struct {
	name  string
	value *Prototype
}

// Prototype is an immutable Starlark type that maps field names to values.
// It is not iterable and does not support len.
//
// A struct has a constructor, a distinct value that identifies a class
// of structs, and which appears in the struct's string representation.
//
// Operations such as x+y fail if the constructors of the two operands
// are not equal.
//
// The default constructor, Default, is the string "struct", but
// clients may wish to 'brand' structs for their own purposes.
// The constructor value appears in the printed form of the value,
// and is accessible using the Constructor method.
//
// Use Attr to access its fields and AttrNames to enumerate them.
type Prototype struct {
	name        string
	frozen      bool
	constructor Value
	parents     []parent
	entries     entries // sorted by name
}

// Default is the default constructor for structs.
// It is merely the string "struct".
const Default = String("prototype")

var Root = &Prototype{
	constructor: Default,
	frozen:      true,
}

type entries []protoSlot

func (a entries) Len() int           { return len(a) }
func (a entries) Less(i, j int) bool { return a[i].name < a[j].name }
func (a entries) Swap(i, j int)      { a[i], a[j] = a[j], a[i] }

type protoSlot struct {
	name  string
	value Value
}

var (
	_ HasAttrs  = (*Prototype)(nil)
	_ HasBinary = (*Prototype)(nil)
	_ Callable  = (*Prototype)(nil)
)

func (s *Prototype) setParent(name string, p *Prototype) {
	s.parents = []parent{
		{
			name:  name,
			value: p,
		},
	}
}

func (s *Prototype) Name() string {
	return s.name
}

func (s *Prototype) CallInternal(thread *Thread, args Tuple, kwargs []Tuple) (Value, error) {
	return FromKeywords(s, kwargs), nil
}

var StopChain = errors.New("stop chain")

func (s *Prototype) AlongChain(f func(p *Prototype) error) error {
	seen := map[*Prototype]struct{}{}

	err := s.alongChainChecked(f, seen)
	if err != nil {
		if err == StopChain {
			return nil
		}

		return err
	}

	return nil
}

func (s *Prototype) alongChainChecked(f func(p *Prototype) error, seen map[*Prototype]struct{}) error {
	if _, ok := seen[s]; ok {
		return nil
	}

	err := f(s)
	if err != nil {
		return err
	}

	seen[s] = struct{}{}

	for _, parent := range s.parents {
		err = parent.value.alongChainChecked(f, seen)
		if err != nil {
			return err
		}
	}

	return nil
}

func (s *Prototype) forEach(f func(k string, v Value) error) error {
	s.AlongChain(func(p *Prototype) error {
		for _, e := range p.entries {
			err := f(e.name, e.value)
			if err != nil {
				return err
			}
		}

		return nil
	})

	return nil
}

// ToStringDict adds a name/value entry to d for each field of the struct.
func (s *Prototype) ToStringDict(d StringDict) {
	s.forEach(func(k string, v Value) error {
		if _, found := d[k]; !found {
			d[k] = v
		}
		return nil
	})
}

func (s *Prototype) String() string {
	buf := new(strings.Builder)
	if s.constructor == Default {
		// NB: The Java implementation always prints struct
		// even for Bazel provider instances.
		buf.WriteString("prototype") // avoid String()'s quotation
	} else {
		buf.WriteString(s.constructor.String())
	}
	buf.WriteByte('(')
	var rest bool

	s.forEach(func(k string, v Value) error {
		if !rest {
			buf.WriteString(", ")
			rest = true
		}
		buf.WriteString(k)
		buf.WriteString(" = ")
		buf.WriteString(v.String())
		return nil
	})

	buf.WriteByte(')')
	return buf.String()
}

// Constructor returns the constructor used to create this struct.
func (s *Prototype) Constructor() Value { return s.constructor }

func (s *Prototype) Type() string { return "prototype" }
func (s *Prototype) Truth() Bool  { return true } // even when empty
func (s *Prototype) Hash() (uint32, error) {
	// Same algorithm as Tuple.hash, but with different primes.
	var x, m uint32 = 8731, 9839
	err := s.forEach(func(k string, v Value) error {
		namehash, _ := String(k).Hash()
		x = x ^ 3*namehash
		y, err := v.Hash()
		if err != nil {
			return err
		}
		x = x ^ y*m
		m += 7349
		return nil
	})

	if err != nil {
		return 0, err
	}

	return x, nil
}

func (s *Prototype) Freeze() {
	s.forEach(func(k string, v Value) error {
		v.Freeze()
		return nil
	})
	s.frozen = true
}

func (x *Prototype) Binary(op syntax.Token, y Value, side Side) (Value, error) {
	if y, ok := y.(*Prototype); ok && op == syntax.PLUS {
		if side == Right {
			x, y = y, x
		}

		if eq, err := Equal(x.constructor, y.constructor); err != nil {
			return nil, fmt.Errorf("in %s + %s: error comparing constructors: %v",
				x.constructor, y.constructor, err)
		} else if !eq {
			return nil, fmt.Errorf("cannot add structs of different constructors: %s + %s",
				x.constructor, y.constructor)
		}

		z := make(StringDict, y.len())
		for _, e := range y.entries {
			z[e.name] = e.value
		}

		return FromStringDict(x, z), nil
	}
	return nil, nil // unhandled
}

func (s *Prototype) Attr(name string) (Value, error) {
	return s.AttrOf(name, s)
}

// Attr returns the value of the specified field.
func (s *Prototype) AttrOf(name string, target Value) (Value, error) {
	for _, parent := range s.parents {
		if parent.name == name {
			return parent.value, nil
		}
	}

	var val Value

	s.AlongChain(func(p *Prototype) error {
		// Binary search the entries.
		// This implementation is a specialization of
		// sort.Search that avoids dynamic dispatch.
		n := len(p.entries)
		i, j := 0, n
		for i < j {
			h := int(uint(i+j) >> 1)
			if p.entries[h].name < name {
				i = h + 1
			} else {
				j = h
			}
		}
		if i < n && p.entries[i].name == name {
			val = p.entries[i].value
			return StopChain
		}

		return nil
	})

	if val != nil {
		if br, ok := val.(HasBindReceiver); ok {
			return br.BindReceiver(target), nil
		}

		return val, nil
	}

	return nil, NoSuchAttrError(
		fmt.Sprintf("no .%s field or method", name))
}

var ErrFrozen = errors.New("unable to change frozen prototype")

// SetField sets the entry to name to the given value. If the prototype is
// frozen returns an error.
func (s *Prototype) SetField(name string, v Value) error {
	if s.frozen {
		return ErrFrozen
	}

	for _, parent := range s.parents {
		if parent.name == name {
			p, ok := v.(*Prototype)
			if !ok {
				return fmt.Errorf("Unable to set parent to non-Prototype: %T", v)
			}

			parent.value = p
		}

		return nil
	}

	// Binary search the entries.
	// This implementation is a specialization of
	// sort.Search that avoids dynamic dispatch.
	n := len(s.entries)
	i, j := 0, n
	for i < j {
		h := int(uint(i+j) >> 1)
		if s.entries[h].name < name {
			i = h + 1
		} else {
			j = h
		}
	}
	if i < n && s.entries[i].name == name {
		s.entries[i].value = v
		return nil
	}

	s.entries = append(s.entries, protoSlot{name, v})

	sort.Sort(s.entries)

	return nil
}

func (s *Prototype) len() int { return len(s.entries) }

// AttrNames returns a new sorted list of the struct fields.
func (s *Prototype) AttrNames() []string {
	var names []string

	s.forEach(func(k string, v Value) error {
		names = append(names, k)
		return nil
	})

	return names
}

func (x *Prototype) CompareSameType(op syntax.Token, y_ Value, depth int) (bool, error) {
	y, ok := y_.(*Prototype)
	if !ok {
		return false, nil
	}

	switch op {
	case syntax.EQL:
		return structsEqual(x, y, depth)
	case syntax.NEQ:
		eq, err := structsEqual(x, y, depth)
		return !eq, err
	default:
		return false, fmt.Errorf("%s %s %s not implemented", x.Type(), op, y.Type())
	}
}

func (x *Prototype) ToList() (*List, error) {
	var l List

	x.forEach(func(k string, v Value) error {
		l.Append(Tuple{String(k), v})
		return nil
	})

	return &l, nil
}

func structsEqual(x, y *Prototype, depth int) (bool, error) {
	xl, err := x.ToList()
	if err != nil {
		return false, err
	}

	yl, err := y.ToList()
	if err != nil {
		return false, err
	}

	return EqualDepth(xl, yl, depth-1)
}
