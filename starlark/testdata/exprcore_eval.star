load("assert.star", "assert")

v = { "a": 1 }

c = => @a

d = c.bind(v)

assert.eq(1, d())
		
person = %{
  age: 12
  def bar() {
    return 13
  }
}

assert.eq(12, person.age)
