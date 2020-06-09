load("assert.star", "assert")

v = { "a": 1 }

c = => @a

d = c.bind(v)

assert.eq(1, d())
