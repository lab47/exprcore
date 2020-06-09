# Tests of Module.

load("assert.star", "assert")

assert.eq(type(assert), "module")
assert.eq(str(assert), '<module "assert">')
assert.eq(dir(assert), ["contains", "eq", "fail", "fails", "lt", "ne", "true"])
assert.fails(=> ({assert: None}), "unhashable: module")

def assignfield() {
    assert.foo = None
}
assert.fails(assignfield, "can't assign to .foo field of module")

# no such field
assert.fails(=> assert.nonesuch, "module has no .nonesuch field or method$")
assert.fails(=> assert.falls, "module has no .falls field or method .did you mean .fails\\?")
