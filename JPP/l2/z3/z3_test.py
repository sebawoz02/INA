from GF import GF

# Addition
a = GF(3)
b = GF(7)
result = GF(10)
assert a + b == result
a += b
assert a == result

# Substraction
a = GF(915)
b = GF(916)
result = GF(1234576)
assert a - b == result
a -= b
assert a == result

# Multiplication
a = GF(12)
b = GF(88)
result = GF(1056)
assert a * b == result
a *= b
assert a == result
assert a > b
assert a >= b
assert b < a
assert b <= a

# Division
b = GF(11)
a = GF(88)
result = GF(8)

assert a / b == result
a /= b
assert a == result

