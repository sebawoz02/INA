from modules import z3

assert z3.factorial_recursive(8) == 40320
assert z3.factorial_iterative(8) == 40320
assert z3.factorial_recursive(0) == 1
assert z3.factorial_iterative(0) == 1

assert z3.gcd_iterative(10193, 14303) == 1
assert z3.gcd_recursive(10193, 14303) == 1

x, y = z3.diophantine_equation_iterative(3, 25, 1)
assert x == -8
assert y == 1

x, y = z3.diophantine_equation_recursive(3, 25, 1)
assert x == -8
assert y == 1

print("Test completed positively!")