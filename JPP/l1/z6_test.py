from modules import z6


assert z6.factorial_recursive(8) == 40320
assert z6.factorial_iterative(8) == 40320
assert z6.factorial_recursive(0) == 1
assert z6.factorial_iterative(0) == 1

assert z6.gcd_iterative(10193, 14303) == 1
assert z6.gcd_recursive(10193, 14303) == 1

print("Test completed positively!")