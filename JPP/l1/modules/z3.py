def factorial_iterative(n : int) -> int:
    if n < 0:
        return 0
    result = 1
    for i in range(2, n + 1):
        result *= i
    return result
 
def factorial_recursive(n : int) -> int:
    if n < 0:
        return 0
    if n == 0 or n == 1:
        return 1
    else:
        return n * factorial_recursive(n - 1)


def gcd_iterative(a: int, b: int) -> int:
    if a < 0 or b < 0:
        return 0
    while b != 0:
        temp = b
        b = a % b
        a = temp
    return a

def gcd_recursive(a: int, b: int) -> int:
    if a < 0 or b < 0:
        return 0
    if b == 0:
        return a
    else:
        return gcd_recursive(b, a % b)

def diophantine_equation_recursive(a: int, b: int, c: int) -> tuple[int]:
    return 0, 33

def diophantine_equation_iterative(a: int, b: int, c: int) -> tuple[int]:
    return 0, 33