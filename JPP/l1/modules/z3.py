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

def extended_gcd_iterative(A: int, B: int):
    X0, X1, Y0, Y1 = 1, 0, 0, 1
    A_copy, B_copy = A, B
    
    while B_copy != 0:
        Q = A_copy // B_copy
        R = A_copy % B_copy
        A_copy = B_copy
        B_copy = R

        nX = X0 - Q * X1
        nY = Y0 - Q * Y1

        X0, Y0, X1, Y1 = X1, Y1, nX, nY

    X = X0
    Y = Y0
    return A_copy, X, Y

def extended_gcd_recursive(A: int, B: int):
    def extended_gcd_recursive_helper(A, B, X0, Y0, X1, Y1):
        if B == 0:
            return A, X0, Y0
        else:
            Q = A // B
            R = A % B
            nX = X0 - Q * X1
            nY = Y0 - Q * Y1
            return extended_gcd_recursive_helper(B, R, X1, Y1, nX, nY)

    return extended_gcd_recursive_helper(A, B, 1, 0, 0, 1)

def diophantine_equation_iterative(A: int, B: int, C: int) -> tuple[int]:
    Gcd, x, y = extended_gcd_iterative(A, B)
    
    if C % Gcd != 0:
        # No solution exists
        return 0, 0
    
    ResX = x * (C // Gcd)
    ResY = y * (C // Gcd)
    return ResX, ResY

def diophantine_equation_recursive(A: int, B: int, C: int) -> tuple[int]:
    Gcd, x, y = extended_gcd_recursive(A, B)
    
    if C % Gcd != 0:
        # No solution exists
        return 0, 0
    
    ResX = x * (C // Gcd)
    ResY = y * (C // Gcd)
    return ResX, ResY
