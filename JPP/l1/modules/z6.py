from ctypes import *


so_file = "/home/sebawoz02/MyProjects/INA/JPP/l1/z6.so"
C_functions = CDLL(so_file)

def factorial_recursive(n : int) -> int:
    if n < 0:
        return 0
    return C_functions.factorial_recursive(n)

def factorial_iterative(n : int) -> int:
    if n < 0:
        return 0
    return C_functions.factorial_iterative(n)

def gcd_iterative(a: int, b: int) -> int:
    if a >= 0 and b >= 0:
        return C_functions.gcd_iterative(a, b)
    return 0

def gcd_recursive(a: int, b: int) -> int:
    if a >= 0 and b >= 0:
        return C_functions.gcd_recursive(a, b)
    return 0