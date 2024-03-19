from ctypes import *


so_file = "/home/sebawoz02/MyProjects/INA/JPP/l1/z6.so"
C_functions = CDLL(so_file)

class Result(Structure):
    _fields_ = [("x", c_long),
                ("y", c_long)]

def factorial_recursive(n : c_ulong) -> c_ulong:
    return C_functions.factorial_recursive(n)

def factorial_iterative(n : c_ulong) -> c_ulong:
    return C_functions.factorial_iterative(n)

def gcd_iterative(a: c_ulong, b: c_ulong) -> c_ulong:
    return C_functions.gcd_iterative(a, b)

def gcd_recursive(a: c_ulong, b: c_ulong) -> c_ulong:
    return C_functions.gcd_recursive(a, b)

def diophantine_equation_recursive(a: c_long, b: c_long, c: c_long) -> Result:
    func = C_functions.diophantine_equation_recursive
    func.restype = Result
    return func(a, b, c)

def diophantine_equation_iterative(a: c_long, b: c_long, c: c_long) -> Result:
    func = C_functions.diophantine_equation_iterative
    func.restype = Result
    return func(a, b, c)