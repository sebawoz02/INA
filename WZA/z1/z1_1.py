class ComplexNumber:
    # a + bi
    def __init__(self, a: int, b: int):
        self.a = a
        self.b = b

    def __truediv__(self, other):
        a, b, c, d = self.a, self.b, other.a, other.b
        if c == 0 and d == 0:
            raise ZeroDivisionError("Cannot divide by zero (0+0i)")

        denominator = c ** 2 + d ** 2
        x = (a * c + b * d) // denominator
        y = (b * c - a * d) // denominator

        quotient = ComplexNumber(x, y)
        remainder = ComplexNumber(a - x * c + y * d, b - x * d - y * c)

        return [quotient, remainder]


# example
n1 = ComplexNumber(9, 3)
n2 = ComplexNumber(3, 2)
q, r = n1 / n2

print(q.a, q.b, r.a, r.b)
