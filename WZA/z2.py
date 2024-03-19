class Polynomial:
    def __init__(self, coefficients, degree=0):
        if degree != 0:
            self.coefficients = [0 for _ in range(degree)] + coefficients
            self.degree = degree
        else:
            self.coefficients = coefficients    # [a_0, a_1, ..., a_{n-1}]
            self.degree = len(coefficients) - 1    # n

    def __str__(self):
        str_rep = ""
        for i in range(self.degree + 1):
            if self.coefficients[self.degree - i] < 0:
                sign = "- "
            elif i > 0:
                sign = "+ "
            else:
                sign = ""
            str_rep += sign + f"{abs(self.coefficients[self.degree - i])}x^{self.degree - i} "
        return str_rep[:-4]

    def __add__(self, other):
        combined_coefficients = [
            self.coefficients[i] + other.coefficients[i] if i <= self.degree and i <= other.degree else
            self.coefficients[i] if i <= self.degree else other.coefficients[i] for i in
            range(max(self.degree, other.degree) + 1)]
        while combined_coefficients[-1] == 0 and len(combined_coefficients) > 1:
            combined_coefficients.pop()
        return Polynomial(combined_coefficients)

    def __sub__(self, other):
        combined_coefficients = [
            self.coefficients[i] - other.coefficients[i] if i <= self.degree and i <= other.degree else
            self.coefficients[i] if i <= self.degree else other.coefficients[i] for i in
            range(max(self.degree, other.degree) + 1)]
        while combined_coefficients[-1] == 0 and len(combined_coefficients) > 1:
            combined_coefficients.pop()
        return Polynomial(combined_coefficients)

    def __truediv__(self, other):
        if other.degree == 0:
            if other.coefficients[0] == 0:
                raise ZeroDivisionError
            for i in range(self.degree + 1):
                self.coefficients[i] /= other.coefficients[0]
            return self, Polynomial([0])

        q = Polynomial([0])
        p = self
        # while LT(other) | LT(p)
        while 0 < p.degree and p.degree >= other.degree:
            # temp = LT(p) / LT(q)
            temp = Polynomial([p.coefficients[-1] / other.coefficients[-1]], p.degree - other.degree)
            q += temp
            p -= temp * other
        r = p
        return [q, r]

    def __floordiv__(self, other):
        return (self / other)[0]

    def __mod__(self, other):
        return (self / other)[1]

    def __mul__(self, other):
        if not isinstance(other, Polynomial):
            raise TypeError("Unsupported operand type(s) for *: 'Polynomial' and '{}'".format(type(other).__name__))

        result_degree = self.degree + other.degree
        result_coefficients = [0] * (result_degree + 1)

        for i, coeff1 in enumerate(self.coefficients):
            for j, coeff2 in enumerate(other.coefficients):
                result_coefficients[i + j] += coeff1 * coeff2

        return Polynomial(result_coefficients)

    @staticmethod
    def gcd(p, q):
        pass

    @staticmethod
    def lcm(p, q):
        return p*q // Polynomial.gcd(p, q)


# test
p1 = Polynomial([-10, -9, 2])
p2 = Polynomial([1, 3])

print(p1)
print(p2)

quotient, remainder = p1 / p2

print(quotient)
print(remainder)
