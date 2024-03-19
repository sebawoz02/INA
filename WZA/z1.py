class GaussianInt:
    def __init__(self, re: int, im: int):
        self.re = round(re)
        self.im = round(im)

    def __str__(self):
        if self.im < 0:
            t = " - "
        else:
            t = " + "
        return f"{self.re}" + t + f"{abs(self.im)}i"

    def __truediv__(self, other):
        a, b, c, d = self.re, self.im, other.re, other.im
        if c == 0 and d == 0:
            raise ZeroDivisionError("Cannot divide by zero (0+0i)")

        denominator = c ** 2 + d ** 2
        x = (a * c + b * d) / denominator
        y = (b * c - a * d) / denominator

        quotient = GaussianInt(x, y)
        remainder = self - quotient * other

        return [quotient, remainder]

    def __floordiv__(self, other):
        return (self / other)[0]

    def norm(self):
        return self.re ** 2 + self.im ** 2

    def __sub__(self, other):
        return GaussianInt(self.re - other.re, self.im - other.im)

    def __add__(self, other):
        return GaussianInt(self.re + other.re, self.im + other.im)

    def __mul__(self, other):
        return GaussianInt(self.re * other.re - self.im*other.im, self.im*other.re + self.re*other.im)

    def __mod__(self, other):
        return (self / other)[1]

    @staticmethod
    def gcd(a, b):
        if a.norm() > b.norm():
            a, b = b, a
        while b.norm() > 0:
            a, b = b, a % b
        return a

    @staticmethod
    def lcm(a, b):
        return a*b // GaussianInt.gcd(a, b)


# example
n1 = GaussianInt(3, 4)
n2 = GaussianInt(1, 3)
q, r = n1 / n2

print(q, r)

print(GaussianInt.gcd(n1, n2))
print(GaussianInt.lcm(n1, n2))
