GF_P = 1234577


def mult_inv(value):
    p = GF_P
    a0 = value
    y = 0
    x = 1

    while value > 1 and p != 0:
        q = value // p

        t = p

        p = value % p
        value = t
        t = y

        y = x - q * y
        x = t

    if value != 1:
        raise ValueError(f"Nie istnieje odwrotność {a0} modulo {GF_P}")

    if x < 0:
        x += GF_P
    return x


class GF:
    def __init__(self, _val):
        self.val = _val % GF_P

    def __add__(self, other):
        return GF(self.val + other.val)

    def __sub__(self, other):
        return GF(self.val - other.val)

    def __mul__(self, other):
        return GF(self.val * other.val)

    def __truediv__(self, other):
        return GF(self.val * mult_inv(other.val))

    def __floordiv__(self, other):
        return self.val * mult_inv(other.val)

    def __iadd__(self, other):
        self.val = (self.val + other.val) % GF_P
        return self

    def __isub__(self, other):
        self.val = (self.val - other.val) % GF_P
        return self

    def __itruediv__(self, other):
        self.val = (self.val * mult_inv(other.val)) % GF_P
        return self

    def __ifloordiv__(self, other):
        self.val = (self.val * mult_inv(other.val)) % GF_P
        return self

    def __eq__(self, other):
        return self.val == other.val

    def __ne__(self, other):
        return self.val != other.val

    def __lt__(self, other):
        return self.val < other.val

    def __gt__(self, other):
        return self.val > other.val

    def __le__(self, other):
        return self.val <= other.val

    def __ge__(self, other):
        return self.val >= other.val


