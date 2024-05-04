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

    @staticmethod
    def set_p(p):
        GF_P = p

    @staticmethod
    def get_p():
        return GF_P

    def __add__(self, other):
        if isinstance(other, GF):
            return GF(self.val + other.val)
        elif isinstance(other, int):
            return GF(self.val + other)

    def __sub__(self, other):
        if isinstance(other, GF):
            return GF(self.val + ( GF_P - other.val ))
        elif isinstance(other, int):
            return GF(self.val + ( GF_P - other % GF_P ))

    def __mul__(self, other):
        if isinstance(other, GF):
            return GF(self.val * other.val)
        elif isinstance(other, int):
            return GF(self.val * (other % GF_P))

    def __truediv__(self, other):
        if isinstance(other, GF):
            return GF(self.val * mult_inv(other.val))
        elif isinstance(other, int):
            return GF(self.val * mult_inv(other % GF_P))

    def __floordiv__(self, other):
        if isinstance(other, GF):
            return GF(self.val * mult_inv(other.val))
        elif isinstance(other, int):
            return GF(self.val * mult_inv(other % GF_P))

    def __iadd__(self, other):
        self.val = (self + other).val
        return self

    def __isub__(self, other):
        self.val = (self - other).val
        return self

    def __imul__(self, other):
        self.val = (self * other).val
        return self

    def __itruediv__(self, other):
        self.val = (self / other).val
        return self

    def __ifloordiv__(self, other):
        self.val = (self / other).val
        return self

    def __eq__(self, other):
        if isinstance(other, GF):
            return self.val == other.val
        elif isinstance(other, int):
            return self.val == other

    def __ne__(self, other):
        if isinstance(other, GF):
            return self.val != other.val
        elif isinstance(other, int):
            return self.val != other

    def __lt__(self, other):
        if isinstance(other, GF):
            return self.val < other.val
        elif isinstance(other, int):
            return self.val < other

    def __gt__(self, other):
        if isinstance(other, GF):
            return self.val > other.val
        elif isinstance(other, int):
            return self.val > other

    def __le__(self, other):
        if isinstance(other, GF):
            return self.val <= other.val
        elif isinstance(other, int):
            return self.val <= other

    def __ge__(self, other):
        if isinstance(other, GF):
            return self.val >= other.val
        elif isinstance(other, int):
            return self.val >= other

    def __str__(self):
        return "GF " + str(self.val)


