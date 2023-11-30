class Pixel:
    def __init__(self, red, green, blue):
        self.red = red
        self.green = green
        self.blue = blue

    def __repr__(self):
        return "({},{},{})".format(self.red, self.green, self.blue)

    def __str__(self):
        return "({},{},{})".format(self.red, self.green, self.blue)

    def __sub__(self, other):
        return Pixel((self.red - other.red) % 256, (self.green - other.green) % 256, (self.blue - other.blue) % 256)

    def __add__(self, other):
        return Pixel((self.red + other.red) % 256, (self.green + other.green) % 256, (self.blue + other.blue) % 256)

    def __floordiv__(self, other):
        return Pixel(self.red // other, self.green // other, self.blue // other)
