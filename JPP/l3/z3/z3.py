import random
from GF import GF
import math

class DHSetup:
    def __init__(self, T):
        self.T = T
        self.generator = self.generate_generator()

    def is_generator(self, a, p, primes):
        for q in primes:
            if self.power(a, (p - 1) // q) == 1:
                return False
        return True

    def generate_generator(self):
        p = self.T.get_p()

        primes = []
        for i in range(2, int(math.sqrt(p - 1)) + 1):
            if (p - 1) % i == 0:
                primes.append(i)
                if i != (p - 1) // i:
                    primes.append((p - 1) // i)

        while True:
            gen_value = random.randint(2, p - 1)
            a = self.T(gen_value)
            if self.is_generator(a, p, primes):
                return a

    def get_generator(self):
        return self.generator

    def power(self, a, b):
        result = self.T(1)
        while b > 0:
            if b % 2 == 1:
                result = result * a
            a = a * a
            b //= 2
        return result




class User:
    def __init__(self, setup):
        self.setup = setup
        self.secret = random.randint(2, setup.T.get_p() - 2)
        self.public_key = setup.power(setup.get_generator(), self.secret)
        self.encryption_key = None

    def get_public_key(self):
        return self.public_key

    def set_key(self, a):
        self.encryption_key = self.setup.power(a, self.secret)

    def encrypt(self, m):
        if self.encryption_key is None:
            raise ValueError("Encryption key is not set")
        return self.encryption_key * m

    def decrypt(self, c):
        if self.encryption_key is None:
            raise ValueError("Encryption key is not set")
        return c / self.encryption_key


# Przykład użycia:
GF.set_p(1234567891)

setup = DHSetup(GF)
user1 = User(setup)
user2 = User(setup)

user1.set_key(user2.get_public_key())
user2.set_key(user1.get_public_key())

print("Generator:", setup.get_generator())
print("Public Key User 1:", user1.get_public_key())
print("Public Key User 2:", user2.get_public_key())
message = 42
print("Message to encrypt:", message)
encrypted_message = user1.encrypt(message)
print("Encrypted Message:", encrypted_message)
decrypted_message = user2.decrypt(encrypted_message)
print("Decrypted Message:", decrypted_message)
