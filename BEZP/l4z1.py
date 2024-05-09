import random
from sys import argv

def is_prime(n):
    if n <= 1:
        return False
    if n <= 3:
        return True
    if n % 2 == 0 or n % 3 == 0:
        return False
    i = 5
    while i * i <= n:
        if n % i == 0:
            return False
        i += 1
    return True

def extended_gcd(a, b):
    if a == 0:
        return (b, 0, 1)
    else:
        g, y, x = extended_gcd(b % a, a)
        return (g, x - (b // a) * y, y)

def gcd(a, b):
    return extended_gcd(a, b)[0]

def modinv(a, m):
    g, x, y = extended_gcd(a, m)
    if g != 1:
        raise Exception('Modular inverse does not exist')
    else:
        return x % m

def generate_keys(p, q):
    n = p * q
    phi = (p - 1) * (q - 1) // gcd(p-1, q-1)

    e = random.randint(2, phi - 1)
    while gcd(e, phi) != 1:
        e = random.randint(2, phi - 1)
    d = modinv(e, phi)

    # Public key: (n, e), Private key: (n, d)
    return (n, e), (n, d)

if __name__ == "__main__":
    try:
        p = int(argv[1])
        q = int(argv[2])
    except Exception:
        print("Podano błędne argumenty")
        exit(1)

    if not is_prime(p):
        print(f"p = {p} is not prime number")
        exit(1)
    if not is_prime(q):
        print(f"p = {q} is not prime number")
        exit(1)

    public_key, private_key = generate_keys(p, q)

    print("RSA Public Key:", public_key)
    print("RSA Private Key:", private_key)
