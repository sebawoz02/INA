from sys import argv
from l4z1 import modinv, gcd

def find_phi(n, e, d):
    p,q = 0,0
    kphi = d * e - 1
    t = kphi
    while t % 2 == 0:
        t //= 2
    a = 2
    while a < 100:
        k = t
        while k < kphi:
            x = pow(a, k, n)
            if x != t and x != n-1 and pow(x, 2, n) == 1:
                p = gcd(x-1, n)
                break
            k *= 2
        a += 2
    q = n//p
    return (p - 1) * (q - 1) // gcd(p-1, q-1)

def generate_private_key(pk_A, sk_A, pk_B):
    n_A, e_A = pk_A
    n_B, e_B = pk_B
    _, d_A = sk_A
    
    if n_A != n_B:
        raise ValueError("Public keys do not share the same modulus")

    phi_n = find_phi(n_A, e_A, d_A)

    d_B = modinv(e_B, phi_n)
    return (n_B, d_B)

if __name__ == "__main__":
    sk_A = (130607, 47017)
    pk_A = (130607, 2473)
    pk_B = (130607, 53581)

    sk_B = generate_private_key(pk_A, sk_A, pk_B) # should be (130607, 58921)
    print("Private key of user B (sk_B):", sk_B)