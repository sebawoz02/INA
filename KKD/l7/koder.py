from sys import argv


G_matrix = [   # g(x) = 1 + x + x^3
    [1, 0, 0, 0],
    [1, 1, 0, 0],
    [0, 1, 1, 0],
    [1, 0, 1, 1],
    [0, 1, 0, 1],
    [0, 0, 1, 0],
    [0, 0, 0, 1],
]


def hamming_encode(bits):
    coded = [0] * 7
    for r in range(7):
        for c in range(4):
            coded[r] += G_matrix[r][c] * int(bits[c])
        coded[r] %= 2
    parity = sum(coded) % 2
    coded.append(parity)
    return ''.join(map(str, coded))


def encode(f_in, f_out):
    with open(f_in, "br") as f_in:
        bitstring = ''.join([bin(c)[2:].zfill(8) for c in f_in.read()])
    encoded = ''.join([hamming_encode(bitstring[i:i+4]) for i in range(0, len(bitstring), 4)])
    with open(f_out, "bw") as f_out:
        f_out.write(bytes([int(encoded[i:i+8], 2) for i in range(0, len(encoded), 8)]))


if __name__ == "__main__":
    if len(argv) < 3:
        print("Usage: python3 koder.py <in> <out>")
        exit(1)
    encode(argv[1], argv[2])
