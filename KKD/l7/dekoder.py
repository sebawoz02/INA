import numpy as np
from sys import argv

H_matrix = np.array([   # h(x) = ( x^7 - 1 )/ g(x) = x^4 + x^2 + x + 1
    [0, 0, 1, 0, 1, 1, 1],
    [0, 1, 0, 1, 1, 1, 0],
    [1, 0, 1, 1, 1, 0, 0],
    ])
two_errors_count = 0


def decode_byte(byte):
    global two_errors_count

    coded = [int(byte[i]) for i in range(0, len(byte) - 1)]
    parity = sum(coded) % 2
    old_parity = int(byte[7])

    decoded = [0]*3
    for r in range(3):
        for c in range(7):
            decoded[r] += H_matrix[r][c] * coded[c]
        decoded[r] %= 2

    syndrome = decoded[0] * 4 + decoded[1] * 2 + decoded[2]

    if parity != old_parity:
        if syndrome != 0:
            """
            001 - 0
            010 - 1
            101 - 2
            011 - 3
            111 - 4
            110 - 5
            100 - 6
            """
            indices = [0, 0, 1, 3, 6, 2, 5, 4]
            idx = indices[syndrome]
            coded[idx] = (coded[idx] + 1) % 2
    elif syndrome != 0:
        two_errors_count += 1
        return "0000"
    return str(coded[0]) + str((coded[1] - coded[0]) % 2) + str(coded[5]) + str(coded[6])


def decode_file(f_in, f_out):
    with open(f_in, "br") as f:
        bitstring = ''.join([bin(c)[2:].zfill(8) for c in f.read()])
    decoded = ''
    for i in range(0, len(bitstring), 8):
        decoded += decode_byte(bitstring[i:i+8])
    with open(f_out, "bw") as f:
        f.write(bytes([int(decoded[i:i + 8], 2) for i in range(0, len(decoded), 8)]))
    print(f"Podwójne błedy: {two_errors_count}")


if __name__ == "__main__":
    if len(argv) < 3:
        print("Usage: python3 dekoder.py <in> <out>")
        exit(1)
    decode_file(argv[1], argv[2])
