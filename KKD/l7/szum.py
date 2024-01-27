from sys import argv
from random import random

if __name__ == "__main__":
    if len(argv) < 4:
        print("Usage: python3 szum.py <p> <in> <out>")
        exit(1)
    p = min(1.0, max(0.0, float(argv[1])))
    input_filename = argv[2]
    output_filename = argv[3]
    with open(input_filename, "br") as f_in, open(output_filename, "bw") as f_out:
        bitstring = ''.join([bin(c)[2:].zfill(8) for c in f_in.read()])
        bits = [bit for bit in bitstring]
        for i in range(len(bitstring)):
            if random() < p:
                bits[i] = '0' if bits[i] == '1' else '1'
        bitstring = ''.join(bits)
        f_out.write(bytes([int(bitstring[i:i + 8], 2) for i in range(0, len(bitstring), 8)]))
