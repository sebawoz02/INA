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
        byte = f_in.read(1)
        while byte:
            val = int.from_bytes(byte, byteorder='big')
            if random() < p:
                val ^= 0b11111111
            f_out.write(val.to_bytes(1, byteorder='big'))
            byte = f_in.read(1)
