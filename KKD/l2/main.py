import math
from encoder import Encoder
from decoder import Decoder
import sys


def main():
    input_file = sys.argv[1]
    output_file = sys.argv[2]
    enc = Encoder()

    result_tmp = ""
    count = 1
    with open(input_file, "rb") as inf:
        byte = inf.read(1)
        while byte:
            result_tmp += enc.encode(byte)
            byte = inf.read(1)
            count += 1
    result = []
    pad = 0
    for i in range(math.ceil(len(result_tmp)/8)):
        temp = result_tmp[(i*8):((i+1)*8)]
        if len(temp) != 8:
            pad = 8 - len(temp)
            temp += "0" * pad
        temp = int(temp, 2)
        result.append(temp)
    result = [pad] + result
    with open(output_file, "wb+") as outf:
        for byte in result:
            outf.write(byte.to_bytes(1, byteorder='big'))

    enc.print_stats(input_file, output_file)


if __name__ == "__main__":
    main()
