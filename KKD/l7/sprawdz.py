from sys import argv

if __name__ == "__main__":
    if len(argv) < 3:
        print("Usage: python3 sprawdz.py <in1> <in2>")
        exit(1)
    filename_1, filename_2 = argv[1], argv[2]
    with open(filename_1, "br") as f1, open(filename_2, "br") as f2:
        counter = 0
        bitstring_1 = ''.join([bin(byte)[2:].zfill(8) for byte in f1.read()])
        bitstring_2 = ''.join([bin(byte)[2:].zfill(8) for byte in f2.read()])

    for i in range(min(len(bitstring_1), len(bitstring_2))):
        if bitstring_1[i:(i+4)] != bitstring_2[i:(i+4)]:
            counter += 1
    print("Liczba nieidentycznych 4-bitowych bloków:", counter)
