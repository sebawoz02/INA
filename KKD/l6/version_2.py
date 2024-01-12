import sys
from pixel import Pixel
from math import ceil


def read_tga(input_file):
    """
    Loads image in tga format from input_file.
    :param input_file: .tga filename
    :return: 2D Pixel array, TGA header
    """
    with open(input_file, "br") as f:
        header = list(map(int, f.read(18)))
        width = header[13] * 256 + header[12]
        height = header[15] * 256 + header[14]

        image = []
        for _ in range(height):  # row
            for _ in range(width):  # col
                image.append(Pixel(*(list(map(int, f.read(3))))))
        return image, header


def read_encoded(input_file, bits):
    with open(input_file, "br") as f:
        header = list(map(int, f.read(18)))
        width = header[13] * 256 + header[12]
        height = header[15] * 256 + header[14]
        data = f.read()

    bits_string = "".join(format(byte, '08b') for byte in data)

    diffs_r = []
    diffs_g = []
    diffs_b = []
    # Extract diffs
    for i in range(0, width*height*9, 9):
        diffs_r.append(int(bits_string[i+1:i+9], 2) if bits_string[i] == '0'
                       else int(bits_string[i+1:i+9], 2)*-1)
    bits_string = bits_string[width*height*9:]
    for i in range(0, width * height * 9, 9):
        diffs_g.append(int(bits_string[i + 1:i + 9], 2) if bits_string[i] == '0'
                       else int(bits_string[i + 1:i + 9], 2) * -1)
    bits_string = bits_string[width * height * 9:]
    for i in range(0, width * height * 9, 9):
        diffs_b.append(int(bits_string[i + 1:i + 9], 2) if bits_string[i] == '0'
                       else int(bits_string[i + 1:i + 9], 2) * -1)
    bits_string = bits_string[width * height * 9:]

    # Extract high quantizer values
    quant_r_val = []
    quant_g_val = []
    quant_b_val = []
    n = 2 ** bits
    for i in range(0, n*8, 8):
        quant_r_val.append(int(bits_string[i:i+8], 2))
    bits_string = bits_string[n*8:]
    for i in range(0, n*8, 8):
        quant_g_val.append(int(bits_string[i:i + 8], 2))
    bits_string = bits_string[n*8:]
    for i in range(0, n*8, 8):
        quant_b_val.append(int(bits_string[i:i + 8], 2))
    bits_string = bits_string[n*8:]

    # Get high values
    val_range = width*height*bits
    r_z = [quant_r_val[int(bits_string[i:i + bits], 2)] for i in range(0, val_range, bits)]
    g_z = [quant_g_val[int(bits_string[i:i + bits], 2)] for i in range(val_range, val_range*2, bits)]
    b_z = [quant_b_val[int(bits_string[i:i + bits], 2)] for i in range(val_range*2, val_range*3, bits)]

    return (diffs_r, diffs_g, diffs_b), (r_z, g_z, b_z), header


# DIFFERENTIAL ENCODING ------------------


def get_differences(sequence, quantizer=None):
    a = sequence[0]
    result = [a]
    for p in sequence[1:]:
        if quantizer is not None:
            a = quantizer[a]
        result.append(p - a)
        a = p
    return result


def revert_differences(x, y):
    result = []
    for i in range(len(x)):
        tmp = max(0, min(255, x[i] + y[i]))
        result.append(tmp)
    return result


def apply_filters(image):
    """
    Applies given filter on each pixel
    :param image: 2D Pixel array
    :return:
    """
    r = []
    g = []
    b = []
    for i in range(len(image)):
        r.append(image[i].r)
        g.append(image[i].g)
        b.append(image[i].b)

    r_low = [r[0]] + [(r[i] + r[i - 1]) // 2 for i in range(1, len(r))]
    g_low = [g[0]] + [(g[i] + g[i - 1]) // 2 for i in range(1, len(g))]
    b_low = [b[0]] + [(b[i] + b[i - 1]) // 2 for i in range(1, len(b))]

    r_high = [r[0]] + [abs(r[i] - r[i - 1]) // 2 for i in range(1, len(r))]
    g_high = [g[0]] + [abs(g[i] - g[i - 1]) // 2 for i in range(1, len(g))]
    b_high = [b[0]] + [abs(b[i] - b[i - 1]) // 2 for i in range(1, len(b))]

    return (r_low, g_low, b_low), (r_high, g_high, b_high)


# NONUNIFORM QUANTIZER -------

def nonuniform_quantizer(quantized_values, bits, min_val=0, max_val=255):
    """
    Get quantizer_dict, quantizer values and indices
    """
    n = 2 ** bits
    while n > max_val - min_val:
        max_val = min(255, max_val + 1)
        min_val = max(-255, min_val - 1)
    occurrences = {i: 0 for i in range(min_val, max_val + 1)}
    for p in quantized_values:
        occurrences[p] += 1
    intervals = {(i, i + 1): occurrences[i] + occurrences[i + 1] for i in occurrences if i < max_val}
    while len(intervals) > n:
        min_interval = sorted(intervals, key=intervals.get)[0]
        dict_list = list(intervals)
        k = dict_list.index(min_interval)

        if k == 0:
            val = dict_list[1]
        elif k == len(dict_list) - 1:
            val = dict_list[-2]
        else:
            val = dict_list[k - 1] if intervals[dict_list[k - 1]] < intervals[dict_list[k + 1]] \
                else dict_list[k + 1]

        if val[0] > min_interval[0]:
            new_interval = (min_interval[0], val[1])
        else:
            new_interval = (val[0], min_interval[1])
        new_interval_value = intervals[min_interval] + intervals[val]
        intervals[new_interval] = new_interval_value
        del intervals[min_interval]
        del intervals[val]
        intervals = dict(sorted(intervals.items()))

    quantizer_val = [(el[0] + el[1]) // 2 for el in intervals]
    quantizer_dict = {}
    j = 0
    for i in range(min_val, max_val + 1):
        if j + 1 < n and abs(quantizer_val[j + 1] - i) <= abs(quantizer_val[j] - i):
            j += 1
        quantizer_dict[i] = j
    indices = [quantizer_dict[i] for i in quantized_values]

    return quantizer_dict, quantizer_val, indices


def encode(image, bits):
    (low_r, low_g, low_b), (high_r, high_g, high_b) = apply_filters(image)

    low_quantizer_r, _, _ = nonuniform_quantizer(low_r, bits, min_val=-255, max_val=255)
    low_quantizer_g, _, _ = nonuniform_quantizer(low_g, bits, min_val=-255, max_val=255)
    low_quantizer_b, _, _ = nonuniform_quantizer(low_b, bits, min_val=-255, max_val=255)

    diff_r, diff_g, diff_b = (get_differences(low_r, low_quantizer_r),
                              get_differences(low_g, low_quantizer_g),
                              get_differences(low_b, low_quantizer_b))

    _, high_quantizer_val_r, z_r = nonuniform_quantizer(high_r, bits)
    _, high_quantizer_val_g, z_g = nonuniform_quantizer(high_g, bits)
    _, high_quantizer_val_b, z_b = nonuniform_quantizer(high_b, bits)

    # Encode low differential
    encoded = ''.join(['0' + bin(abs(el))[2:].zfill(8) if el > 0
                       else '1' + bin(abs(el))[2:].zfill(8)
                       for el in diff_r])
    encoded += ''.join(['0' + bin(abs(el))[2:].zfill(8) if el > 0
                        else '1' + bin(abs(el))[2:].zfill(8)
                        for el in diff_g])
    encoded += ''.join(['0' + bin(abs(el))[2:].zfill(8) if el > 0
                        else '1' + bin(abs(el))[2:].zfill(8)
                        for el in diff_b])
    # Encode high quantizer values
    for i in (high_quantizer_val_r, high_quantizer_val_g, high_quantizer_val_b):
        encoded += ''.join(bin(el)[2:].zfill(8) for el in i)
    # Encode high indices
    for i in (z_r, z_g, z_b):
        encoded += ''.join(bin(el)[2:].zfill(bits) for el in i)

    padding = (8 - len(encoded) % 8) % 8
    encoded += '0'*padding
    return encoded


def decode(input_file, output_file, bits):
    (diffs_r, diffs_g, diffs_b), (r_z, g_z, b_z), header = read_encoded(input_file, bits)
    r = revert_differences(diffs_r, r_z)
    g = revert_differences(diffs_g, g_z)
    b = revert_differences(diffs_b, b_z)

    bitmap = [ch for sub in zip(b, g, r) for ch in sub]
    with open(output_file, "bw") as f:
        f.write(bytes(header) + bytes(bitmap))


def main():
    if sys.argv[1] == '-e':
        if len(sys.argv) < 4:
            print("Usage: python l6_main.py -e k in_file out_file")
            return
        else:
            image, h = read_tga(sys.argv[3])
            bitstring = encode(image, int(sys.argv[2]))
            with open(sys.argv[4], "bw") as f:
                f.write(bytes(h) + bytes([int(bitstring[i:i + 8], 2) for i in range(0, len(bitstring), 8)]))

    elif sys.argv[1] == '-d':
        if len(sys.argv) < 4:
            print("Usage: python l6_main.py -d k in_file out_file")
            return
        else:
            decode(sys.argv[3], sys.argv[4], int(sys.argv[2]))


if __name__ == "__main__":
    main()
