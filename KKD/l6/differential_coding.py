def differences_sequence(sequence):
    a = sequence[0]
    result = [a]
    for p in sequence[1:]:
        a = p - a
        result.append(a)
        a = p
    return result


def reconstruct_from_differences(diffs):
    a = diffs[0]
    result = [a]
    for q in diffs[1:]:
        a = a + q
        result.append(a)

    return result


def differential_encoding(bitmap):
    rs = [pixel.r for row in bitmap for pixel in row]
    gs = [pixel.g for row in bitmap for pixel in row]
    bs = [pixel.b for row in bitmap for pixel in row]

    bin_r = ['1' + bin(abs(el))[2:].zfill(8) if el < 0
             else '0' + bin(abs(el))[2:].zfill(8)
             for el in differences_sequence(rs)]
    bin_g = ['1' + bin(abs(el))[2:].zfill(8) if el < 0
             else '0' + bin(abs(el))[2:].zfill(8)
             for el in differences_sequence(gs)]
    bin_b = ['1' + bin(abs(el))[2:].zfill(8) if el < 0
             else '0' + bin(abs(el))[2:].zfill(8)
             for el in differences_sequence(bs)]

    bins = bin_r + bin_g + bin_b
    return ''.join(bins)


def read_encoded(file_in):
    with open(file_in, "br") as f:
        header = list(map(int, f.read(18)))
        n = int.from_bytes((f.read(1)), byteorder='big')
        result = ''.join([bin(c)[2:].zfill(8) for c in f.read()])
        result = result[:len(result) - n]
    return result, header


def differential_decoding(file):
    bitstring, header = read_encoded(file)
    all_colors = [int(el[1:], 2) if el[0] == '0' else -int(el[1:], 2) for el in
                  [bitstring[i:i + 9] for i in range(0, len(bitstring), 9)]]
    b_list = [el for el in all_colors[0: len(all_colors) // 3]]
    g_list = [el for el in all_colors[len(all_colors) // 3: 2 * len(all_colors) // 3]]
    r_list = [el for el in all_colors[2 * len(all_colors) // 3:]]

    rs = reconstruct_from_differences(r_list)
    gs = reconstruct_from_differences(g_list)
    bs = reconstruct_from_differences(b_list)
    rgbs = []
    for i in range(len(rs)):
        rgbs.extend([rs[i], gs[i], bs[i]])
    return rgbs, header
