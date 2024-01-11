import sys
from pixel import Pixel
from differential_coding import *


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
        for row in range(height):
            image.append([])
            for col in range(width):
                image[row].append(Pixel(*(list(map(int, f.read(3))))))
        return image, header, width, height


def sum_of_neighbours(image, i, j, width, height):
    """
    :return: Sum of all neighbours values and num of neighbours
    """
    num_of_neighbours = 0
    summ = 0
    if i > 0:
        summ += image[i - 1][j]
        num_of_neighbours += 1
        if j < width - 1:
            summ += image[i - 1][j + 1]
            num_of_neighbours += 1
        if j > 0:
            summ += image[i - 1][j - 1]
            num_of_neighbours += 1
    if i < height - 1:
        summ += image[i + 1][j]
        num_of_neighbours += 1
        if j < width - 1:
            summ += image[i + 1][j + 1]
            num_of_neighbours += 1
        if j > 0:
            summ += image[i + 1][j - 1]
            num_of_neighbours += 1
    summ += image[i][j]
    num_of_neighbours += 1
    if j < width - 1:
        summ += image[i][j + 1]
        num_of_neighbours += 1
    if j > 0:
        summ += image[i][j - 1]
        num_of_neighbours += 1
    return summ, num_of_neighbours


def highpass_filter(image, i, j, width, height):
    summ, neighbours = sum_of_neighbours(image, i, j, width, height)
    x = (neighbours+1) * image[i][j] - summ
    return 0 if x < 0 else 255 if x > 255 else x


def lowpass_filter(image, i, j, width, height):
    summ, neighbours = sum_of_neighbours(image, i, j, width, height)
    x = (image[i][j] + summ) // (neighbours + 1)
    return 0 if x < 0 else 255 if x > 255 else x


def apply_filter(image, filter_func, width, height):
    """
    Applies given filter on each pixel
    :param image: 2D Pixel array
    :param filter_func: lowpass_filter / highpass_filter
    :return:
    """
    rs = [[pixl.r for pixl in row] for row in image]
    gs = [[pixl.g for pixl in row] for row in image]
    bs = [[pixl.b for pixl in row] for row in image]

    output = []

    for i in range(height):
        output.append([])
        for j in range(width):
            output[i].append(Pixel(
                filter_func(bs, i, j, width, height),
                filter_func(gs, i, j, width, height),
                filter_func(rs, i, j, width, height)
            ))

    return output


def bitstring_to_file(bitstring, header, file_out):
    padding = 8 - len(bitstring) % 8
    bitstring = bitstring + padding * '0'
    bytes_list = bytes([padding]) + bytes([int(bitstring[i:i + 8], 2) for i in range(0, len(bitstring), 8)])
    with open(file_out, "bw") as f:
        f.write(bytes(header) + bytes_list)


def nonuniform_quantizer(color_values, bits):
    """
    Modified Linde-Buzo-Gray algorithm
    :param color_values: flattened (2D -> 1D) values fo one color
    :param bits: number of quantizer bits
    :return:
    """
    n = 2 ** bits
    occurrences = {i: 0 for i in range(0, 256)}
    for p in color_values:
        occurrences[p] += 1
    intervals = {(i, i + 1): occurrences[i] + occurrences[i + 1] for i in occurrences if i % 2 == 0}

    while len(intervals) > n:
        min_interval = sorted(intervals, key=intervals.get)[0]
        dict_list = list(intervals)
        k = dict_list.index(min_interval)

        if k == 0:
            to_join = dict_list[1]
        elif k == len(dict_list) - 1:
            to_join = dict_list[-2]
        else:
            to_join = dict_list[k - 1] if intervals[dict_list[k - 1]] < intervals[dict_list[k + 1]] \
                     else dict_list[k + 1]

        if to_join[0] > min_interval[0]:
            new_interval = (min_interval[0], to_join[1])
        else:
            new_interval = (to_join[0], min_interval[1])
        new_interval_value = intervals[min_interval] + intervals[to_join]
        intervals[new_interval] = new_interval_value
        del intervals[min_interval]
        del intervals[to_join]
        intervals = dict(sorted(intervals.items()))

    centroids = [(el[0] + el[1]) // 2 for el in intervals]
    centroid_dict = {}
    j = 0
    for i in range(0, 256):
        if j + 1 < n and abs(centroids[j + 1] - i) <= abs(centroids[j] - i):
            j += 1
        centroid_dict[i] = j

    indices = [centroid_dict[v] for v in color_values]
    return indices, centroids


def quantizer_encoding(image, bits):
    """
    Returns nonuniform quantized image for each color.
    :param image: 2D Pixel Array
    :param bits: number of quantizer bits
    :return: encoded image
    """
    # Flatten the image
    red = []
    green = []
    blue = []
    for _, row in enumerate(image):
        for _, pixel in enumerate(row):
            red.append(pixel.r)
            green.append(pixel.g)
            blue.append(pixel.b)

    red_indices, red_centroids = nonuniform_quantizer(red, bits)
    green_indices, green_centroids = nonuniform_quantizer(green, bits)
    blue_indices, blue_centroids = nonuniform_quantizer(blue, bits)

    encoded = red_centroids  # centroids + k bit indices
    for i in range(len(green_centroids)):
        encoded.append(green_centroids[i])
    for i in range(len(blue_centroids)):
        encoded.append(blue_centroids[i])
    encoded = ''.join([bin(el)[2:].zfill(8) for el in encoded])

    encoded_indices = []
    for i in range(len(red_indices)):
        encoded_indices.append(blue_indices[i])
        encoded_indices.append(green_indices[i])
        encoded_indices.append(red_indices[i])

    return encoded + ''.join([bin(el)[2:].zfill(bits) for el in encoded_indices])


def quantizer_decoding(file_name, bits):
    # Read file
    n = 2 ** bits
    with open(file_name, "rb") as f:
        header = list(map(int, f.read(18)))
        padding = f.read(1)
        red_centroids = list(map(int, f.read(n)))
        green_centroids = list(map(int, f.read(n)))
        blue_centroids = list(map(int, f.read(n)))
        image = f.read()

    # Extract indices and apply centroids
    bits_string = "".join(format(byte, '08b') for byte in image)
    output = []
    for i in range(len(bits_string)//bits):
        if i % 3 == 0:
            output.append(blue_centroids[int(bits_string[i*bits:i*bits+bits], 2)])
        elif i % 3 == 1:
            output.append(green_centroids[int(bits_string[i*bits:i*bits+bits], 2)])
        else:
            output.append(red_centroids[int(bits_string[i*bits:i*bits+bits], 2)])
    return output, header


def main():
    if sys.argv[1] == '-e':
        if len(sys.argv) < 5:
            print("Usage: python l6_main.py -e k in_file high_out_file low_out_file")
            return
        else:
            image, h, width, height = read_tga(sys.argv[3])

            high = apply_filter(image, highpass_filter, width, height)
            low = apply_filter(image, lowpass_filter, width, height)

            encoded_high = quantizer_encoding(high, int(sys.argv[2]))
            bitstring_to_file(encoded_high, h, sys.argv[4])

            encoded_low = differential_encoding(low)
            bitstring_to_file(encoded_low, h, sys.argv[5])

    elif sys.argv[1] == '-d':
        if len(sys.argv) < 4:
            print("Usage: python l6_main.py -d k -L/-H in_file out_file")
            return
        else:
            if sys.argv[3] == "-L":
                image, header = differential_decoding(sys.argv[4])
                with open(sys.argv[5], "wb") as f:
                    f.write(bytes(header) + bytes(image))
            if sys.argv[3] == "-H":
                image, header = quantizer_decoding(sys.argv[4], int(sys.argv[2]))
                with open(sys.argv[5], "wb") as f:
                    f.write(bytes(header) + bytes(image))


if __name__ == "__main__":
    main()
