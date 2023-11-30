import sys
import math
import numpy as np
from Pixel import Pixel


def encode(pixels, image_width, image_height, mode):
    encoded = []
    for i in range(image_width):
        for j in range(image_height):
            if j == 0:
                w = Pixel(0, 0, 0)
            else:
                w = pixels[image_width * i + (j - 1)]
            if i == 0:
                n = Pixel(0, 0, 0)
            else:
                n = pixels[image_width * (i - 1) + j]
            if j == 0 or i == 0:
                nw = Pixel(0, 0, 0)
            else:
                nw = pixels[image_width * (i - 1) + (j - 1)]
            encoded.append(pixels[image_width * i + j] - predicator_formula(mode, w, n, nw))

    return encoded


def get_entropy(pixels, type):
    result = {}
    size = 0
    if type == 'all':
        for pixel in pixels:
            if pixel.__repr__() in result.keys():
                result[pixel.__repr__()] += 1
            else:
                result[pixel.__repr__()] = 1
            size += 1
    else:
        for i in range(256):
            result[i] = 0
        for pixel in pixels:
            result[getattr(pixel, type)] += 1
            size += 1
    entropy = 0
    for item in result.values():
        if item == 0:
            continue
        p = item/size
        entropy -= p*math.log2(p)
    return entropy


def loco_l(w, n, nw):
    if nw >= max(w, n):
        return min(w, n)
    if nw <= min(w, n):
        return max(w, n)
    return w + n - nw


def predicator_formula(mode, w, n, nw):
    return {
        1: w,
        2: n,
        3: nw,
        4: n + w - nw,
        5: n + (w - nw),
        6: w + ((n - nw) // 2),
        7: (n + w) // 2,
        8: Pixel(loco_l(w.red, n.red, nw.red),
                 loco_l(w.green, n.green, nw.green),
                 loco_l(w.blue, n.blue, nw.blue),
                 )
    }[mode]


def read_tga_file(filename):
    with open(filename, "rb") as f:
        header = f.read(18)
        width = header[13] * 256 + header[12]
        height = header[15] * 256 + header[14]
        image_data = np.frombuffer(f.read(width * height * 3), dtype=np.uint8).reshape((height, width, 3))[::-1, :, ::-1]
        pixels = []
        for i in range(height):
            for j in range(width):
                pixels.append(Pixel(
                    int(image_data[i][j][0]),
                    int(image_data[i][j][1]),
                    int(image_data[i][j][2]))
                )
        return np.array(pixels), width, height


def main():
    predicators = {
        1: 'W',
        2: 'N',
        3: 'NW',
        4: 'N+W-NW',
        5: 'N+(W-NW)',
        6: 'W+(N-NW)/2',
        7: '(N+W)/2',
        8: 'LOCO-l'  # new standard
    }
    if len(sys.argv) < 2:
        return
    file = sys.argv[1]
    pixels, image_width, image_height = read_tga_file(file)
    types = ['all', 'red', 'green', 'blue']
    modes = [1, 2, 3, 4, 5, 6, 7, 8]
    entropies = []

    # Calculate entropy for all possible predicators
    print("[INPUT ENTROPY]:\n")
    for x in types:
        print(f"{x} entropy\t {get_entropy(pixels, x)}")
        for i in modes:
            entropies.append((i, x, get_entropy(encode(pixels, image_width, image_height, i), x)))

    # Print entropy for each color and each predicator
    max_key_len = max(map(len, predicators.values()))
    for j in range(4):
        print(f"\n[{entropies[j * 8][1]}]:")
        for i in range(8):
            print(f"{predicators[entropies[j * 4 + i][0]].ljust(max_key_len)} {entropies[j * 4 + i][2]}")

    # Find optimal encoding for each color
    print("\n[OPTIMAL]:")
    for x in types:
        temp = []
        for item in entropies:
            if item[1] == x:
                temp.append(item)
        opt = min(temp, key=lambda t: t[2])
        print(f"{opt[1]} - {predicators[opt[0]]} : {opt[2]}")


if __name__ == "__main__":
    main()
