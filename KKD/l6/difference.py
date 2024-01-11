import sys
import numpy as np
from pixel import Pixel


def read_tga(input_file):
    """
    Loads image in tga format from input_file.
    :param input_file: .tga filename
    :return: 2D Pixel array, width, height
    """
    with open(input_file, "br") as f:
        header = list(map(int, f.read(18)))
        width = header[13] * 256 + header[12]
        height = header[15] * 256 + header[14]

        image = []
        for i in range(height):
            image.append([])
            for j in range(width):
                image[i].append(Pixel(*(list(map(int, f.read(3))))))
        return [p for row in image for p in row]


def calculate_mse(image1, image2, color="all"):
    if color == "red":
        values1 = np.array([p.r for p in image1])
        values2 = np.array([p.r for p in image2])
        return np.mean((values1 - values2)**2)
    if color == "green":
        values1 = np.array([p.g for p in image1])
        values2 = np.array([p.g for p in image2])
        return np.mean((values1 - values2)**2)
    if color == "blue":
        values1 = np.array([p.b for p in image1])
        values2 = np.array([p.b for p in image2])
        return np.mean((values1 - values2)**2)
    values1 = np.array([np.array([p.r, p.g, p.b]) for p in image1])
    values2 = np.array([np.array([p.r, p.g, p.b]) for p in image2])
    return np.mean((values1 - values2) ** 2)


def calculate_psnr(mse):
    if mse == 0:
        return 0
    max_pixel_value = 255.0
    return 20 * np.log10(max_pixel_value / np.sqrt(mse))


def main():
    file_path1 = sys.argv[1]
    file_path2 = sys.argv[2]

    image1 = read_tga(file_path1)

    image2 = read_tga(file_path2)

    mse_total = calculate_mse(image1, image2)

    mse_red = calculate_mse(image1, image2, "red")
    mse_green = calculate_mse(image1, image2, "green")
    mse_blue = calculate_mse(image1, image2, "blue")

    psnr_total = calculate_psnr(mse_total)
    psnr_red = calculate_psnr(mse_red)
    psnr_green = calculate_psnr(mse_green)
    psnr_blue = calculate_psnr(mse_blue)

    # Wyświetl wyniki
    print(f"Błąd średniokwadratowy całego obrazu: {mse_total}")
    print(f"Błąd średniokwadratowy składowej czerwonej: {mse_red}")
    print(f"Błąd średniokwadratowy składowej zielonej: {mse_green}")
    print(f"Błąd średniokwadratowy składowej niebieskiej: {mse_blue}")
    print(f"PSNR całego obrazu: {psnr_total} dB")
    print(f"PSNR składowej czerwonej: {psnr_red} dB")
    print(f"PSNR składowej zielonej: {psnr_green} dB")
    print(f"PSNR składowej niebieskiej: {psnr_blue} dB")


if __name__ == "__main__":
    main()
