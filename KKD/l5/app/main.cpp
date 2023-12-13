#include <cmath>
#include <cstdlib>
#include <fstream>
#include <header_tga.hpp>
#include <iostream>
#include <lbg_tree.hpp>
#include <vector>

static std::vector<std::vector<Pixel>> load_image(std::ifstream* input_stream,
                                                  TGA_header tga_head);

std::vector<std::vector<Pixel>> load_image(std::ifstream* input_stream,
                                           TGA_header tga_head)
{
  std::vector<std::vector<Pixel>> image(tga_head.height,
                                        std::vector<Pixel>(tga_head.width));

  for(int i = 0; i < tga_head.height; i++) {
    for(int j = 0; j < tga_head.width; j++) {
      *input_stream >> std::noskipws >> image[i][j].red;
      *input_stream >> std::noskipws >> image[i][j].green;
      *input_stream >> std::noskipws >> image[i][j].blue;
    }
  }

  return image;
}

int main(int argc, char** argv)
{
  if(argc < 4) {
    std::cout << "Usage:\n"
                 "./l5 <image_file> <output_file> <num_of_colors>\n"
                 "0 <= num_of_colors <= 24 -> [2^0, 2^24]"
              << std::endl;
    return 1;
  }
  // Load parameters
  std::ifstream input_stream = std::ifstream(argv[1]);
  std::ofstream output_stream = std::ofstream(argv[2]);
  auto no_colors = (int)strtol(argv[3], nullptr, 10);

  if(!input_stream.is_open() || !output_stream.is_open() ||
     !(no_colors >= 0 && no_colors <= 24)) {
    std::cout << "Usage:\n"
                 "./l5 <image_file> <output_file> <num_of_colors>\n"
                 "0 <= num_of_colors <= 24 -> [2^0, 2^24]"
              << std::endl;
    return 1;
  }

  // Read header
  TGA_header header{};
  input_stream.read(reinterpret_cast<char*>(&header), sizeof(TGA_header));

  // Load Pixels to 2D vector
  std::vector<std::vector<Pixel>> image = load_image(&input_stream, header);
  // Insert pixels to binary tree and perform vector quantization
  LBG_tree tree(image, no_colors);
  // Copy header to output file
  output_stream.write((char*)&header, sizeof(TGA_header));

  uint64_t mse_helper = 0;
  uint64_t snr_helper = 0;

  // Write image to output file and calculate mse and snr
  for(size_t i = 0; i < header.height; i++) {
    for(size_t j = 0; j < header.width; j++) {
      Pixel pixel = tree.next_node(image[i][j]);
      output_stream << pixel.red;
      output_stream << pixel.green;
      output_stream << pixel.blue;
      mse_helper += LBG_tree::distance(image[i][j], pixel);
      snr_helper += pixel.red * pixel.red + pixel.green * pixel.green +
                    pixel.blue * pixel.blue;
    }
  }

  input_stream.close();
  output_stream.close();

  double signal_power = double(snr_helper) / (header.height * header.width);
  double mse = double(mse_helper) / (header.height * header.width);
  double snr = 10.0 * log10(signal_power / mse);
  std::cout << "Mean square error:\t\t" << mse << std::endl;
  std::cout << "Signal to noise ratio [dB]:\t" << snr << std::endl;
}
