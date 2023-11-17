//
// Created by sebawoz02 on 17.11.23.
//
#include <cmath>

#include <analyze.hpp>

double calculate_entropy(const uint32_t* occurs, const size_t file_size)
{
  double entropy = 0.0;

  for(size_t i = 0; i < 256; i++) {
    if(occurs[i] == 0)
      continue;
    const double p =
      static_cast<double>(occurs[i]) / static_cast<double>(file_size);
    entropy -= p * log2(p);
  }
  return entropy;
}
double calculate_compression_rate(const size_t in_size, const size_t out_size)
{
  return static_cast<double>(in_size) / static_cast<double>(out_size);
}
