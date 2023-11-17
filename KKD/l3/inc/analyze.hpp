#pragma once

#include <map>
#include <string>

double calculate_entropy(const uint32_t* occurs, size_t file_size);
double calculate_compression_rate(size_t in_size, size_t out_size);
