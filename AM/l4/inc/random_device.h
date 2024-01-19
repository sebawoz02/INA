#pragma once

#include <cstddef>
#include <random>

struct Random_Device{
public:
    explicit Random_Device(size_t n);

    std::pair<size_t, size_t> get_random_points();
    float get_random_float();
private:
    std::mt19937 generator;
    size_t range;
    std::uniform_int_distribution<size_t> distribution;
};