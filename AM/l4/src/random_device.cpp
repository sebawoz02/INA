#include <random_device.h>


std::pair<size_t, size_t> Random_Device::get_random_points() {
    size_t i = 0;
    size_t j = 0;
    while(i + 1 == j || i == j + 1 || i == j) {
        i = distribution(generator);
        j = distribution(generator);
    }
    if(j < i)
        return {j, i};
    return {i, j};
}


Random_Device::Random_Device(size_t n) {
    std::random_device rd;
    generator = std::mt19937(rd());
    distribution = std::uniform_int_distribution<size_t>(1, n - 2);
    range = n - 2;
}


float Random_Device::get_random_float() {
    float x;
    size_t num = distribution(generator);

    x = (static_cast<float>(num) - 1.0f) / static_cast<float>(range);
    return x;
}