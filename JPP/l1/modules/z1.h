#ifndef INA_JPP_L1_Z1_H
#define INA_JPP_L1_Z1_H

#include <stdint.h>

struct Result
{
    int64_t x;
    int64_t y;
};


uint64_t factorial_iterative(const uint8_t n);

uint64_t factorial_recursive(const uint8_t n);

uint64_t gcd_iterative(uint64_t a, uint64_t b);

uint64_t gcd_recursive(const uint64_t a, const uint64_t b);

struct Result diophantine_equation_iterative(const int64_t a, const int64_t b, const int64_t c); 

struct Result diophantine_equation_recursive(const int64_t a, const int64_t b, const int64_t c);  

#endif //INA_JPP_L1_Z1_H
