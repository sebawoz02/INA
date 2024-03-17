#ifndef INA_JPP_L1_Z7_H
#define INA_JPP_L1_Z7_H

#include <stdint.h>

void z7_module_init(void);

uint64_t factorial_iterative(const uint8_t n);

uint64_t factorial_recursive(const uint8_t n);

uint64_t gcd_iterative(uint64_t a, uint64_t b);

uint64_t gcd_recursive(const uint64_t a, const uint64_t b);

void z7_module_destroy(void);

#endif //INA_JPP_L1_Z7_H