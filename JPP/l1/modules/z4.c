#include <stdint.h>

extern uint64_t ada_factorial_iterative(const uint8_t n);
extern uint64_t ada_factorial_recursive(const uint8_t n);
extern uint64_t ada_gcd_iterative(uint64_t a, uint64_t b);
extern uint64_t ada_gcd_recursive(const uint64_t a, const uint64_t b);

uint64_t factorial_iterative(const uint8_t n)
{
    return ada_factorial_iterative(n);
}

uint64_t factorial_recursive(const uint8_t n)
{
    return ada_factorial_recursive(n);
}

uint64_t gcd_iterative(uint64_t a, uint64_t b)
{
    return ada_gcd_iterative(a, b);
}

uint64_t gcd_recursive(const uint64_t a, const uint64_t b)
{
    return ada_gcd_recursive(a, b);
}