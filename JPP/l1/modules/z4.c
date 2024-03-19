#include <stdint.h>
#include "z4.h"

extern uint64_t ada_factorial_iterative(const uint8_t n);
extern uint64_t ada_factorial_recursive(const uint8_t n);
extern uint64_t ada_gcd_iterative(uint64_t a, uint64_t b);
extern uint64_t ada_gcd_recursive(const uint64_t a, const uint64_t b);
extern struct Result ada_diophantine_equation_iterative(const int64_t a, const int64_t b, const int64_t c);
extern struct Result ada_diophantine_equation_recursive(const int64_t a, const int64_t b, const int64_t c);

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

struct Result diophantine_equation_iterative(const int64_t a, const int64_t b, const int64_t c){
    return ada_diophantine_equation_iterative(a, b ,c);
}

struct Result diophantine_equation_recursive(const int64_t a, const int64_t b, const int64_t c){
    return ada_diophantine_equation_recursive(a, b ,c);
}