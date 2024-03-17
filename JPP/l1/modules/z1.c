#include "z1.h"
#include <stdio.h>

uint64_t factorial_iterative(const uint8_t n)
{
    uint64_t result = 1;
    for (size_t i = 2; i <= n; ++i) {
        result *= i;
    }
    return result;
}

uint64_t factorial_recursive(const uint8_t n)
{
    if (n == 0 || n == 1)
        return 1;
    else
        return n * factorial_recursive(n - 1);
}

uint64_t gcd_iterative(uint64_t a, uint64_t b)
{
    uint64_t temp;
    while (b != 0) {
        temp = b;
        b = a % b;
        a = temp;
    }
    return a;
}

uint64_t gcd_recursive(const uint64_t a, const uint64_t b)
{
    if (b == 0)
        return a;
    else
        return gcd_recursive(b, a % b);
}
