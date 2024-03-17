#include "modules/z1.h"
#include <assert.h>
#include <stdio.h>

static void test_factorial_functions(void)
{
    {
        const uint8_t n = 8;
        const uint64_t result = 40320;
        assert(factorial_recursive(n) == result);

        assert(factorial_iterative(n) == result);
    }

    {
        const uint8_t n = 0;
        const uint64_t result = 1;
        assert(factorial_recursive(n) == result);

        assert(factorial_iterative(n) == result);
    }
}

static void test_gcd_functions(void)
{
    {
        uint64_t a, b;
        a = 10193;
        b = 14303;

        assert(gcd_iterative(a, b) == 1);

        assert(gcd_recursive(a, b) == 1);
    }

    {
        uint64_t a, b;
        a = 10000460284912;
        b = 643634664;
        const uint64_t result = 8;
        assert(gcd_iterative(a, b) == result);

        assert(gcd_recursive(a, b) == result);
    }
}

/*
static void test_diophantine_equations_functions(void)
{
    {
        int64_t a = 96, b = -12, c = 36;
        diophantine_equation_iterative(a, b, c);
    }
    {
        int64_t a = 33, b = 12, c = 12;
        diophantine_equation_iterative(a, b, c);
    }
}
*/

int main(void)
{
    test_factorial_functions();

    test_gcd_functions();

    // test_diophantine_equations_functions();

    printf("Test completed positively!\n");
    return 0;
}