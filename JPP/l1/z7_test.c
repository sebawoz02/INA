#include "modules/z7.h"
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

int main(void)
{
    z7_module_init();

    test_factorial_functions();

    test_gcd_functions();

    // test_diophantine_equations_functions();

    z7_module_destroy();

    printf("Test completed positively!\n");
    return 0;
}