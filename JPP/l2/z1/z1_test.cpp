#include "GF.hpp"
#include <cassert>
#include <stdio.h>


int main()
{
    // Addition
    GF a = GF(3);
    GF b = GF(7);
    GF result = GF(10);
    assert(a + b == result);
    a += b;
    assert(a == result);

    // Substraction
    a = GF(915);
    b = GF(916);
    result = GF(1234576);
    assert(a - b == result);
    a -= b;
    assert(a == result);

    // Multiplication
    a = GF(12);
    b = GF(88);
    result = GF(1056);
    assert(a * b == result);
    a *= b;
    assert(a == result);
    assert(a > b);
    assert(a >= b);
    assert(b < a);
    assert(b <= a);

    // Division
    b = GF(11);
    a = GF(88);
    result = GF(8);

    assert(a / b == result);
    a /= b;
    assert(a == result);

    return 0;
}