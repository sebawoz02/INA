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

static int64_t extended_gcd_iterative(int64_t A, int64_t B, int64_t *X, int64_t *Y) {
    int64_t Q, R;
    int64_t X1, Y1, X0, Y0;
    int64_t nX, nY, A_copy, B_copy;

    X0 = 1; X1 = 0; Y0 = 0; Y1 = 1; A_copy = A; B_copy = B;
    
    while (B_copy != 0) {
        Q = A_copy / B_copy;
        R = A_copy % B_copy;
        A_copy = B_copy;
        B_copy = R;

        nX = X0 - Q * X1;
        nY = Y0 - Q * Y1;

        X0 = X1; Y0 = Y1; X1 = nX; Y1 = nY;
    }

    *X = X0;
    *Y = Y0;
    return A_copy;
}

static int64_t extended_gcd_recursive_helper(int64_t A, int64_t B, int64_t X0, int64_t Y0, int64_t X1, int64_t Y1, int64_t *X, int64_t *Y) {
    int64_t Q, R, nX, nY;
    
    if (B == 0) {
        *X = X0;
        *Y = Y0;
        return A;
    } else {
        Q = A / B;
        R = A % B;
        nX = X0 - Q * X1;
        nY = Y0 - Q * Y1;
        return extended_gcd_recursive_helper(B, R, X1, Y1, nX, nY, X, Y);
    }
}

static int64_t extended_gcd_recursive(int64_t A, int64_t B, int64_t *X, int64_t *Y) {
    return extended_gcd_recursive_helper(A, B, 1, 0, 0, 1, X, Y);
}

struct Result diophantine_equation_iterative(int64_t A, int64_t B, int64_t C) {
    struct Result Res;
    int64_t Gcd = extended_gcd_iterative(A, B, &Res.x, &Res.y);
    
    if (C % Gcd != 0) {
        // No solution exists
        Res.x = 0;
        Res.y = 0;
        return Res;
    }

    Res.x *= (C / Gcd);
    Res.y *= (C / Gcd);
    return Res;
}

struct Result diophantine_equation_recursive(int64_t A, int64_t B, int64_t C) {
    struct Result Res;
    int64_t Gcd = extended_gcd_recursive(A, B, &Res.x, &Res.y);
    
    if (C % Gcd != 0) {
        // No solution exists
        Res.x = 0;
        Res.y = 0;
        return Res;
    }

    Res.x *= (C / Gcd);
    Res.y *= (C / Gcd);
    return Res;
}
