#pragma once

#include <cstdint>
#include <stdio.h>
#define GF_P (int64_t)1234577 

class GF
{
public:
    int64_t value;
    GF(int64_t  _value) : value((_value % GF_P)){};

    GF operator+(const GF& other) const
    {
        return GF(value + other.value);
    }

    GF operator-(const GF& other) const    
    {
        return GF(value + (GF_P - other.value));
    }

    GF operator*(const GF& other) const
    {
        return GF(value * other.value);
    }

    GF operator/(const GF& other)
    {
        int64_t a = other.value;
        int64_t inversed = inverse(a);
        return GF(value * inversed);
    }
    
    bool operator==(const GF& other) const {
        return value == other.value;
    }

    bool operator!=(const GF& other) const {
        return !(*this == other);
    }

    bool operator<(const GF& other) const {
        return value < other.value;
    }

    bool operator<=(const GF& other) const {
        return value <= other.value;
    }

    bool operator>(const GF& other) const {
        return value > other.value;
    }

    bool operator>=(const GF& other) const {
        return value >= other.value;
    }

    GF& operator=(const GF& other) {
        if (this != &other) {
            value = other.value;
        }
        return *this;
    }

    GF& operator+=(const GF& other) {
        value = (value + other.value) % GF_P;
        return *this;
    }

    GF& operator-=(const GF& other) {
        value = (value + (GF_P - other.value)) % GF_P;
        return *this;
    }

    GF& operator*=(const GF& other) {
        value = (value * other.value) % GF_P;
        return *this;
    }

    GF& operator/=(const GF& other) {
        uint64_t inversed = inverse(other.value);
        value = (value * inversed) % GF_P;
        return *this;
    }

private:
    inline int64_t inverse(int64_t a)
    {
        int64_t p, a0, y, x;
        p = GF_P;
        a0 = a;
        y = 0;
        x = 1;

        while (a > 1 && p != 0)
        {
            int64_t q = a / p;
            int64_t t = p;
            p = a % p;
            a = t;
            t = y;
            y = x - q * y;
            x = t;
        }

        if (a != 1)
        {
            fprintf(stderr, "Nie istnieje odworotność %ld modulo %ld\n", a0, GF_P);
        }

        if(x < 0)
        {
            x += GF_P;
        }
        return x; 
    }
};
