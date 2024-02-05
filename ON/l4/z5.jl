# Sebastian Woźniak 268491
# Testy l4z5

include("../l4_module.jl")
using .L4_Module

f1(x) = ℯ^x
a = 0.0
b = 1.0
for n in 5:5:15
    rysujNnfx(f1, a, b, n)
end 

f2(x) = x^2 * sin(x)
a = -1.0
b = 1.0
for n in 5:5:15
    rysujNnfx(f2, a, b, n)
end 