# Sebastian Woźniak 268491
# Testy l4z6

include("../l4_module.jl")
using .L4_Module

f1(x) = abs(x)
a = -1.0
b = 1.0
for n in 5:5:15
#    rysujNnfx(f1, a, b, n)
end 

f2(x) = 1/(1+x^2)
a = -5.0
b = 5.0
for n in 5:5:15
    rysujNnfx(f2, a, b, n)
end 