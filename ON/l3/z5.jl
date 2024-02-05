# Sebastian Woźniak
# Wyznaczenie miejsca przecięcia f(x) = 3x oraz g(x) = e^x metodą bisekcji 

include("module_z1_z2_z3.jl")

using .SolvingFunctions

f(x) = 3*x
g(x) = ℯ^x 
fg(x) = f(x) - g(x) # Miejsce przecięcia 

delta = 1e-4
epsilon = 1e-4
a = 0     # or a = 0, b = 1
b = 1

(r, v, it, err) = SolvingFunctions.solve_bisect(fg, a, b, delta, epsilon)
if err == 1
    println("Brak wartości w [$(a); $(b)]")
else
    println("Wyznaczone miejsce przecięcia w przedziale [$(a), $(b)]: x = $(r), f(x) - g(x) = $(v), iteracje = $(it)")
end
