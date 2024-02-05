# Sebastian Woźniak
# Wyznaczenie pierwiastka równania sin(x) − (1/2 * x)^2 = 0 

include("module_z1_z2_z3.jl")

using .SolvingFunctions


f(x) = sin(x) - (0.5 * x)^2
pf(x) = cos(x) - 0.5*x

delta = 0.5 * 1e-5
epsilon = 0.5 * 1e-5

(r, v, it, err) = SolvingFunctions.solve_bisect(f, -1, 2, delta, epsilon)
println("Metoda bisekcji: \tr = $(r),\tv = $(v),\tit = $(it)")
(r, v, it, err) = SolvingFunctions.solve_Newton(f, pf, 1.5, delta, epsilon, 50)
println("Metoda Newtona:  \tr = $(r),\tv = $(v),\tit = $(it)")
(r, v, it, err) = SolvingFunctions.solve_secant(f, 1, 2, delta, epsilon, 50)
println("Metoda siecznych:\tr = $(r),\tv = $(v),\tit = $(it)")
