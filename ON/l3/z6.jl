# Sebastian Woźniak
# Wyznaczenie miejsca zerowego funkcji f1(x) = e^(1-x) - 1 oraz f2(x) = x*e^(-x) 

include("module_z1_z2_z3.jl")

using .SolvingFunctions


f1(x) = ℯ^(1-x) - 1
f2(x) = x*ℯ^(-x)

pf1(x) = -ℯ^(1-x)
pf2(x) = ℯ^(-x) *(1 - x*log(ℯ))

delta = 1e-5
epsilon = 1e-5

println("[Funkcja f1]:")
(r, v, it, err) = SolvingFunctions.solve_bisect(f1, 0, 2, delta, epsilon)
println("Metoda bisekcji: \tr = $(r),\tv = $(v),\tit = $(it), err = $(err)")
(r, v, it, err) = SolvingFunctions.solve_Newton(f1, pf1, 0.5, delta, epsilon, 50)
println("Metoda Newtona:  \tr = $(r),\tv = $(v),\tit = $(it), err = $(err)")
(r, v, it, err) = SolvingFunctions.solve_secant(f1, 0, 0.5, delta, epsilon, 50)
println("Metoda siecznych:\tr = $(r),\tv = $(v),\tit = $(it), err = $(err)")

println("[Funkcja f2]:")
(r, v, it, err) = SolvingFunctions.solve_bisect(f2, -1, 1, delta, epsilon)
println("Metoda bisekcji: \tr = $(r),\tv = $(v),\tit = $(it), err = $(err)")
(r, v, it, err) = SolvingFunctions.solve_Newton(f2, pf2, 1, delta, epsilon, 100)
println("Metoda Newtona:  \tr = $(r),\tv = $(v),\tit = $(it), err = $(err)")
(r, v, it, err) = SolvingFunctions.solve_secant(f2, -1, -0.5, delta, epsilon, 100)
println("Metoda siecznych:\tr = $(r),\tv = $(v),\tit = $(it), err = $(err)")