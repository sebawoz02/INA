include("l4_module.jl")

using .L4_Module

xs = [-1., 0., 1., 2.]
f = [-1., 0., -1., 2.]
println(naturalna(xs, ilorazyRoznicowe(xs, f)))
