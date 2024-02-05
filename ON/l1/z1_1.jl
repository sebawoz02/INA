# Sebastian Woźniak
# Program wyznaczający epsilony maszynowe

# Funkcja obliczająca epsilon maszynowy iteracyjnie
function calculate_epsilon(T)
    epsilon = convert(T, 1.0)   # zmienna 'epsilon' jest dzielona przez 2.0 aż stanie się epsilonem maszynowym
    while one(T) + (epsilon / convert(T, 2.0)) > one(T)
        epsilon /= convert(T, 2.0)
    end
    return epsilon
end

eps_float16 = calculate_epsilon(Float16)    # epsilon maszynowy dla Float16
eps_float32 = calculate_epsilon(Float32)    # epsilon maszynowy dla Float32   
eps_float64 = calculate_epsilon(Float64)    # epsilon maszynowy dla Float64

# epsilony maszynowe z funkcji eps()
eps_float16_julia = eps(Float16)  
eps_float32_julia = eps(Float32)
eps_float64_julia = eps(Float64)

# Float16
println("Obliczony epsilon: ", eps_float16)
println("Wartość z eps(Float16): ", eps_float16_julia)

# Float32
println("Obliczony epsilon: ", eps_float32)
println("Wartość z eps(Float32): ", eps_float32_julia)

# Float64
println("Obliczony epsilon: ", eps_float64)
println("Wartość z eps(Float64): ", eps_float64_julia)
