# Sebastian Woźniak
# Program wyznaczający liczby maszynowy eta

# Funkcja obliczająca epsilon maszynowy iteracyjnie
function calculate_eta(T)
    # zmienna 'eta' jest dzielona przez dwa do momentu kiedy następne działanie sprawi że eta = 0
    eta = convert(T, 1.0)   
    while eta / convert(T, 2.0) > zero(T)
        eta /= convert(T, 2.0)
    end
    return eta
end

# Float16
eta_float16 = calculate_eta(Float16)    # eta dla Float16
println("Obliczony Float16 Machine Eta: ", eta_float16)
println("nextfloat(Float16(0.0)): ", nextfloat(Float16(0.0)))

# Float32
eta_float32 = calculate_eta(Float32)    # eta dla Float32
println("Obliczony Float32 Machine Eta: ", eta_float32)
println("nextfloat(Float32(0.0)): ", nextfloat(Float32(0.0)))

# Float64
eta_float64 = calculate_eta(Float64)    # eta dla Float64
println("Obliczony Float64 Machine Eta: ", eta_float64)
println("nextfloat(Float64(0.0)): ", nextfloat(Float64(0.0)))