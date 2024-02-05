# Sebastian Woźniak
# Program wyznaczający liczbę MAX dla Float16, Float32 i Float64

# Funkcja znajdująca maksymalną wartość dla podanego typu
function find_max_value(T)
    max_value = nextfloat(zero(T))  # MAX inicjalizowany jako eta
    # Powiększaj wartość dwukrotnie dopóki następne działani nie sprawi
    # że max_value = INF
    while !isinf(max_value * convert(T, 2.0))   
        max_value *= convert(T, 2.0)
    end

    rest = T(max_value) # reszta brakująca do maksymalnej wartości
    # Zmniejszaj 'rest' dopóki nie stanie się liczbą maszynową eta
    while rest + max_value > max_value 
        # Dodawaj 'rest' do max_value dopóki max_value != INF
        while !isinf(max_value + rest)
            max_value += rest
        end
        rest /= convert(T, 2.0)
    end
    return max_value
end

# Float16
max_float16 = find_max_value(Float16)
println("Obliczony MAX dla Float16: ", max_float16)
println("floatmax(Float16): ", floatmax(Float16))

# Float32
max_float32 = find_max_value(Float32)
println("Obliczony MAX dla Float32: ", max_float32)
println("floatmax(Float32): ", floatmax(Float32))

# Float64
max_float64 = find_max_value(Float64)
println("Obliczony MAX dla Float64: ", max_float64)
println("floatmax(Float64): ", floatmax(Float64))