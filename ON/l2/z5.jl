# Sebastian Woźniak
# Program obliczający równanie rekurencyjne

# Funkcja obliczająca iteracyjnie rekurencyjne równanie p_n+1 = p_n + r*p_n*(1-p_n)
# @param T      - typ danych Float32 lub Float64
# @param n      - docelowa liczba iteracji
# @param r      - stała
# @param p0     - wartość początkowa
# @param cut    - jeżeli true po 10 iteracji zostanie przeprowadzone obcięcie wyniku do 3 liczb po przecinku
function p(T, n, r, p0, cut=false)
    i = 0
    result = T(p0)
    while i != n
        i += 1
        result = result + r*result*(1-result)
        if cut && i == 10
            result = T(trunc(result; digits=3))
        end
    end
    return result
end

println("Bez obcięcia Float32: $(p(Float32, 40, 3, 0.01))")
println("Z obcięciem Float32: $(p(Float32, 40, 3, 0.01, true))")
println("Bez obcięcia Float64: $(p(Float64, 40, 3, 0.01))")
println("Z obcięciem Float64: $(p(Float64, 40, 3, 0.01, true))")

println("n|\tFloat32\t|\tFloat64")
for i in 2:5:40
    println("$i\t$(p(Float32, i, 3, 0.01))\t$(p(Float64, i, 3, 0.01))")
end