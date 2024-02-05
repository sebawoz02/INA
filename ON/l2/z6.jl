# Sebastian Woźniak
# Program obliczający równanie rekurencyjne

# Funkcja obliczająca iteracyjnie rekurencyjne równanie x_n+1 := (x_n)^2 + c
# @param n  - liczba iteracji
# @param c  - stała
# @param x0 - wartość początkowa
function x(n, c, x0)
    result = Float64(x0)
    for i in 1:n
        result = result^2 + c
        println("$i $result")
    end
    return result
end

x(40, -1, 0.25)

