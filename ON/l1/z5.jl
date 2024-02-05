# Sebastian Woźniak
# Program obliczający iloczyn skalarny dwóch wektrów różnymi sposobami

# Funkcja liczaca iloczyn skalarny "w przód"
# @param T - precyzja np, Float32
# @param x - wektor x
# @param y - wektor y
# @param n - długośc wektorów
function dot_product_forward(T, x, y, n)
    S = T(0.0) # suma
    for i in 1:n
        S += T(x[i])*T(y[i])
    end
    return S
end    

# Funkcja liczaca iloczyn skalarny "w tył"
# @param T - precyzja np, Float32
# @param x - wektor x
# @param y - wektor y
# @param n - długośc wektorów
function dot_product_backward(T, x, y, n)
    S = T(0.0)  # suma
    for i in 1:n
        S += T(x[n-i+1])*T(y[n-i+1])
    end
    return S
end

# Funkcja liczaca iloczyn skalarny poprzez dodanie liczb dodatnich malejąco
# i ujemnych rosnąco.
# @param T - precyzja np, Float32
# @param x - wektor x
# @param y - wektor y
# @param n - długośc wektorów
function dot_product_descending(T, x, y, n)
    products = []   # pusta tablica na iloczyny
    for i in 1:n
        push!(products, T(x[i])*T(y[i]))
    end
    # posortowana malejąco tablica liczb nieujemnych
    positive_nums = sort(filter(a -> a >= 0, products), rev=true) 
    # posortowana rosnąco tablica liczb ujemnych
    negative_nums = sort(filter(a -> a < 0, products))
    S_1 = T(0.0) # suma nieujemnych liczb
    for i in positive_nums
        S_1 += T(i)
    end
    S_2 = T(0.0) # suma ujemnych liczb
    for i in negative_nums
        S_2 += T(i)
    end
    S = T(S_1) + T(S_2)
    return S
end 


# Funkcja liczaca iloczyn skalarny poprzez dodanie liczb dodatnich rosnąco
# i ujemnych malejąco.
# @param T - precyzja np, Float32
# @param x - wektor x
# @param y - wektor y
# @param n - długośc wektorów
function dot_product_ascending(T, x, y, n)
    products = []   # pusta tablica na iloczyny
    for i in 1:n
        push!(products, T(x[i])*T(y[i]))
    end
    # posortowana malejąco tablica liczb nieujemnych
    positive_nums = sort(filter(a -> a >= 0, products)) 
    # posortowana rosnąco tablica liczb ujemnych
    negative_nums = sort(filter(a -> a < 0, products), rev=true)
    S_1 = T(0.0) # suma nieujemnych liczb
    for i in positive_nums
        S_1 += T(i)
    end
    S_2 = T(0.0) # suma ujemnych liczb
    for i in negative_nums
        S_2 += T(i)
    end
    S = T(S_1) + T(S_2)
    return S
end 


x = [2.718281828, -3.141592654, 1.414213562, 0.5772156649, 0.3010299957]
y = [1486.2497, 878366.9879, -22.37492, 4773714.647, 0.000185049]

println("[Prawdziwy iloczyn skalarny]:")
println("-1.00657107000000e-11\n")
# Float32
println("[Pojedyńcza precyzja - Float32]:")
println("Iloczyn 'w przód': \t", dot_product_forward(Float32, x, y, 5))
println("Iloczyn 'w tył': \t", dot_product_backward(Float32, x, y, 5))
println("Iloczyn malejąco:\t", dot_product_descending(Float32, x, y, 5))
println("Iloczyn rosnąco:\t", dot_product_ascending(Float32, x, y, 5))

# Float64
println("\n[Podwójna precyzja - Float64]:")
println("Iloczyn 'w przód':\t", dot_product_forward(Float64, x, y, 5))
println("Iloczyn 'w tył':\t", dot_product_backward(Float64, x, y, 5))
println("Iloczyn malejąco:\t", dot_product_descending(Float64, x, y, 5))
println("Iloczyn rosnąco:\t", dot_product_ascending(Float64, x, y, 5))