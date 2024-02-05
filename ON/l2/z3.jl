# Sebastian Woźniak
# Program rozwiązujący równanie liniowe Ax = b, gdzie A to macierz, a b to wektor prawych stron

using LinearAlgebra

# Funkcja generująca losowa macierz n x n z zadanym wskaźnikiem uwarunkowania c.
# @param n - rozmiar macierzy
# @param c - wskaźnik uwarunkowania macierzy
# @return macierz A
function matcond(n, c)
    # Wygenerowanie losowej macierzy n x n
    A = randn(n, n)
    
    # Obliczenie wartości osobliwych
    # U - lewa macierz ortogonalna
    # Σ - macierz diagonalna
    # V - prawa macierz ortogonalna
    U, Σ, V = svd(A)
    
    # Ponowna konstrukcja macierzy z wartości osobliwych
    A = U * diagm(0 => [LinRange(1.0, c, n);]) * V'
    
    return A
end

# Funkcja generująca macierz Hilberta o rozmiarze n
# @param n - rozmiar macierzy
# @return macierz Hilberta A
function mathib(n)
    # return hilbert(n)
    return [1 / (i + j - 1) for i in 1:n, j in 1:n]
end

# Funkcja obliczająca wartość wektora prawych stron b w równaniu Ax = b.
# @param A - macierz o rozmiarze n x n
# @param n - rozmiar macierzy
# @return wektor prawych stron
function calc_b(A, n)
    x = ones(n , 1)
    b = A*x
    return b
end

# Przeprowadzenie testów
# Macierz Hilberta
println("[Macierz Hilberta]")
println("n\t|\tBłąd metodą Gaussa\t|\tBłąd metodą inwersji\t|\tRząd macierzy\t|\tWskaznik uwarunkowania")
for n in 2:2:20
    x = ones(n , 1)
    A = mathib(n)
    b = calc_b(A, n)
    println("$n\t\t$(norm(A\b - x)/norm(x))\t\t$(norm(inv(A)*b-x)/norm(x))\t\t$(rank(A))\t\t$(cond(A))")
end
# Losowa macierz
println("[Macierz Losowa]")
println("n\t|\t\tc\t|\tBłąd metodą Gaussa\t|\tBłąd metodą inwersji\t|\tRząd macierzy")
for n in [5, 10, 20]
    for c in [ 1, 10, 10^3, 10^7, 10^12, 10^16]
        x = ones(n , 1)
        A = matcond(n, c)
        b = calc_b(A, n)
        println("$n\t\t$(norm(c))\t\t$(norm(A\b - x)/norm(x))\t\t$(norm(inv(A)*b-x)/norm(x))\t\t$(rank(A))")
    end
end


