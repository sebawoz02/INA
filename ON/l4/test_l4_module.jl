# Sebastian Woźniak
# Testy do funkcji z modułu do zadań 1, 2, 3, 4

include("l4_module.jl")

using .L4_Module

function compare_vectors(v1, v2, eps)
    # Funkcja porównująca vektory z dopuszczalnym błędem eps.
    return all(abs.(v1 - v2) .<= eps)
end

eps = 1e-11 # dopuszczalny błąd

# Funkcja testująca plotowanie wielomianu interpolacyjnego
function test_rysujNnfx()
    # Samo sprawdzenie czy pomyślnie zostanie stworzony wykres
    f(x) = ℯ^x
    rysujNnfx(f, Float64(0), Float64(1), 5)
end

# Testy funkcji liczącej ilorazy różnicowe
function test_ilorazyRoznicowe()
    # f1
    f1(x) = 2*x + 5
    xs = [1., 3.]
    ilorazy = ilorazyRoznicowe(xs, f1.(xs))
    @assert compare_vectors(ilorazy, [7., 2.], eps)

    # f2
    f2(x) = x^3 - 3*x^2 + 3*x + 5
    xs = [1., 3. , 5., 7.]
    ilorazy = ilorazyRoznicowe(xs, f2.(xs))
    @assert compare_vectors(ilorazy, [6., 4., 6., 1.], eps)
end

# Testy funkcji liczącej wartości wielomianu
function test_warNewton()
    # f1
    f1(x) = 2*x + 5
    xs = [1., 3.]
    y = f1.(xs)
    ilorazy = ilorazyRoznicowe(xs, y)
    wartosci(t) = warNewton(xs, ilorazy, t)
    @assert compare_vectors(wartosci.(xs), [7., 11.], eps)

    # f2
    f2(x) = x^3 - 3*x^2 + 3*x + 5
    xs = [1., 3. , 5., 7.]
    y = f2.(xs)
    ilorazy = ilorazyRoznicowe(xs, y)
    wartosci2(t) = warNewton(xs, ilorazy, t)
    @assert compare_vectors(wartosci2.(xs), [6., 14., 70., 222.], eps)
end

# Testy funkcji liczącej wspołczynniki postaci naturalnej wielomianu
function test_naturalna()
    #f1
    f1(x) = 2*x + 5
    xs = [1., 3.]
    ilorazy = ilorazyRoznicowe(xs, f1.(xs))
    @assert compare_vectors(naturalna(xs, ilorazy), [5., 2.], eps)

    #f2
    f2(x) = x^3 - 3*x^2 + 3*x + 5
    xs = [1., 3. , 5., 7.]
    ilorazy = ilorazyRoznicowe(xs, f2.(xs))
    @assert compare_vectors(naturalna(xs, ilorazy), [5., 3., -3., 1.], eps)

    #f3
    f3(x) = x^3 - 5
    xs = [1., 2. , 3., 6.]
    ilorazy = ilorazyRoznicowe(xs, f3.(xs))
    @assert compare_vectors(naturalna(xs, ilorazy), [-5., 0., 0., 1.], eps)
end

# Test funkcji z zadania 1,2,3.
function test_all()
    # Dane wejściowe
    f(x) = 4*x^3 + 2*x^2 - 4*x
    xs = [0., 1., 2., 3., 4.]
    # Cel: z powyższymi danymi wyznaczyć wspołczynniki postaci naturalnej oraz wartosci
    # ilorazyRoznicowe -> warNewton
    # ilorazyRoznicowe -> naturalna
    y = f.(xs)
    ilorazy = ilorazyRoznicowe(xs, y)
    @assert compare_vectors(ilorazy, [0., 2., 14., 4., 0.], eps)

    wartosci(t) = warNewton(xs, ilorazy, t)
    @assert compare_vectors(wartosci.(xs), [0., 2., 32., 114., 272.], eps)

    @assert compare_vectors(naturalna(xs, ilorazy), [0., -4., 2., 4., 0.], eps)
end

print("Testy ilorazyRoznicowe()...")
test_ilorazyRoznicowe()
println("Success")
print("Testy warNewton()...")
test_warNewton()
println("Success")
print("Testy naturalna()...")
test_naturalna()
println("Success")
print("Testy połączenia funkcji 1,2,3...")
test_all()
println("Success")
print("Testy rysujNnfx()...")
test_rysujNnfx()
println("Success")