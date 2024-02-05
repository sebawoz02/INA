# Sebastian Woźniak
# Testy do funkcji z modułu do zadań 1, 2, 3

include("module_z1_z2_z3.jl")

using .SolvingFunctions

# Dane używane w testach
function f(x)
    return -x - 3
end
function g(x)
    return x - pi
end
function h(x)
    return x^3 - 7*x + 6
end
delta = 1e-4
epsilon = 1e-4

# Funkcja przeprowadzająca testy na funkcji używającej metody bisekcji
function test_solve_bisect()

    # Test 1
    (_, _, it, err) = SolvingFunctions.solve_bisect(f, 3, 1, delta, epsilon)
    
    @assert err == 1  # Błednie podany przedział ( a >= b ) powinenin zwrócić błąd
    @assert it == 0
    
    # Test 2
    (_, _, it, err) = SolvingFunctions.solve_bisect(f, -2, 2, delta, epsilon)
    
    @assert err == 1  # Przedział w którym nie zmienia się znak zwraca błąd
    @assert it == 0

    # Test 3
    (r, v, it, err) = SolvingFunctions.solve_bisect(f, -4, 0, delta, epsilon) # Poprawne wykonanie funkcji

    @assert r == -3
    @assert v == 0
    @assert it == 2 # ((-4+0)/2) -> ((-4-2)/2)
    @assert err == 0

    # Test 4
    (r, v, it, err) = SolvingFunctions.solve_bisect(g, 0, 4, delta, epsilon) # Wyznaczenie liczby pi

    @assert abs(pi - r) <= delta
    @assert abs(v) <= epsilon
    @assert it > 10     # Wyznaczenie dokładnej wartości wymaga więcej iteracji
    @assert err == 0

    # Test 5
    xs = [-3, 1, 2]
    (r, v, _, err) = SolvingFunctions.solve_bisect(h, -4, 7, delta, epsilon) # Funkcja posiada 3 miejsca zerowe w podanym przedziale
    # Jedno z miejsc powinno zostać znalezione
    found = false
    for x in xs
        if abs(x - r) <= delta
            found = true
            break
        end
    end
    @assert found
    @assert abs(v) <= epsilon
    @assert err == 0

end

# Funkcja przeprowadzająca testy na funkcji używającej metody Newtona
function test_solve_Newton()

    pf(x) = -1
    pg(x) = 1
    ph(x) = 3*x^2 -7

    # Test 1
    # Dla funkcji liniowej z a>0 znajdziemy miejsce zerowe po pierwszej iteracji niezależnie od x0
    (r, v, it, err) = SolvingFunctions.solve_Newton(g, pg , 10000, delta, epsilon, 2)
    @assert abs(r - pi) <= delta
    @assert abs(v) <= epsilon
    @assert it == 1
    @assert err == 0

    # Test 2
    (r, v, it, err) = SolvingFunctions.solve_Newton(f, pf , -2, delta, epsilon, 20)
    @assert abs(r + 3) <= delta
    @assert abs(v) <= epsilon
    @assert it < 20
    @assert err == 0

    # Test 3
    xs = [-3, 1, 2]
    (r, v, it, err) = SolvingFunctions.solve_Newton(h, ph , 0, delta, epsilon, 20)
    found = false
    for x in xs
        if abs(x - r) <= delta
            found = true
            break
        end
    end
    @assert found
    @assert abs(v) <= epsilon
    @assert it < 20
    @assert err == 0

    # Test 4
    # Źle podane x0 prowadzi do wydłużenia obliczeń i przekroczenia maksymalnej liczby iteracji
    (r, v, it, err) = SolvingFunctions.solve_Newton(h, ph, 123232, delta, epsilon, 20)
    @assert it == 20
    @assert err == 1

    # Test 5
    # Wywołanie z pochodną bliską zeru wywoła Błąd
    fake_ph(x) = 1e-9 
    (r, v, it, err) = SolvingFunctions.solve_Newton(h, fake_ph, 123232, delta, epsilon, 20)
    @assert err == 2

end



function test_solve_secant()
     # Test 1
    (r, v, it, err) = SolvingFunctions.solve_secant(f , -4, -2, delta, epsilon, 20)
    @assert abs(r + 3) <= delta
    @assert abs(v) <= epsilon
    @assert it < 20
    @assert err == 0

    # Test 2
    (r, v, it, err) = SolvingFunctions.solve_secant(g , 1, 4, delta, epsilon, 20)
    @assert abs(r - pi) <= delta
    @assert abs(v) <= epsilon
    @assert it < 20
    @assert err == 0

    # Test 3
    xs = [-3, 1, 2]
    (r, v, it, err) = SolvingFunctions.solve_secant(h , -4, 3, delta, epsilon, 20)
    found = false
    for x in xs
        if abs(x - r) <= delta
            found = true
            break
        end
    end
    @assert found
    @assert abs(v) <= epsilon
    @assert it < 20
    @assert err == 0
    
    # Test 4
    # Źle podane x0 i x1 prowadzi do wydłużenia obliczeń i przekroczenia maksymalnej liczby iteracji
    (r, v, it, err) = SolvingFunctions.solve_secant(h, 1313, 100, delta, epsilon, 20)
    @assert it == 20
    @assert err == 1

end


test_solve_bisect()
println("Testy solve_bisect zakończone pozytywnie.")
test_solve_Newton()
println("Testy solve_Newton zakończone pozytywnie.")
test_solve_secant()
println("Testy solve_secant zakończone pozytywnie.")