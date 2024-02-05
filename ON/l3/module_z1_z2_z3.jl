# Sebastian Woźniak 268491
# Moduł z funkcjami rozwiązującymi równania na sposoby z zadań 1, 2, 3.

module SolvingFunctions

    # Funkcja rozwiązująca równanie f(x) = 0 metodą bisekcji
    # @param f - funkcja f(x)
    # @param a - początek przedziału
    # @param b - koniec przedziału
    # @param delta - dokładność przybliżenia x
    # @param epsilon - dokładność przybliżenia f(x)
    #
    # @return (r, v, it, err)
    #        r - przybliżenie rozwiązania f(x) = 0
    #        v - f(r)
    #        it - liczba wykonanych iteracji
    #        err - sygnalizacja błędu, 0 - brak błedu, 1 - funkcja nie zmienia znaku w przedziale [a, b]
    function solve_bisect(f, a, b, delta, epsilon)
        v = zero(Float64)
        left = f(a)
        right = f(b)
        e = b - a          # Błąd przybliżenia x
        r = 0
        it = 0           # Liczba iteracji

        if left * right >= 0 || a >= b    # Źle podany przedział
            return (r, v, it, 1)
        end

        while abs(f(r)) > epsilon && abs(e) > delta
            e /= 2
            r = a + e
            it += 1
            if f(a) * f(r) < 0
                b = r
            elseif f(r) == 0
                return (r, v, it, 0)
            else
                a = r
            end
        end

        return (r, v, it, 0)
    end


    # Funckja rozwiązująca równanie f(x) = 0 metodą Newtona
    # @param f - funkcja f(x)
    # @param pf - pochodna funkcji f(x)
    # @param x0 - przybliżenie początkowe
    # @param delta - dokładność przybliżenia x
    # @param epsilon - dokładność przybliżenia f(x)
    # @param maxit - maksymalna liczba iteracji
    #
    # @return (r, v, it, err)
    #        r - przybliżenie rozwiązania f(x) = 0
    #        v - f(r)
    #        it - liczba wykonanych iteracji
    #        err - sygnalizacja błędu, 0 - brak błedu, 1 - przekroczono maxit, 2 - pochodna bliska zeru
    function solve_Newton(f, pf, x0, delta, epsilon, maxit)
        r = x0
        it = 0
        v = f(r)

        if abs(pf(r)) < epsilon
            return (r, f(r), it, 2)
        end
    
        while (abs(v) > epsilon && abs(v / pf(r)) > delta) && it < maxit
            r = r - v / pf(r)
            v = f(r)
            it += 1
        end
    
        if it >= maxit
            return (r, v, it, 1)
        end
    
        return (r, v, it, 0)
    end


    # Funkcja rozwiązująca równanie f(x) = 0 metodą siecznych
    # @param f - funkcja f(x)
    # @param x0 - przyblizenie początkowe 1
    # @param x1 - przyblizenie początkowe 2
    # @param delta - dokładność przybliżenia x
    # @param epsilon - dokładność przybliżenia f(x)
    # @param maxit - maksymalna liczba iteracji
    #
    # @return (r, v, it, err)
    #        r - przybliżenie rozwiązania f(x) = 0
    #        v - f(r)
    #        it - liczba wykonanych iteracji
    #        err - sygnalizacja błędu, 0 - brak błedu, 1 - przekroczono maxit
    function solve_secant(f, x0, x1, delta, epsilon, maxit)
        r = x1
        v = f(r)
        it = 0
        err = 0

        while (abs(v) > epsilon && abs(x1-x0) > delta) && it < maxit
            r = x1 - f(x1) * (x1 - x0) / (f(x1) - f(x0))

            # Przesunięcie przybliżenia
            x0 = x1
            x1 = r
            v = f(r)
            it += 1
        end

        if it == maxit
            err = 1
        end

        return (r, v, it, err)
    end
end