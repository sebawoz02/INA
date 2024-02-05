# Sebastian Woźniak 268491
# Moduł zawierający funkcje do rozwiązywania układu Ax = b metodą eliminacji Gaussa oraz
# z wykorzystaniem rozkładu LU macierzy A.


module l5_solver
    import LinearAlgebra.norm
    export read_matrix, read_vector, gauss_elimination, gauss_elimination_LU, solve_LU, get_b_error, solve_and_save, solve_and_save_2
    include("mymatrix.jl")

    
    # Funkcja odczytujacy z pliku wektor prawych stron b.
    #
    # @param input_file - lokalizacja pliku tekstowego zawierającego wektor b 
    #                     zapisany w określonym formacie.
    # @return n - rozmiar wektora, b - wektor prawych stron 
    function read_vector(input_file)
        open(input_file) do fd
            in_data = readline(fd)

            n = parse(Int64, in_data)
            b = Vector{Float64}(zeros(n))
            
            idx = 1
            while !eof(fd)
                in_data = readline(fd)
                b[idx] = parse(Float64, in_data)
                idx += 1
            end # while !eof

            return n, b
        end
    end # read_vector


    #  Funkcja rozwiązującą układ Ax = b metodą eliminacji Gaussa dla dwóch wariantów:
    #  0. bez wyboru elementu głównego,
    #  1. z częściowym wyborem elementu głównego.
    #  Biorąc pod uwagę specyfikacje macierzy A oraz stałą wartość l algorytm wyznacznie
    #  macierzy schodkowej ma złożonośc obliczeniową O(n)
    #
    # @param A - macierz A zapisana w strukturze MyMatrix
    # @param b - wektor prawych stron
    # @param variant - numer wariantu 0/1
    # @return rozwiązanie układu Ax = b
    function gauss_elimination(A::MyMatrix, b::Vector{Float64}, variant::Bool)
        Ap = deepcopy(A)
        bp = copy(b)
        n = A.n

        if (variant)
            l2 = 2*A.l
        else
            l2 = A.l
        end

        # Wyznacznie macierzy trójkątnej A' oraz odpowiadającego wektora prawych stron b'
        # iteracja po kolumnach
        for k in 1:(A.n - 1)

            # Wybranie elementu głownego
            if (variant)
                main_elem = k
                # Wyznaczenie największej wartości
                for i in (k+1):min(A.n, k + A.l)
                    if (abs(mymatrix_get(Ap, i, k)) > abs(mymatrix_get(Ap, main_elem, k)))
                        main_elem = i
                    end
                end
                if main_elem != k
                    mymatrix_swap_rows(Ap, k, main_elem)
                    bp[k], bp[main_elem] = bp[main_elem], bp[k]
                end
            end

            # Iteracja po wierszach
            for i in (k+1):min(n, k + A.l)
                l = mymatrix_get(Ap, i, k)/mymatrix_get(Ap, k, k)
                
                mymatrix_set(Ap, i, k, zero(Float64))
                # Odejmowanie wiersza od wiersza
                for j in (k+1):min(n, k + l2)
                    mymatrix_set(Ap, i, j, mymatrix_get(Ap, i, j) - l*mymatrix_get(Ap, k, j))
                end

                bp[i] = bp[i] - l*bp[k]

            end # for i
        end # for k
        
        # Obliczenie x
        x = Vector{Float64}(undef, n)
        x[n] = bp[n]/mymatrix_get(Ap, n, n)
        for i in (n-1):-1:1
            s = zero(Float64)
            for j in i+1:min(n, i+2*Ap.l)
                s = s + mymatrix_get(Ap, i, j)*x[j]
            end
            x[i] = (bp[i] - s)/mymatrix_get(Ap, i, i)
        end
        return x
    end # gauss_elimination


    #  Funkcja wyznaczająca rozkład LU macierzy A:
    #  0. bez wyboru elementu głównego,
    #  1. z częściowym wyborem elementu głównego.
    #
    # @param A - macierz A zapisana w strukturze MyMatrix
    # @param b - wektor prawych stron
    # @param variant - numer wariantu 0/1
    # @return rozwiązanie układu rozkład LU macierzy A, odpowiadający wektor b
    function gauss_elimination_LU(A::MyMatrix, b::Vector{Float64}, variant::Bool)
        Ap = deepcopy(A)
        bp = copy(b)
        n = A.n

        if (variant)
            l2 = 2*A.l
        else
            l2 = A.l
        end

        # Wyznaczanie rozkładu LU
        for k in 1:(n-1)
            # Wybranie elementu głownego
            if (variant)
                main_elem = k
                # Wyznaczenie największej wartości
                for i in (k+1):min(n, k + A.l)
                    if (abs(mymatrix_get(Ap, i, k)) > abs(mymatrix_get(Ap, main_elem, k)))
                        main_elem = i
                    end
                end
                if main_elem != k
                    mymatrix_swap_rows(Ap, k, main_elem)
                    bp[k], bp[main_elem] = bp[main_elem], bp[k]
                end
            end

            # Iteracja po wierszach
            for i in (k+1):min(n, k + A.l)
                multiplier = mymatrix_get(Ap, i, k)/mymatrix_get(Ap, k, k)
                # Przypisanie mnożnika
                mymatrix_set(Ap, i, k, multiplier)
                for j in (k+1):min(n, k + l2)
                    mymatrix_set(Ap, i, j, mymatrix_get(Ap, i, j) - multiplier*mymatrix_get(Ap, k, j))
                end
            end # for i
        end # for k

        return Ap, bp
    end # gauss_elimination_LU 


    # Funkcja rozwiązującą układ Ax = b wykorzystując rozkład LU macierzy A.
    #
    # @param LU - rozkład LU macierzy A
    # @param b - wektor prawych stron
    # @return rozwiązanie x
    function solve_LU(LU::MyMatrix, b::Vector{Float64})
        n = LU.n
        # Wyznaczenie z w równaniu Lz = b
        z = copy(b)
        for k in 1:(n-1)
            for i in (k+1):min(n, k + 2*LU.l)
                z[i] -= z[k]*mymatrix_get(LU, i, k)
            end
        end

        # Wyznacznie x z rówania Ux = z
        x = Vector{Float64}(undef, n)
        x[n] = z[n]/mymatrix_get(LU, n, n)
        for i in (n-1):-1:1
            s = zero(Float64)
            for j in (i+1):min(n, i+2*LU.l)
                s = s + mymatrix_get(LU, i, j)*x[j]
            end
            x[i] = (z[i] - s)/mymatrix_get(LU, i, i)
        end

        return x
    end # solve_LU


    # Oblicza błąd względny wartości b
    function get_b_error(A::MyMatrix, b::Vector{Float64}, x::Vector{Float64})
        # A * x
        b_calc = zeros(A.n)
        for i in 1:A.n
            for j in max(1, i - A.l):min(A.n, i + A.l)
                b_calc[i] += mymatrix_get(A, i, j)*x[j]
            end
        end

        return norm(b - b_calc, 2) / norm(b, 2)
    end # get_b_error

    
    # Zapisuje w pliku tekstowym pbliczone rozwiązanie xˆ, jeśli macierz A była czytana z pliku,
    # a wektor prawych stron b był obliczany b = Ax, gdzie x = (1,...,1)^T
    function solve_and_save(A_input_file, out_filename, use_LU::Bool, variant::Bool)
        A = read_matrix(A_input_file)
        x = [one(Float64) for _ in 1:A.n]
        b = zeros(A.n)
        for i in 1:A.n
            for j in max(1, i - A.l):min(A.n, i + A.l)
                b[i] += mymatrix_get(A, i, j)*x[j]
            end
        end

        if (use_LU)
            LU, b2 = gauss_elimination_LU(A, b, variant)
            calc_x = solve_LU(LU, b2)
        else
            calc_x = gauss_elimination(A, b, variant)
        end
        err_x = norm(calc_x - x, 2) / norm(x, 2)
        println("Błąd względny - $err_x")

        open(out_filename, "w+") do f
            write(f, string(err_x)*"\n")
            for i in 1:A.n
                write(f, string(calc_x[i])*"\n")
            end
        end 
    end #  solve_and_save

    # Odczytuje z pliku A oraz b i zapisuje w pliku tekstowym rozwiązanie Ax = b.
    function solve_and_save_2(A_input_file, b_input_file, out_filename, use_LU::Bool, variant::Bool)
        A::MyMatrix = read_matrix(A_input_file)
        _, b::Vector{Float64} = read_vector(b_input_file)

        if (use_LU)
            LU, b2 = gauss_elimination_LU(A, b, variant)
            x = solve_LU(LU, b2)
        else
            x = gauss_elimination(A, b, variant)
        end

        open(out_filename, "w+") do f
            for i in 1:A.n
                write(f, string(x[i])*"\n")
            end
        end 
    end # solve_and_save_2

end # l5_solver