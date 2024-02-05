# Sebastian Woźniak 268491
# API do struktury efektywanie przechowującej macierz o specyficznej postaci.

# Specjalna struktura macierzy pamiętająca tylko elementy niezerowe
mutable struct MyMatrix
    # counter::Int64 count operations on vectors A, B, C

    n::Int64    # rozmiar macierzy
    l::Int64    # rozmiar macierzy Ak, Bk, Ck
    v::Int64    # liczba bloków wewnętrznych |A| = v, |B| = v - 1, |C| = v - 1
    A::Vector{Matrix{Float64}}  # Wektor bloków A
    B::Vector{Matrix{Float64}}  # Wektor bloków B
    C::Vector{Matrix{Float64}}  # Wektor bloków C
end


# Funkcja odczytująca z pliku macierz i zapisująca ją w strukturze.
#
# @param input_file - lokalizacja pliku tekstowego zawierającego macierz A 
#                     zapisaną w określonym formacie. 
# @return structure MyMatrix
function read_matrix(input_file)
    open(input_file) do fd
        in_data = split(readline(fd))

        n::Int64 = parse(Int64, in_data[1]) # rozmiar macierzy A
        l::Int64 = parse(Int64, in_data[2]) # rozmiar macierzy Ak, Bk, Ck
        v::Int64 = n / l
        A = Vector{Matrix{Float64}}(undef, v)
        B = Vector{Matrix{Float64}}(undef, v - 1)
        C = Vector{Matrix{Float64}}(undef, v - 1)

        m = MyMatrix(n, l, v, A, B, C)

        for i in 1:(v-1)
            m.A[i] = zeros(l, l)
            m.B[i] = zeros(l, l)
            m.C[i] = zeros(l, l)
        end
        m.A[v] = zeros(l, l)

        while !eof(fd)
            in_data = split(readline(fd))
            i::Int64 = parse(Int64, in_data[1])
            j::Int64 = parse(Int64, in_data[2])
            val::Float64 = parse(Float64, in_data[3])

            iv, jv, il, jl = priv_mymatrix_get_idx(m, i, j)

            if iv == jv
                m.A[iv][il, jl] = val
            elseif iv > jv
                m.B[jv][il, jl] = val
            elseif iv < jv
                m.C[iv][il, jl] = val
            end

        end # while !eof

        return m
    end
end # read_matrix


# Funkcja zwracająca index bloku Ak|Bk|Ck w strukturze do którego należy element
# macierzy m pod indexem i,j oraz index elementu wewnątrz bloku.
#
# @param m - struktura macierzy MyMatrix
# @param i, j - kolumna oraz rząd w macierzy nxn
# @return - up^
function priv_mymatrix_get_idx(m::MyMatrix, i::Int64, j::Int64)
    iv::Int64 = floor(Int64, (i - 1) / m.l) + 1
    jv::Int64 = floor(Int64, (j - 1) / m.l) + 1

    il::Int64 = (i - 1) % m.l + 1
    jl::Int64 = (j - 1) % m.l + 1
    return iv, jv, il, jl
end


# Zwraca liczbe znajdującą się pod i,j w macierzy zapisanej w strukturze MyMatrix.
#
# @param m - struktura macierzy MyMatrix
# @param i, j - kolumna oraz rząd w macierzy nxn
# @return - up^
function mymatrix_get(m::MyMatrix, i::Int64, j::Int64)
    iv, jv, il, jl = priv_mymatrix_get_idx(m, i, j)
    # m.counter += 1

    if iv == jv
        return m.A[iv][il, jl]
    elseif iv == jv + 1
        return m.B[jv][il, jl]
    elseif iv + 1 == jv
        return m.C[iv][il, jl]
    else
        return zero(Float64)
    end
    
end # mymatrix_get


# Ustawia liczbę pod i,j w macierzy zapisanej w strukturze MyMatrix.
#
# @param m - struktura macierzy MyMatrix
# @param i, j - kolumna oraz rząd w macierzy nxn
# @param val - nowa wartość
# @return - None
function mymatrix_set(m::MyMatrix, i::Int64, j::Int64, val::Float64)
    iv, jv, il, jl = priv_mymatrix_get_idx(m, i, j)
    # m.counter += 1

    if iv == jv
        m.A[iv][il, jl] = val
    elseif iv == jv + 1
        m.B[jv][il, jl] = val
    elseif iv + 1 == jv
        m.C[iv][il, jl] = val
    end
    
end # mymatrix_set


# Zamienia miejscami rzędy x i y
#
# @param m - macierz zapisana w strukturze MyMatrix
# @param x - numer pierwszego rzędu
# @param y - numer drugiego rzędu
function mymatrix_swap_rows(m::MyMatrix, x::Int64, y::Int64)
    if x > y
        x, y = y, x
    end
    xv, yv, xl, yl = priv_mymatrix_get_idx(m, x, y)
    for j in 1:m.l
        if xv == yv
            # m.counter += 2
            m.A[xv][xl, j], m.A[yv][yl, j] = m.A[yv][yl, j], m.A[xv][xl, j]
            if xv > 1
                # m.counter += 2
                m.B[xv - 1][xl, j], m.B[yv - 1][yl, j] = m.B[yv - 1][yl, j], m.B[xv - 1][xl, j]
            end
            if yv < m.v
                # m.counter += 2
                m.C[xv][xl, j], m.C[yv][yl, j] = m.C[yv][yl, j], m.C[xv][xl, j]
            end

        elseif yv == xv + 1
            # m.counter += 2
            m.A[xv][xl, j], m.B[yv - 1][yl, j] = m.B[yv - 1][yl, j], m.A[xv][xl, j]
            if xv > 1
                # m.counter += 1
                m.B[xv - 1][xl, j] = zero(Float64)
            end
            if yv < m.v
                # m.counter += 2
                m.C[xv][xl, j], m.A[yv][yl, j] = m.A[yv][yl, j], m.C[xv][xl, j]
            end
        end
    end # for j

end # mymatrix_swap_rows
