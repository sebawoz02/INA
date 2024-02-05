# Sebastian Woźniak
# Program sprawdzający równomierne rozmieszczenie względem podanej delty

# Funkcja sprawdza czy 1000 kolejnych liczb jest równomiernie rozmieszczonych
# zgodnie z podaną deltą
# @param start - pierwsza liczba z 1000 kolejnych
# @param delta - oczekiwana delta wobec której sprawdzane będzie rozmieszczenie
function check_distr_between(start, delta)
    # jeżeli delta jest dodatnia sprawdzamy rosnąco
    if(delta > zero(Float64))
        func = nextfloat
    else    # w przeciwnym przypadku malejąco
        func = prevfloat
    end
    num = Float64(start)    # liczba która będzie powiększana funkcją nextfloat lub prevfloat
    for k in 0:1000
        x = Float64(start) + Float64(k) * Float64(delta)    # oblicz następną oczekiwaną liczbe ze wzoru start + k*delta
        bitstr_x = bitstring(x)
        bitstr_num = bitstring(num)
        if bitstr_num != bitstr_x  # Liczba oczekiwana i rzeczywista kolejna liczba są różne
            println("[Nierównomierne rozmieszczenie]")
            println("liczba $num - $bitstr_num nie jest możliwa do zapisania jako 1 + k*2^-52")
            println("Wykryta delta: ", abs(num - func(num)))
            return
        end
        num = func(Float64(num))    # następna liczba
    end
    println("Sukces. Rozmieszczenie jest równomiernie z krokiem $delta")
end

delta = Float64(2.0)^Float64(-52)   # 2^-52
# Sprawdzenie w [1, 2]
println("[1, 2]")
check_distr_between(Float64(1.0), delta)
check_distr_between(Float64(2.0), -delta)

# Sprawdzenie w [1/2, 1]
println("\n[1/2, 1]")
check_distr_between(Float64(1.0)/Float64(2.0), delta)
check_distr_between(one(Float64), -delta)

# Sprawdzenie w [2, 4]
println("\n[2, 4]")
check_distr_between(Float64(2.0), delta)
check_distr_between(Float64(3.0), delta)
check_distr_between(Float64(4.0), -delta)