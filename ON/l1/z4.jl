# Sebastian Woźniak
# Program znajdujący najmniejsza liczbe spałniającą x * 1/x != 1

# Funkcja szuka liczby spełniającej warunek x * 1/x != 1 w podanym przedziale
# @param start - początek przedziału
# @param endd - koniec przedziału
# Note: przedział jest obustronnie otwarty
function find_smallest(start, endd)
    num = nextfloat(Float64(start))
    while num * (1.0/num) == 1.0
        num = nextfloat(num)    # sprawdź następną liczbe
    end
    if num >= endd  # nie znaleziono liczby w podanym przedziale
        println("Nie znaleziono liczby w podanym przedziale która spełni warunek x * 1/x != 1")
    else
        println("Najmniejszą liczbą w przedziale ($start;$endd) spełniająca warunek x * 1/x != 1 jest $num")
    end
end

find_smallest(Float64(1.0), Float64(2.0))
