# Sebastian Woźniak
# Program liczący pochodną funkcji oraz błąd w obliczeniach

# Funkcja licząca pochodną funkcji f
# @param f      - Funkcja
# @param x0     - punkt pochodnej
function deriative(f, h, x0)
    return (Float64(f(x0 + h)) - Float64(f(x0)))/ Float64(h)
end

f(x) = sin(x) + cos(3x)
real_df(x) = Float64(cos(x)) - Float64(3.0)*Float64(sin(3x)) # prawdziwa pochodna funkcji f

real_val = real_df(one(Float64))    # prawdziwa wartośc pochodnej f'(1)
x0 = one(Float64)   # punkt w którym liczona będzie pochodna
"""
# h
println("h\t\t\tf_tilde\t\t\t|f_tilde - f'|\n")
for i in 0:54
    f_tilde = deriative(f, Float64(2.0)^-i, x0) # przybliżona wartość pochodnej w punkcie x0 = 1
    println("2^-i\t\tf_tilde\t\t", abs(f_tilde - real_val))
end
"""   
# 1 + h
println("h\t\t1+h\t\t\tf(1+h)\n")
for i in 0:54
    fh = f(Float64(2.0)^(-i)+ one(Float64))
    h1 = one(Float64) + Float64(2.0)^-i
    println("2^-$i\t\t$h1\t\t\t$fh")
end
