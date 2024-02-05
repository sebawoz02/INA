# Sebastian Woźniak
# Program liczący wartości funkcji f(x) i g(x) w zadaniu 6.

function f(x)
    sqrt(Float64(x)^2 + one(Float64)) - one(Float64)
end

function g(x)
    Float64(x)^2 / (sqrt(Float64(x)^2 + one(Float64)) + one(Float64))
end

println("x\t f(x)\t g(x)\n")
for k in 1:20   # pierwsze 20 wartości k
    println("8^-$k\t", f(Float64(8.0)^-k), "\t ", g(Float64(8.0)^-k), 30)
end