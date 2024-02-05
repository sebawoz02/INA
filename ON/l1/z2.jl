# Sebastian Woźniak
# Program liczący epsilon maszynowy działaniem 3*(4/3-1)-1

eps16 = Float16(3)*(Float16(4)/Float16(3) - one(Float16)) - one(Float16)    # epsilon maszynowy dla Float16
println("Float16")
println("3 * ( 4/3 - 1) - 1 = ", eps16)
println("Wartość z eps(Float16): ", eps(Float16))

eps32 = Float32(3)*(Float32(4)/Float32(3) - one(Float32)) - one(Float32)    # epsilon maszynowy dla Float32
println("")
println("Float32")
println("3 * ( 4/3 - 1) - 1 = ", eps32)
println("Wartość z eps(Float32): ", eps(Float32))

eps64 = Float64(3)*(Float64(4)/Float64(3) - one(Float64)) - one(Float64)    # epsilon maszynowy dla Float32
println("")
println("Float64")
println("3 * ( 4/3 - 1) - 1 = ", eps64)
println("Wartość z eps(Float64): ", eps(Float64))