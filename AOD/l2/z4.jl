using JuMP
using GLPK

# składowane kontenery
teren = [
	1 1 1 0 1 0
	0 0 0 1 0 0
	0 1 1 0 0 1
	1 0 0 1 0 1
	0 0 0 0 0 1 
]
# zasięg kamer	
k = 2
# Wysokosc N terenu i szerokosc M terenu
N = size(teren)[1]
M = size(teren)[2]

model = Model(GLPK.Optimizer)

# VARIABLES
@variable(model, kamery[1:N, 1:M], Bin)		# macierz kamery informuje czy w danej pozycji znajduje się kamera (1) lub nie (0)

# OBJECTIVE FUNCTION
@objective(model, Min, sum(kamery))		# minimalizacja liczby kamer

# CONSTRAINTS
@constraint(model, [n in 1:N, m in 1:M], kamery[n, m] + teren[n, m] <= 1)	# kamera i kontener nie mogą zajmować tego samego kwadratu
for n in 1:N, m in 1:M
	if teren[n, m] == 1
		vertical = max(1, n - k):min(N, n+k)
		horizontal = max(1, m-k):min(M, m+k)
		@constraint(model, (sum(kamery[vertical,m]) + sum(kamery[n, horizontal])) >=1)
	end
end

optimize!(model)

display(round.(Int, value.(kamery)))	# rozmieszczenie kamer
display(value.(teren))	#rozmieszczenie kontenerów
