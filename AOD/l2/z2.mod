# DATA
param n, integer, >= 2;		# liczba miast
set N, default {1..n};		# zbiór miast
set A within N cross N;		# połączenia między miastami
param time{(i,j) in A};		# macierz czasów przejazdu
param cost{(i,j) in A};		# macierz kosztów przejazdu
param i0, symbolic, in N, default 1;	# miasto startowe
param j0, symbolic, in N, != i0, default n;	# miasto końcowe
param T, integer, default 10000;	# limit czasowy

# VARIABLES

var X{(i, j) in A}, >= 0,  <= T, binary;	# 1 - jeżeli krawędź została wybrana, 0 - jeżeli krawędź nie została wybrana

# OBJECTIVE FUNCTION

minimize total_cost : sum{(i, j) in A} X[i,j]*cost[i,j];	# scieżka z i0 do j0 o najmniejszym koszcie

# CONSTRAINTS

s.t. total_time : sum {(i,j) in A} X[i,j] * time[i,j] <= T;
s.t. r{i in 1..n}: sum{(j,i) in A} X[j,i] + (if i = i0 then 1) =
                   sum{(i,j) in A} X[i,j] + (if i = j0 then 1);


solve;

display total_cost;
display total_time;

printf{(i, j) in A}"(%d, %d) - %d\n",i, j, X[i,j]; 


