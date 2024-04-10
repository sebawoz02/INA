max_sum(L, S):-
	max_sum(L, 0, 0, S).

% Jeżeli lista jest pusta to wynikiem jest M 
max_sum([], M, _, M).
% L - lista
% M - maksymalna suma
% T - akumulator
% S - wynik wyjściowy
max_sum([X | L], M, T, S):-
	Tmp is X + T,
	(Tmp < 0 -> max_sum(L, M, 0, S); (porównaj(Tmp, M, Y), max_sum(L, Y, Tmp, S)) ).

porównaj(M, X, X) :-
	X > M.
porównaj(M, X, M) :-
	X =< M.