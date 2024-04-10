wariancja(L, D):-
	sum(L, S),
	length(L, N),
	wariancja(L, N, S, X),
	D is X/N.

sum([], 0).
sum([X | L], S):-
	sum(L, S1),
	S is X + S1.

wariancja([], _, _, 0).

% L [in] - lista
% N [in] - długosc listy
% S [in] - suma elementów listy
% V [in/out] - wariancja
wariancja([X | L], N, S, V):-
	wariancja(L, N, S, V1),
	V is ((X - S/N)*(X - S/N)) + V1.    % suma kwadratów różnic elementów ze srednią