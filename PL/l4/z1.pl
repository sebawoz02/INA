wyrażenie(L, X, Y):-
	dzialanie(L, Y), % wygeneruj dzialanie
	W is Y,
	X = W.  % sprawdz poprawnosc

dzialanie([X], X).

dzialanie(L, W):-
	append(X, Y, L),
	X \= [], Y \= [],
	dzialanie(X, Z1),
	dzialanie(Y, Z2),
	(
	W = Z1 + Z2;
	W = Z1 - Z2;
	W = Z1 * Z2;
	(Z2 =\= 0 ,W = Z1/Z2)
	).