arc(a, b).
arc(b, a).
arc(b, c).
arc(c, d).

osiągalny(X, X, _).

osiągalny(X, Y, Visited) :-
    arc(X, Z),
    \+ member(Z, Visited),  % Z nie został jeszcze odwiedzony
    osiągalny(Z, Y, [X | Visited]). % dodaj X do odwiedzonych i sprawdza od Z do Y

osiągalny(X, Y) :-
    osiągalny(X, Y, []).