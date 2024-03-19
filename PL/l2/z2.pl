jednokrotnie(X, L) :-
    select(X, L, B),
    \+ member(X, B).

dwukrotnie(X, L) :-
    select(X, L, B),
    jednokrotnie(X, B).