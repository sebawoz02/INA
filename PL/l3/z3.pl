even_permutation([], []).
even_permutation([X | Xs], Ys) :-
    even_permutation(Xs, Z),
    add_to_odd(Z, X, Ys).
even_permutation([X | Xs], Ys) :-
    odd_permutation(Xs, Z),
    add_to_even(Z, X, Ys).

odd_permutation([X | Xs], Ys) :-
    odd_permutation(Xs, Z),
    add_to_odd(Z, X, Ys).
odd_permutation([X | Xs], Ys) :-
    even_permutation(Xs, Z),
    add_to_even(Z, X, Ys).

add_to_odd(Ys, X, [X | Ys]).
add_to_odd([A, B | L], X, [A, B | Ys]) :-
    add_to_odd(L, X, Ys).

add_to_even([X | Xs], H, [X | Ys]) :-
    add_to_odd(Xs, H, Ys).
