is_divisible(X, Y) :-
    X mod Y =:= 0.

is_divisible(X, Y) :-
    N is Y + 1,
    N*N =< X,
    is_divisible(X, N).

is_prime(X) :-
    X > 1,
    \+ is_divisible(X, 2).

prime(LO, HI, N) :-
    between(LO, HI, N),
    is_prime(N).