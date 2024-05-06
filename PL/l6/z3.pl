p([]) --> [].
p([X|Xs]) --> [X], p(Xs).

# a^n b^n, n >= 0
p1 --> [].
p1 --> [ a ], p1, [ b ].

# a^n b^n c^n, n >= 0
p2 --> counter(N, a), counter(N, b), counter(N, c).   

# a^n b^fib(n), n >= 0
p3 --> counter(N, a), {fib(N, F), !}, counter(F, b).
fib(0, 0).
fib(1, 1).
fib(N, F) :- A is N - 1, B is N - 2, fib(A, F1), fib(B, F2), F is F1 + F2.

counter(N, C) --> [ C ], counter(Np, C), {N is Np + 1}.
counter(0, _) --> [].