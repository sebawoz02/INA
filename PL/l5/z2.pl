board(L) :-
    length(L, N),
    draw(L, 1, N).

draw(_, Row, N) :-
    Row > N,
    break(N),
    nl,
    !.

% L - lista z predykatu hetmany
% R - numer obecnego rzędu
% N - liczba wszystkich rzędów
draw(L, R, N) :-
    break(N),
    Rev is N + 1 - R,   % odwrócona numeracja
    row(Rev, 1, L, N),
    row(Rev, 1, L, N),
    Rn is R + 1,
    draw(L, Rn, N),
    !.

% koniec rzędu
row(_, Col, _, N) :-
    N + 1 =:= Col,
    write('|'),
    nl,
    !.

% zajęte pole
row(Row, Col, Tab, N) :-
    nth1(Col, Tab, Row),
    (   (((Row + N + 1) mod 2 =:= 0, Col mod 2 =:= 1)
        ;((Row + N + 1) mod 2 =:= 1, Col mod 2 =:= 0)
        ) -> write('|:###:') ; write('| ### ')
    ),
    Col1 is Col + 1,
    row(Row, Col1, Tab,N),
    !.

% wolne pole
row(Row, Col, Tab, N) :-
    (   (((Row + N + 1) mod 2 =:= 0, Col mod 2 =:= 1)
        ;((Row + N + 1) mod 2 =:= 1, Col mod 2 =:= 0)
        ) -> write('|:::::') ; write('|     ')
    ),
    Col1 is Col + 1,
    row(Row, Col1, Tab,N),
    !.

% --- predykaty do przerwy między rzędami ---
break(0) :-
    write('+'),
    nl,
    !.

break(N) :-
    write('+-----'),
    N1 is N - 1,
    break(N1).
% -----------------

% -- predykat hetmany z listy 4 --
hetmany(N, P) :-
    numlist(1, N, L),
    permutation(L,P),
    dobra(P).

dobra(P) :-
    \+ zla(P).

zla(P) :-
    append(_, [Wi | L1], P),
    append(L2, [Wj | _ ], L1),
    length(L2, K),
    abs(Wi - Wj) =:= K + 1.
% ------------------