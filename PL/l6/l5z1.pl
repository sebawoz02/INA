keyW(read).
keyW(write).
keyW(if).
keyW(then).
keyW(else).
keyW(fi).
keyW(while).
keyW(do).
keyW(od).
keyW(and).
keyW(or).
keyW(mod).

sepp(';').
sepp('+').
sepp('-').
sepp('*').
sepp('/').
sepp('(').
sepp(')').
sepp('<').
sepp('>').
sepp('=<').
sepp('>=').
sepp(':=').
sepp('=').
sepp('/=').

skip(' ').
skip('\t').
skip('\n').
skip('\r').

identifier(X) :- 
    atom_chars(X, List), 	
    \+ (member(X, List),
    \+ char_type(X, upper)).

checkToken(L, end_of_file, L).

checkToken(L, X, L2) :- 
    atom_chars(X, Chars),
    last(Chars, ';') -> 
        (select(';', Chars, NL), !,
        atom_chars(T, NL),
        addToken(L, T, L3),
        append(L3, [sep(';')], L2));
        addToken(L, X, L2).

addToken(L, end_of_file, L).

addToken(L, X, L2) :-  	
    (keyW(X) -> append(L, [key(X)], L2);                                % key
    (sepp(X) -> append(L, [sep(X)], L2);                                % sep
    (atom_number(X, N), integer(N), N >= 0) -> append(L, [int(N)], L2); % int
    (identifier(X) -> append(L, [id(X)], L2);                             % id
    L2 = L))).

% EOF
readNext(end_of_file, L, L) :- !.

readNext(C, X, X2) :- 	
    skip(C), !,
    get_char(Cn),
    readNext(Cn, X, X2).

% wczytaj słowo i przypasuj token
% C - ostatnio wczytany znak
% L - obecna lista tokenów
% X - końcowa lista tokenów
readNext(C, L, X) :- 	
    readWord(C, Cn, '', H),
    checkToken(L, H, L2),
    readNext(Cn, L2, X).

% EOF, cięcie 
readWord(end_of_file, end_of_file, Nf, Nf) :- !.

% koniec słowa, cięcie
readWord(Cn, Cn, Nf, Nf) :- 
    skip(Cn), !.

% dodaj znak do słowa i czytaj dalej
% Cc - aktualny znak czytanego słowa,
% Cn - następny znak, który ma zostać przeczytany,
% N - początkowe słowo,
% Nf - końcowe wczytane słowo.
readWord(Cc, Cn, N, Nf) :- 	
    atom_concat(N, Cc, N2),
    get_char(C),
    readWord(C, Cn, N2, Nf).

tokenize(X) :- 	
    get_char(C),
    readNext(C, [], X).

scanner(FD, Tokens) :- 	
    current_input(Input),
    set_input(FD),
    tokenize(Tokens),
    set_input(Input).