:- consult('z1.pl').
:- consult('l5z1.pl')
:- consult('interpreter.pl')

wykonaj(X) :-
    open(X, read, FD),
    scanner(FD, Tokens),
    close(FD),
    phrase(program(Instr), Tokens),
    interpreter(Instr).
