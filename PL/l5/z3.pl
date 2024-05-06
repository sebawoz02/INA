browse(Term) :-
	writeTerm(Term),
	readCmd(Cmd),
	execute(Cmd, Term, []).
	
% param1 - komenda
% T - term
% P - obecna ścieżka
execute('i', T, P) :-
	T =.. [_, Child | _],
	!,
	writeTerm(Child),
	readCmd(Cmd),
	append([T], P, Pn),
	execute(Cmd, Child, Pn).
execute('i', T, P) :-
	writeTerm(T),
	readCmd(Cmd),
	execute(Cmd, T, P).

execute('o', T, []) :-
	writeTerm(T).
execute('o', _, [Parent | P]) :-
	writeTerm(Parent),
	readCmd(Cmd),
	execute(Cmd, Parent, P).

execute('n', T, []) :-
	writeTerm(T),
	readCmd(Cmd),
	execute(Cmd, T, []).
execute('n', T, [Parent | P]) :-
	Parent =.. [_ | Child],
	append(_, [T | []], Child),
	!,
	writeTerm(T),
	readCmd(Cmd),
	execute(Cmd, T, [Parent | P]).
execute('n', T, [Parent | P]) :-
	Parent =.. [_ | Child],
	append(_, [T | L2], Child),
	L2 = [Next | _],
	writeTerm(Next),
	readCmd(Cmd),
	execute(Cmd, Next, [Parent | P]).

execute('p', T, [Parent | P]) :-
	Parent =.. [_ | Child],
	Child = [T | _],
	!,
	writeTerm(T),
	readCmd(Cmd),
	execute(Cmd, T, [Parent | P]).
execute('p', T, [Parent | P]) :-
	Parent =.. [_ | Child],
	append(L1, [T | _], Child),
	append(_, [Prev], L1),
	writeTerm(Prev),
	readCmd(Cmd),
	execute(Cmd, Prev, [Parent | P]).

readCmd(Cmd) :-
	write('command: '),
	read(Cmd).
	
writeTerm(T) :-
	write(T),
	write('\n').