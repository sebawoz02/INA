na_lewo(X, Y, [X,Y|_]).
na_lewo(X, Y, [_|Rest]):- na_lewo(X, Y, Rest).

obok(X, Y, Domy):- na_lewo(X, Y, Domy).
obok(X, Y, Domy):- na_lewo(Y, X, Domy).

rybki(Kto):-
	Domy = [[1,_,_,_,_,_], [2,_,_,_,_,_], [3,_,_,_,_,_], [4,_,_,_,_,_], [5,_,_,_,_,_]],
	member([1,_,norweg,_,_,_], Domy),
	member([_,czerwony,anglik,_,_,_], Domy),
	na_lewo([_,zielony,_,_,_,_],[_,bialy,_,_,_,_], Domy),
	member([_,_,dunczyk,_,herbata,_], Domy),
	obok([_,_,_,_,_,light],[_,_,_,koty,_,_], Domy),
	member([_,zolty,_,_,_,cygara], Domy),
	member([_,_,niemiec,_,_,fajka],Domy),
	member([3,_,_,_,mleko,_], Domy),
	obok([_,_,_,_,_,light],[_,_,_,_,wode,_], Domy),
	member([_,_,_,ptaki,_,bez], Domy),
	member([_,_,szwed,psy,_,_], Domy),
	obok([_,_,norweg,_,_,_],[_,niebieski,_,_,_,_], Domy),
	obok([_,_,_,konie,_,_],[_,zolty,_,_,_,_], Domy),
	member([_,_,_,_,piwo,mentol], Domy),
	member([_,zielony,_,_,kawa,_], Domy),
	member([_,_,Kto,rybki,_,_], Domy).