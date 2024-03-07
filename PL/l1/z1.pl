ojciec(piotrek, andrzej).
ojciec(pawel, cezary).
matka(ania, cezary).
mezczyzna(piotrek).
kobieta(ania).
rodzic(ania, cezary).

jest_matka(X) :- matka(X,_), kobieta(X).
jest_ojcem(X) :- ojciec(X,_), mezczyzna(X).
jest_synem(X) :- mezczyzna(X), rodzic(_, X).
siostra(X, Y) :- kobieta(X), rodzenstwo(X, Y), X \= Y.
dziadek(X, Y) :- ojciec(X, Z), rodzic(Z, Y), X \= Y.
rodzenstwo(X, Y) :- rodzic(Z, X), rodzic(Z, Y), X \= Y.