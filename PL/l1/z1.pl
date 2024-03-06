ojciec(imie_ojca, imie_dziecka).
matka(imie_matki, imie_dziecka).
mezczyzna(imie_ojca).
kobieta(imie_matki).
rodzic(imie_ojca, imie_dziecka).

jest_matka(X) := matka(X,_), kobieta(X).
jest_ojcem(X) := ojciec(X,_), mezczyzna(X).
jest_synem(X) := mezczyzna(X), (ojciec(_,X); matka(_,X)).
siostra(X, Y) := kobieta(X), rodzenstwo(X, Y), X \= Y.
dziadek(X, Y) := ojciec(X, Z), (ojciec(Z, Y), matka(Z, Y)), X \= Y.
rodzenstwo(X, Y) := rodzic(Z, X), rodzic(Z, Y), X \= Y.