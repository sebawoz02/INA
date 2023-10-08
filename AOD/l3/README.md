# Implementacja trzech wersji algorytmu Dijkstry do znajdowania najkrótszej ścieżki do każdego wierzchołka w grafie G z podanego wierzchołka wyjściowego.

Każdy algorytm jako dane wejściowe w postaci:

./dijkstra [-d] plik_z_grafem [-ss/-p2p] plik_z_problemami [-res] plik_na_czasy_wykonania

 - Format pliku z grafem:

p sp 4 5    //  shortest path problem , liczb wiecholków, liczba łuków.

a 1 3 45    //  krawędz z wierchołka 1 do 3 o wadze 45

- Format pliku z problemem.

p 2     // wierzcholek wyjsciowy 2

lub 

q 1 2   // droga z wierzcholka 1 do 2

- Kompilacja:

make - podstawowy algorytm dijkstry

make dial - algorytm Dial

make radix - algorytm dijkstry z wykorzystanie radix heap