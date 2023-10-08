# Data

set LOTNISKA;
set FIRMY;
param CENY{l in LOTNISKA, f in FIRMY}, >=0;
param zapotrzebowanie{LOTNISKA};
param max_dostawa{FIRMY};

# Variables

var kupione{FIRMY, LOTNISKA}, >=0, integer;
/* Liczba galonów dostarczona z firm 1..m do lotnisk 1..n */

# Objective function

minimize cost: sum{f in FIRMY}( sum{ l in LOTNISKA } (kupione[f,l]*CENY[l,f]));
/* Koszt zakupu galonów paliwa na wszystkie lotniska. */

# Constraints

s.t. WyslaneNaLotnisko{l in LOTNISKA}: sum{f in FIRMY}(kupione[f, l]) = zapotrzebowanie[l];
/* Zapotrzebowanie na paliwo na każdym z lotnisk. */
s.t. WyslaneZFirmy{f in FIRMY}: sum{l in LOTNISKA}(kupione[f, l]) <= max_dostawa[f];
/* Maksymalne liczby galonów paliwa które mogą sprzedać firmy. */

                                                                                  
solve;
display cost;
display {f in FIRMY}(sum{l in LOTNISKA}(kupione[f,l]));
