# DATA
set MASZYNY;
set PRODUKTY;
param prod_params{p in PRODUKTY, {'cena','max_popyt','koszt_mat'}};
param maszyny_params{m in MASZYNY, {'koszt_h', 'dostepnosc'}};
param czas_prod{p in PRODUKTY, m in MASZYNY}, >0;

# VARIABLES
var wyprodukowane{p in PRODUKTY}, >=0, integer;

# OBJECTIVE FUNCTION		zysk ze sprzedaży  -  koszt materiałów	-  koszt pracy maszyn
maximize zysk: sum{p in PRODUKTY}(wyprodukowane[p]*prod_params[p, 'cena'] - wyprodukowane[p]*prod_params[p, 'koszt_mat'] 
					- sum{m in MASZYNY}(maszyny_params[m, 'koszt_h']*wyprodukowane[p]*czas_prod[p, m]/60)) ;
					
# CONSTRAINTS
s.t. dostepnosc_maszyn{m in MASZYNY}: sum{p in PRODUKTY}(wyprodukowane[p]*czas_prod[p, m]) <= maszyny_params[m, 'dostepnosc']; 
s.t. max_popyt{p in PRODUKTY}: wyprodukowane[p] <= prod_params[p, 'max_popyt'];

solve;

display zysk;

printf{p in PRODUKTY}: "Wyprodukowano %d kg produktu %s. \n", wyprodukowane[p], p; 

end;
