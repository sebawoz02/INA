set ZMIANY;
set DZIELNICE;
param min_radiowozy{p in DZIELNICE, z in ZMIANY};
param max_radiowozy{p in DZIELNICE, z in ZMIANY};
param rad_zmiany{z in ZMIANY};
param rad_dzielnice{p in DZIELNICE};

# Variables
var rad_wyslane{DZIELNICE, ZMIANY}, >=0, integer;
/* radiowozy wysłane do dzielnic na zmianach */

# Objective function

minimize total_wyslane: sum{p in DZIELNICE}(sum{z in ZMIANY}rad_wyslane[p,z]);
/* suma wszystkich wysłanych radiowozów w ciągu doby */

# Constraints

s.t. min_wyslane_zmiana{z in ZMIANY, p in DZIELNICE}: min_radiowozy[p, z] <= rad_wyslane[p, z]; 
s.t. max_wyslane_zmiana{z in ZMIANY, p in DZIELNICE}: max_radiowozy[p, z] >= rad_wyslane[p, z]; 
s.t. min_dzielnice_dzien{p in DZIELNICE}:sum{z in ZMIANY}(rad_wyslane[p, z]) >= rad_dzielnice[p]; 
s.t. min_rad_zmiana{z in ZMIANY}:sum{p in DZIELNICE}(rad_wyslane[p, z]) >= rad_zmiany[z]; 

solve;

display total_wyslane;
printf{p in DZIELNICE, z in ZMIANY}: "Radiowozy wysłane do dzielnicy %s na zmianie %s : %d. \n", p, z, rad_wyslane[p, z];

end;

