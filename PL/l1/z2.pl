on(block1, block2).
on(block2, block3).

above(B1, B2) :- on(B1, B2).
above(B1, B2) :- on(B3, B2), above(B1, B3).