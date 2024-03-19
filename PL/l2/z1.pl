środkowy(L, X) :-
    append(L1, [X|L2], L), % L jest złączeniem dwóch podlist L1 i L2 z elementem X po środku
    length(L1, Len),    % Dlugość L1 to Len
    length(L2, Len).    % Dlugość L2 to Len