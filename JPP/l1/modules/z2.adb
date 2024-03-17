package body Z2 is
    function Factorial_Iterative(N : Natural) return Natural is
        Result : Natural := 1;
    begin
        for I in 2 .. N loop
            Result := Result * I;
        end loop;
        return Result;
    end Factorial_Iterative;


    function Factorial_Recursive(N : Natural) return Natural is
    begin
        if N = 0 or N = 1 then
            return 1;
        else
            return N * Factorial_Recursive(N - 1);
        end if;
    end Factorial_Recursive;

    function GCD_Iterative(A, B : Natural) return Natural is
        Temp : Natural;
        B2 : Natural;
        A2 : Natural;
    begin
        B2 := B;
        A2 := A;
        while B2 /= 0 loop
            Temp := B2;
            B2 := A2 mod B2;
            A2 := Temp;
        end loop;
        return A2;
    end GCD_Iterative;

    function GCD_Recursive(A, B : Natural) return Natural is
    begin
    if B = 0 then
        return A;
    else
        return GCD_Recursive(B, A mod B);
    end if;
    end GCD_Recursive;
end Z2;
