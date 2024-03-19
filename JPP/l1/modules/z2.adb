package body Z2 is
    function Factorial_Iterative(N : UInt64) return UInt64 is
        Result : UInt64 := 1;
    begin
        for I in 2 .. N loop
            Result := Result * I;
        end loop;
        return Result;
    end Factorial_Iterative;


    function Factorial_Recursive(N : UInt64) return UInt64 is
    begin
        if N = 0 or N = 1 then
            return 1;
        else
            return N * Factorial_Recursive(N - 1);
        end if;
    end Factorial_Recursive;

    function GCD_Iterative(A, B : UInt64) return UInt64 is
        Temp : UInt64;
        B2 : UInt64;
        A2 : UInt64;
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

    function GCD_Recursive(A, B : UInt64) return UInt64 is
    begin
    if B = 0 then
        return A;
    else
        return GCD_Recursive(B, A mod B);
    end if;
    end GCD_Recursive;

    function Diophantine_Equation_Iterative(A, B, C: Int64) return Result is
        Res: Result;
    begin   
        Res.X := 0;
        Res.Y := 0;
        return Res;
    end Diophantine_Equation_Iterative;

    function Diophantine_Equation_Recursive(A, B, C: Int64) return Result is
        Res: Result;
    begin  
        Res.X := 33;
        Res.Y := 3;
        return Res;
    end Diophantine_Equation_Recursive;

end Z2;
