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


    function Extended_GCD_Iterative(A, B: Int64; X, Y: out Int64) return Int64 is
        Q, R: Int64;
        X1, Y1, X0, Y0: Int64;
        nX, nY, A_copy, B_copy: Int64;
    begin
        X0 := 1; X1 := 0; Y0 := 0; Y1 := 1; A_copy:= A; B_copy := B;
        while B_copy /= 0 loop
            Q := A_copy/B_copy;
            R := A_copy rem B_copy;
            A_copy := B_copy;
            B_copy := R;

            nX := X0 - Q*X1;
            nY := Y0 - Q*Y1;

            X0 := X1; Y0 := Y1; X1 := nX; Y1 := nY;
        end loop; 
        X := X0;
        Y := Y0;
        return A_copy;
    end Extended_GCD_Iterative;

    function Extended_GCD_Recursive(A, B: Int64; X, Y: out Int64) return Int64 is
        function Extended_GCD_Recursive_Helper(A, B, X0, Y0, X1, Y1: Int64) return Int64 is
            Q, R, nX, nY: Int64;
        begin
            if B = 0 then
                X := X0;
                Y := Y0;
                return A;
            else
                Q := A / B;
                R := A rem B;
                nX := X0 - Q * X1;
                nY := Y0 - Q * Y1;
                return Extended_GCD_Recursive_Helper(B, R, X1, Y1, nX, nY);
            end if;
        end Extended_GCD_Recursive_Helper;
    begin

    return Extended_GCD_Recursive_Helper(A, B, 1, 0, 0, 1);
    end Extended_GCD_Recursive;

    function Diophantine_Equation_Iterative(A, B, C: Int64) return Result is
        Res: Result;
        Gcd: Int64;
    begin   
        Gcd := Extended_GCD_Iterative(A, B, Res.X, Res.Y);
        if C mod Gcd /= 0 then
            -- No solution exists
            Res.X := 0;
            Res.Y := 0;
            return Res;
        end if;

        Res.X := Res.X * (C/Gcd);
        Res.Y := Res.Y * (C/Gcd);
        return Res;
    end Diophantine_Equation_Iterative;

    function Diophantine_Equation_Recursive(A, B, C: Int64) return Result is
        Res: Result;
        Gcd: Int64;
    begin  
        Gcd := Extended_GCD_Recursive(A, B, Res.X, Res.Y);
        if C mod Gcd /= 0 then
            -- No solution exists
            Res.X := 0;
            Res.Y := 0;
            return Res;
        end if;

        Res.X := Res.X * (C/Gcd);
        Res.Y := Res.Y * (C/Gcd);
        return Res;
    end Diophantine_Equation_Recursive;

end Z2;
