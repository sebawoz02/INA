with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Z2; use Z2;
with Interfaces.C; use Interfaces.C;
with Ada.Assertions; use Ada.Assertions;

procedure Z2_Test is
N : UInt64;
Res : UInt64;
begin
    N := 3;
    Res := Z2.Factorial_Iterative(N);
    Assert(Res = 6);
    
    Res := Z2.Factorial_Recursive(N);
    Assert(Res = 6);

    N := 20;
    Res := Factorial_Iterative(N);
    Assert(Res = 2432902008176640000);

    declare
        A: UInt64 := 84;
        B: UInt64 := 4;
        GCD : UInt64;
    begin
        GCD := Z2.GCD_Iterative(A, B);
        Assert(GCD = 4);
        GCD := Z2.GCD_Iterative(A, B);
        Assert(GCD = 4);
    end;

    declare
        A: Int64 := 3;
        B: Int64 := 25;
        C: Int64 := 1;
        Res2: Result;
    begin
        Res2 := Diophantine_Equation_Iterative(A, B, C);
        Assert(Res2.X = -8);
        Assert(Res2.Y = 1);

        Res2 := Diophantine_Equation_Recursive(A, B, C);
        Assert(Res2.X = -8);
        Assert(Res2.Y = 1);
    end;
end Z2_Test;