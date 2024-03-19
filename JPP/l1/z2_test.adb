with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Z2; use Z2;
with Interfaces.C; use Interfaces.C;
with Ada.Assertions; use Ada.Assertions;

procedure Z2_Test is
N : UInt64;
Result : UInt64;
begin
    N := 3;
    Result := Z2.Factorial_Iterative(N);
    Assert(Result = 6);
    
    Result := Z2.Factorial_Recursive(N);
    Assert(Result = 6);

    declare
        A: UInt64 := 84;
        B: UInt64 := 4;
        GCD : UInt64;
    begin
        GCD := Z2.GCD_Iterative(A, B);
        Assert(GCD = 4);
    end;

    declare
        A: UInt64 := 84;
        B: UInt64 := 4;
        GCD : UInt64;
    begin
        GCD := Z2.GCD_Iterative(A, B);
        Assert(GCD = 4);
    end;
end Z2_Test;