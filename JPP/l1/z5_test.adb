with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Z5; use Z5;
with Interfaces.C; use Interfaces.C;
with Ada.Assertions; use Ada.Assertions;

procedure Z5_Test is
N : UInt64;
Result : UInt64;
begin
    N := 3;
    Result := Factorial_Iterative(N);
    Assert(Result = 6);
    
    Result := Factorial_Recursive(N);
    Assert(Result = 6);

    declare
        A: UInt64 := 84;
        B: UInt64 := 4;
        GCD : UInt64;
    begin
        GCD := GCD_Iterative(A, B);
        Assert(GCD = 4);
    end;

    declare
        A: UInt64 := 84;
        B: UInt64 := 4;
        GCD : UInt64;
    begin
        GCD := GCD_Iterative(A, B);
        Assert(GCD = 4);
    end;
end Z5_Test;