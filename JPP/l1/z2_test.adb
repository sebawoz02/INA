with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Z2;

procedure Z2_Test is
N : Natural;
Result : Natural;
begin
    Put("Put natural number n: ");
    Get(N);
    
    Result := Z2.Factorial_Iterative(N);

    Put("Factorial iterative: ");
    Put(Result);
    New_Line;

    Put("Put natural number n: ");
    Get(N);
    
    Result := Z2.Factorial_Recursive(N);

    Put("Factorial recursive: ");
    Put(Result);
    New_Line;

    declare
        A: Natural := 84;
        B: Natural := 4;
        GCD : Natural;
    begin
        GCD := Z2.GCD_Iterative(A, B);
        Put("GCD for ");
        Put(A);
        Put(" and ");
        Put(B);
        Put(" (iterative): ");
        Put(GCD);
        New_Line;
    end;

    declare
        A: Natural := 84;
        B: Natural := 4;
        GCD : Natural;
    begin
        GCD := Z2.GCD_Iterative(A, B);
        Put("GCD for ");
        Put(A);
        Put(" and ");
        Put(B);
        Put(" (recursive): ");
        Put(GCD);
        New_Line;
    end;
end Z2_Test;