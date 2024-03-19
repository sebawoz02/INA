with Interfaces.C; use Interfaces.C;

package Z5 is
    subtype Int64 is Interfaces.C.long;
    subtype UInt64 is Interfaces.C.unsigned_long;

    type Result is record
        X : Int64;
        Y : Int64;
    end record;
    pragma Convention (C, Result);

    function Factorial_Iterative(N : UInt64) return UInt64;
    pragma Import(C, Factorial_Iterative, "factorial_iterative");

    function Factorial_Recursive(N : UInt64) return UInt64;
    pragma Import(C, Factorial_Recursive, "factorial_recursive");

    function GCD_Iterative(A, B : UInt64) return UInt64;
    pragma Import(C, GCD_Iterative, "gcd_iterative");

    function GCD_Recursive(A, B : UInt64) return UInt64;
    pragma Import(C, GCD_Recursive, "gcd_recursive");

    function Diophantine_Equation_Iterative(A, B, C: Int64) return Result;
    pragma Import(C, Diophantine_Equation_Iterative, "diophantine_equation_iterative");

    function Diophantine_Equation_Recursive(A, B, C: Int64) return Result;
    pragma Import(C, Diophantine_Equation_Recursive, "diophantine_equation_recursive");
end Z5;