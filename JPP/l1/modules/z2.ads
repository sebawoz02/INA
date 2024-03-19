with Interfaces.C; use Interfaces.C;

package Z2 is

    subtype Int64 is Interfaces.C.long;
    subtype UInt64 is Interfaces.C.unsigned_long;

    type Result is record
        X : Int64;
        Y : Int64;
    end record;
    pragma Convention(C, Result);

    function Factorial_Iterative(N : UInt64) return UInt64;
    pragma Export(C, Factorial_Iterative, "ada_factorial_iterative");

    function Factorial_Recursive(N : UInt64) return UInt64;
    pragma Export(C, Factorial_Recursive, "ada_factorial_recursive");

    function GCD_Iterative(A, B : UInt64) return UInt64;
    pragma Export(C, GCD_Iterative, "ada_gcd_iterative");

    function GCD_Recursive(A, B : UInt64) return UInt64;
    pragma Export(C, GCD_Recursive, "ada_gcd_recursive");

    function Diophantine_Equation_Iterative(A, B, C: Int64) return Result;
    pragma Export(C, Diophantine_Equation_Iterative, "ada_diophantine_equation_iterative");

    function Diophantine_Equation_Recursive(A, B, C: Int64) return Result;
    pragma Export(C, Diophantine_Equation_Recursive, "ada_diophantine_equation_recursive");

end Z2;