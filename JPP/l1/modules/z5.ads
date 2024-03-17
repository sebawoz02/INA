package Z5 is
    function Factorial_Iterative(N : Natural) return Natural;
    pragma Import(C, Factorial_Iterative, "factorial_iterative");

    function Factorial_Recursive(N : Natural) return Natural;
    pragma Import(C, Factorial_Recursive, "factorial_recursive");

    function GCD_Iterative(A, B : Natural) return Natural;
    pragma Import(C, GCD_Iterative, "gcd_iterative");

    function GCD_Recursive(A, B : Natural) return Natural;
    pragma Import(C, GCD_Recursive, "gcd_recursive");
end Z5;