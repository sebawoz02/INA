package Z2 is
    function Factorial_Iterative(N : Natural) return Natural;
    pragma Export(C, Factorial_Iterative, "ada_factorial_iterative");

    function Factorial_Recursive(N : Natural) return Natural;
    pragma Export(C, Factorial_Recursive, "ada_factorial_recursive");

    function GCD_Iterative(A, B : Natural) return Natural;
    pragma Export(C, GCD_Iterative, "ada_gcd_iterative");

    function GCD_Recursive(A, B : Natural) return Natural;
    pragma Export(C, GCD_Recursive, "ada_gcd_recursive");

end Z2;