package z2;

import z2.GF;

public class Main {
    // Launch with -ea switch
    public static void main(String[] args) {
        testAddition();
        testSubtraction();
        testMultiplication();
        testDivision();
        System.out.println("Test completed positively!");
    }

    private static void testAddition() {
        GF a = new GF(5);
        GF b = new GF(7);
        GF result = GF.add(a, b);
        a.iadd(b);
        assert result.value == 12;
        assert a.value == 12;
    }

    private static void testSubtraction() {
        GF a = new GF(10);
        GF b = new GF(7);
        GF result = GF.sub(a, b);
        a.isub(b);
        assert result.value == 3;
        assert a.value == 3;
    }

    private static void testMultiplication() {
        GF a = new GF(5);
        GF b = new GF(7);
        GF result = GF.mul(a, b);
        a.imult(b);
        assert result.value == 35;
        assert a.value == 35;
    }

    private static void testDivision() {
        GF a = new GF(15);
        GF b = new GF(3);
        GF result = GF.div(a, b);
        a.idiv(b);
        assert result.value == 5;
        assert a.value == 5;
    }
}
