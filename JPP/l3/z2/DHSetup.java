package z2;

import java.util.Random;
import z2.GF;

public class DHSetup<T extends GF>{
    private T generator;
    public long p;
    
    public DHSetup() {
        this.p = T.get_p();
        setGenerator();
    }

    public void setGenerator() {
        Random random = new Random();
        long potentialGenerator;
        do {
            potentialGenerator = random.nextLong() % (p - 1) + 1; // Losuj liczbę od 1 do p-1
        } while (!isValidGenerator(potentialGenerator, p - 1));
        
        generator = (T) T.newInstance(potentialGenerator);
    }
    
    public T getGenerator() {
        return generator;
    }

    public T power(T a, long b) {
        T result = (T) T.newInstance(1);
        while (b > 0) {
            if ((b & 1) == 1) { // sprawdź czy ostatni bit jest ustawiony na 1
                result = (T) T.mul(result, a);
            }
            a = (T) T.mul(a, a);
            b >>= 1;
        }
        return result;
    }
    
    // Checks if a number is a valid generator modulo p
    private boolean isValidGenerator(long a, long pMinusOne) {
        for (long q = 2; q < pMinusOne; q++) {
            if (pMinusOne % q == 0) {
                if (T.eq(power((T) T.newInstance(a), q), (T) T.newInstance(1))) {
                    return false;
                }
            }
        }
        return true;
    }
}
