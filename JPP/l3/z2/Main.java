package z2;

import z2.GF;
import z2.DHSetup;

public class Main {
    // Launch with -ea switch
    public static void main(String[] args) {
        GF.set_p(1234567891);

        DHSetup<GF> setup = new DHSetup<>();

        User<GF> userA = new User<>(setup);
        GF publicKeyA = userA.getPublicKey();

        User<GF> userB = new User<>(setup);
        GF publicKeyB = userB.getPublicKey();

        // Wymiana kluczy publicznych między użytkownikami
        userA.setKey(publicKeyB, setup);
        userB.setKey(publicKeyA, setup);

        GF message = new GF(53242);
        GF encrypted = userA.encrypt(message);
        GF decrypted = userB.decrypt(encrypted);

        System.out.println("Klucz publiczny użytkownika A: " + publicKeyA.value);
        System.out.println("Klucz publiczny użytkownika B: " + publicKeyB.value);
        System.out.println("Wiadomość do zaszyfrowania: " + message.value);
        System.out.println("Zaszyfrowana wiadomość: " + encrypted.value);
        System.out.println("Odszyfrowana wiadomość: " + decrypted.value);
    }
}