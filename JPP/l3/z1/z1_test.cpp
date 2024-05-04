#include "DHSetup.hpp"
#include "User.hpp"
#include "../../l2/z1/GF.hpp"

int main() {
    GF::set_p(1234567891);

    DHSetup<GF> setup;

    User<GF> userA(setup);
    GF publicKeyA = userA.getPublicKey();
    User<GF> userB(setup);
    GF publicKeyB = userB.getPublicKey();

    // Wymiana kluczy publicznych między użytkownikami
    userA.setKey(publicKeyB);
    userB.setKey(publicKeyA);

    // Symulacja szyfrowania i deszyfrowania
    GF message = 53242;
    GF encrypted = userA.encrypt(message);
    GF decrypted = userB.decrypt(encrypted);

    std::cout << "Klucz publiczny użytkownika A: " << publicKeyA << std::endl;
    std::cout << "Klucz publiczny użytkownika B: " << publicKeyB << std::endl;
    std::cout << "Wiadomość do zaszyfrowania: " << message << std::endl;
    std::cout << "Zaszyfrowana wiadomość: " << encrypted << std::endl;
    std::cout << "Odszyfrowana wiadomość: " << decrypted << std::endl;

    return 0;
}
