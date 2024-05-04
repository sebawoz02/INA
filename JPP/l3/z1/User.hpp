#pragma once

#include "DHSetup.hpp"

template<typename T>
class User {
private:
    unsigned long secret;
    T publicKey;
    T privateKey;
    bool keySet; // Flaga informująca, czy klucz został ustawiony

public:
    User(DHSetup<T>& setup) : keySet(false){
        // Ustawienie losowego sekretu
        std::random_device rd;
        std::mt19937 gen(rd());
        std::uniform_int_distribution<unsigned long> dis(2, T::get_p() - 2);
        secret = dis(gen);

        publicKey = setup.getGenerator();
        publicKey = DHSetup<T>::power(publicKey, secret);
    }

    T getPublicKey() {
        return publicKey;
    }

    void setKey(T a) {
        privateKey = DHSetup<T>::power(a, secret);
        keySet = true;
    }

 T encrypt(T m) {
        if (!keySet) {
            std::cerr << "Error: Klucz nie został jeszcze ustawiony!" << std::endl;
            return T();
        }
        return m * privateKey;
    }

    T decrypt(T c) {
        if (!keySet) {
            std::cerr << "Error: Klucz nie został jeszcze ustawiony!" << std::endl;
            return T();
        }
        return c / privateKey;
    }
};

