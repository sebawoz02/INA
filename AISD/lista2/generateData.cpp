#include <iostream>
#include <random>
#include <chrono>
#include <bits/stdc++.h>

using namespace std;

int main(int argc, char** argv){
    if(argc < 2) return -1;
    mt19937 mt{ static_cast<unsigned int>(chrono::steady_clock::now().time_since_epoch().count()) };
    int n = stoi(argv[1]);
    int* tab;
    uniform_int_distribution dis{0, 2*n - 1};
    tab = new int[n];
    cout << n << endl;
    // tablica losowych kluczy
    for(int i = 0; i < n; i++){
        tab[i] = dis(mt);
    }
    for(int i =0; i<n;i++){
        cout << tab[i] << endl;
    }

    // tablica losowych kluczy rosnaco
    for(int i = 0; i < n; i++){
        tab[i] = dis(mt);
    }
    sort(tab, tab+n);
    for(int i =0; i<n;i++){
        cout << tab[i] << endl;
    }

    // tablica losowych kluczy malejaco
    for(int i = 0; i < n; i++){
        tab[i] = dis(mt);
    }
    sort(tab, tab+n, greater<>());
    for(int i =0; i<n;i++){
        cout << tab[i] << endl;
    }
}
