#include <iostream>
#include <bits/stdc++.h>

using namespace std;

long long swaps = 0;    //zmienna globalna liczaca ilosc przestawien kluczy ( swap() = 2 przestawienia )
long long comps = 0;    //zmienna globalna liczaca ilosc porownan kluczy
int n;  // dlugosc tablic
bool print;  // czy wyswietlac poszczegolne kroki sortowania

// funkcja wyswietlajaca tablice
void printArr(int A[], int N){
    cout << "[ ";
    for(int i = 0; i< N; i++){
        cout << A[i] << " ";
    }
    cout << "]" << endl;
}

// Hoare partition z jednym pivotem.
// Dzieli tablice na dwie ( klucze wieksze od pivota oraz wieksze/rowne pivotowi ) - O(n)
int partition(int A[], int p, int q){
    int pivot = A[p];
    int i = p-1, j = q+1;
    while(true) {
        do{ i++;
            comps++;} while (A[i] < pivot);
        do{ j--;
            comps++;} while (A[j] > pivot);
        if(i >= j) return j;
        swap(A[i], A[j]);
        swaps+=2;
    }
}

// Funkcja sortujaca metoda insertion sort
// Wstawianie kluczy miedzy posortowane klucze od lewej strony - O(n^2)
void insertionSort(int* A, int low, int N){
    int i, j, key;
    for(i = low+1; i<=N;i++){
        key = A[i];
        j = i - 1;
        while(j >= 0 && A[j] > key){
            comps++;
            A[j+1] = A[j];
            swaps++;
            j--;
        }
        A[j+1] = key;
        comps++;
        if(n<40)printArr(A, n);
    }
}

// funkcja sortujaca dzialajaca jak quick sort natomiast gdy rozmiar podtablicy spada ponizej 9 uzywa insertion sorta
void advancedSort(int A[], int low, int high){
    if(low < high){
        if((high - low) < 9){
            insertionSort(A, low, high);
        }
        else{
            int pivot = partition(A, low, high);
            if(print) printArr(A, n);
            advancedSort(A, low, pivot);
            advancedSort(A, pivot+1, high);
        }
    }
}

// funkcja sprawdzajaca czy tablica jest posortowana niemalejaco
bool checkIfSorted(const int A[], int N){
    for(int i = 1; i<N; i++){
        if(A[i-1] > A[i]) return false;
    }
    return true;
}


int main() {
    cin >> n;
    int* A = new int[n];
    print = (n < 40);
    for(int num = 1; num <=3; num++) {
        swaps = 0;
        comps = 0;
        if(num==1)cout << "[ LOSOWA TABLICA ]" << endl;
        if(num==2)cout << "[ ROSNACA TABLICA ]" << endl;
        if(num==3)cout << "[ MALEJACA TABLICA ]" << endl;
        if(print) cout << "Tablica wejsciowa " << num <<": [ ";
        for (int i = 0; i < n; i++) {
            cin >> A[i];
            if(print)cout << A[i] << " ";
        }
        if(print) cout << "]" << endl;
        advancedSort(A, 0, n-1);
        if(print) {
            cout << "Tablica wyjsciowa " << num << ": ";
            printArr(A, n);
        }
        if(!checkIfSorted(A, n)) {
            cout << endl << "Blad przy sortowaniu!" << endl;
            return -1;
        }
        cout << endl<< "Liczba porowanan kluczy: " << comps << endl;
        cout << "Liczba przestawien kluczy: " << swaps << endl << endl;
    }
}

