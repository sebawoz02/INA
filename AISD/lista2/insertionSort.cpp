#include <iostream>

using namespace std;

long long swaps = 0;    //zmienna globalna liczaca ilosc przestawien kluczy ( swap() = 2 przestawienia )
long long comps = 0;    //zmienna globalna liczaca ilosc porownan kluczy

// funkcja wyswietlajaca tablice
void printArr(int A[], int N){
    cout << "[ ";
    for(int i = 0; i< N; i++){
        cout << A[i] << " ";
    }
    cout << "]" << endl;
}

// Funkcja sortujaca metoda insertion sort
// Wstawianie kluczy miedzy posortowane klucze od lewej strony - O(n^2)
void insertionSort(int* A, int n){
    int i, j, key;
    for(i = 1; i<n;i++){
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

// funkcja sprawdzajaca czy tablica jest posortowana niemalejaco
bool checkIfSorted(const int A[], int N){
    for(int i = 1; i<N; i++){
        if(A[i-1] > A[i]) return false;
    }
    return true;
}

int main() {
    int n;
    cin >> n;
    int* A = new int[n];
    bool print = (n < 40);
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
        insertionSort(A, n);
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
