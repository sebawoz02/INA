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


int hoarePartition(int A[], int p, int q){
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


void quickSort(int A[], int p, int q){
    if(p < q){
        int r = hoarePartition(A, p, q);
        if(print) printArr(A, n);
        quickSort(A, p , r);
        quickSort(A, r+1, q);
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
        quickSort(A, 0, n - 1);
        if(print) {
            cout << "Tablica wyjsciowa " << num << ": ";
            printArr(A, n);
        }
        if(!checkIfSorted(A, n)) {
            cout << endl << "Blad przy sortowaniu!" << endl;
            return -1;
        }
        cout << endl <<"Liczba porowanan kluczy: " << comps << endl;
        cout << "Liczba przestawien kluczy: " << swaps << endl << endl;
    }
}
