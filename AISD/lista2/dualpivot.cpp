#include <iostream>
#include <bits/stdc++.h>

using namespace std;

long long swaps = 0;    //zmienna globalna liczaca ilosc przestawien kluczy ( swap() = 2 przestawienia )
long long comps = 0;    //zmienna globalna liczaca ilosc porownan kluczy
int n;      // dlugosc tablic
bool print;  // czy wyswietlac poszczegolne kroki sortowania

// funkcja wyswietlajaca tablice
void printArr(int A[], int N){
    cout << "[ ";
    for(int i = 0; i< N; i++){
        cout << A[i] << " ";
    }
    cout << "]" << endl;
}

// funkcja dzielaca tablice na trzy podtablice i zwracajaca 2 pivoty
// tab1: klucze mniejsze od pivota 1
// tab2: klucze >= pivot1 && klucze <= pivot2
// tab3: klucze wieksze od pivota 2
int partition(int A[], int low, int high, int* lp){
    if(A[low] > A[high]) swap(A[low], A[high]); swaps+=2;
    int p = A[low], q = A[high];    // lewy i prawy pivot
    int j = low + 1, k = low + 1, g = high - 1;     //iteratory
    int l = 0, s = 0;   // countery
    while(k <= g){
        if(l>s){    // jezeli liczba elementow duzych wieksza od liczby elementow malych najpierw porownaj z q
            if(A[k] > q) {
                comps++;
                while(A[g] > q && k < g) g--; comps++;
                comps++;
                swap(A[k], A[g]); swaps++;
                l++;
                g--;
                if(A[k] < p){
                    swap(A[k], A[j]); swaps++;
                    j++;
                    s++;
                }
            }
            else if(A[k] < p){
                comps+=2;
                swap(A[k], A[j]); swaps++;
                s++;
                j++;
            }
            comps+=2;
        }
        else{ // jezeli liczba elementow malych wieksza/rowna liczbie elementow duzych najpierw porownaj z p
            if(A[k] < p){
                comps++;
                swap(A[k], A[j]); swaps++;
                s++;
                j++;
            }
            else if(A[k] > q) {
                comps+=2;
                while(A[g] > q && k < g) g--; comps++;
                comps++;
                swap(A[k], A[g]); swaps++;
                l++;
                g--;
                comps++;
                if(A[k] < p){
                    swap(A[k], A[j]); swaps++;
                    j++;
                    s++;
                }
            }
            comps+=2;
        } k++;
    }
    j--;
    g++;
    swap(A[low], A[j]);  swaps++;
    swap(A[high], A[g]);  swaps++;

    *lp = j;
    return g;
}

// funkcja dzielaca problem sortowania na trzy podproblemy
void dualPivotQuickSort(int A[], int low, int high){
    if(low < high){
        int lp, rp;
        rp = partition(A, low, high, &lp);
        if(print) printArr(A, n);
        dualPivotQuickSort(A, low, lp-1);
        dualPivotQuickSort(A, lp + 1, rp -1);
        dualPivotQuickSort(A, rp + 1, high);
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
        dualPivotQuickSort(A, 0, n - 1);
        if(print) {
            cout << "Tablica wyjsciowa " << num << ": ";
            printArr(A, n);
        }
        if(!checkIfSorted(A, n)) {
            cout << endl << "Blad przy sortowaniu!" << endl;
            return -1;
        }
        cout << endl << "Liczba porowanan kluczy: " << comps << endl;
        cout << "Liczba przestawien kluczy: " << swaps << endl << endl;
    }
}
