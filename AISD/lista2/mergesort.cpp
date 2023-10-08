#include <iostream>

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

// funkcja laczaca dwie tablice tak aby kluczy byly posortowane niemalejaco
void merge(int A[], int const left, int const mid, int const right){
    int const A1 = mid - left + 1;
    int const A2 = right - mid;

    auto *leftArray = new int [A1], *rightArray = new int[A2];

    for(int i = 0; i < A1; i++) {leftArray[i] = A[left + i]; swaps++;}
    for(int i = 0; i < A2; i++) {rightArray[i] = A[mid + 1 + i]; swaps++;}

    auto indexOfA1 = 0, indexOfA2 = 0, indexOfMerged = left;
    while(indexOfA1 < A1 && indexOfA2 < A2){
        comps++;
        if(leftArray[indexOfA1] <= rightArray[indexOfA2]){
            A[indexOfMerged] = leftArray[indexOfA1];
            indexOfA1++;
        } else {
            A[indexOfMerged] = rightArray[indexOfA2];
            indexOfA2++;
        }
        swaps++;
        indexOfMerged++;
    }

    while(indexOfA1 < A1){
        A[indexOfMerged] = leftArray[indexOfA1];
        indexOfMerged++;
        swaps++;
        indexOfA1++;
    }

    while(indexOfA2 < A2){
        A[indexOfMerged] = rightArray[indexOfA2];
        indexOfMerged++;
        swaps++;
        indexOfA2++;
    }

    delete[] leftArray;
    delete[] rightArray;
}

// funkcja dzielaca tablice na wiecej mniejszych tablic ( z kazdym wywolaniem na dwie ) i potem laczaca je funkcja merge
void mergeSort(int A[], int const begin, int const end){
    if(begin >= end) return;
    int mid =  begin + (end - begin)/ 2;
    mergeSort(A, begin, mid);
    mergeSort(A, mid + 1, end);
    merge(A, begin, mid, end);
    if(print) printArr(A, n);
}

// funkcja sprawdzajaca czy tablica jest posortowana niemalejaco - O(n*logn)
bool checkIfSorted(const int A[], int N){
    for(int i = 1; i<N; i++){
        if(A[i-1] > A[i]) return false;
    }
    return true;
}



int main(){
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
        mergeSort(A, 0, n - 1);
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
