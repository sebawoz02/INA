//
// Created by sebastian on 04.05.23.
//

#include <stdbool.h>
#include <stdlib.h>
#include <time.h>
#include <stdio.h>

long swaps = 0;
long comps = 0;


// Swaps element i with element j.
void swap(int* i, int* j){
    swaps++;
    int tmp = *i;
    *i = *j;
    *j = tmp;
}

int min(int a, int b){
    return a > b ? b : a;
}

// funkcja sprawdzajaca czy tablica jest posortowana niemalejaco
bool checkIfSorted(const int A[], int N){
    for(int i = 1; i<N; i++){
        if(A[i-1] > A[i]) return false;
    }
    return true;
}


bool checkIfReversed(const int A[], int N){
    for(int i = 1; i<N; i++){
        if(A[i-1] < A[i]) return false;
    }
    return true;
}

void sort(int* A, size_t p, size_t q){
    if(q <= p) return;
    size_t i = p - 1, j = q + 1;
    int pivot = A[p];
    while (1){
        do{
            ++i;
        } while (A[i]< pivot);
        do {
            --j;
        } while (A[j] > pivot);
        if(i < j){
            int tmp = A[i];
            *(&A[i]) = *(&A[j]);
            *(&A[j]) = tmp;
        } else break;
    }
    sort(A, p, j);
    sort(A, j+1 , q);
}



int hoarePartition(int* A, size_t p, size_t q, int pivot){
    size_t i = p - 1, j = q + 1;
    while (1){
        do{
            i++;
            comps++;
        } while (A[i] < pivot);
        do{
            j--;
            comps++;
        } while (A[j] > pivot);
        if(i >= j) return j;
        swap(&A[i], &A[j]);
    }
}


/* SELECT ------------------------------------------------ */

// Returns pointer to the k-th order (0 - n-1) statistic from array A. The pivot is the median of the medians.
// 'groupSize' is the size of the groups from which the medians will be selected.
int selectM(int* A, size_t p, size_t q, size_t k, signed char groupSize) {
    if(p == q)  return A[p];
    size_t numOfMedians = (q-p + 1) % groupSize == 0 ? (q-p + 1)/groupSize : (q-p + 1)/groupSize + 1;
    // compute medians and move them to the left
    int* medians = malloc(numOfMedians* sizeof(int));
    size_t j = 0;
    for (size_t i = p; i <= q; i+=groupSize) {
        size_t subR = min(i + groupSize - 1, q);
        sort(A, i, subR);   // sort in O(1)
        medians[j] = A[(i+subR)/2];
        ++j;
    }
    // median of medians
    int pivot = selectM(medians, 0,  numOfMedians - 1, (numOfMedians - 1)/2, groupSize);
    size_t pivotIdx = hoarePartition(A, p , q, pivot);
    free(medians);
    if(k  == pivotIdx) return A[pivotIdx];
    if(k  < pivotIdx) return selectM(A, p , pivotIdx , k , groupSize);
    return selectM(A, pivotIdx + 1, q, k, groupSize);
}

// funkcja dzielaca tablice na trzy podtablice i zwracajaca 2 pivoty
// tab1: klucze mniejsze od pivota 1
// tab2: klucze >= pivot1 && klucze <= pivot2
// tab3: klucze wieksze od pivota 2
size_t partition(int* A, size_t low, size_t high, int* lp, bool withSelect){
    if(A[low] > A[high]) swap(&A[low], &A[high]);
    int p = withSelect ? selectM(A, low, high, low + (high - low + 1)/3, 5) : A[low], q = withSelect ? selectM(A, low, high, low + (high- low + 1)/3 * 2, 5) : A[high];    // lewy i prawy pivot

    size_t j = low, k = low, g = high;     //iteratory
    int l = 0, s = 0;   // countery
    while(k <= g){
        if(l>s){    // jezeli liczba elementow duzych wieksza od liczby elementow malych najpierw porownaj z q
            if(A[k] > q) {
                comps++;
                while(A[g] > q && k < g) g--; comps++;
                comps++;
                swap(&A[k], &A[g]);
                l++;
                g--;
                if(A[k] < p){
                    swap(&A[k], &A[j]);
                    j++;
                    s++;
                }
            }
            else if(A[k] < p){
                comps+=2;
                swap(&A[k], &A[j]);
                s++;
                j++;
            }
            comps+=2;
        }
        else{ // jezeli liczba elementow malych wieksza/rowna liczbie elementow duzych najpierw porownaj z p
            if(A[k] < p){
                comps++;
                swap(&A[k], &A[j]);
                s++;
                j++;
            }
            else if(A[k] > q) {
                comps+=2;
                while(A[g] > q && k < g) g--; comps++;
                comps++;
                swap(&A[k], &A[g]);
                l++;
                g--;
                comps++;
                if(A[k] < p){
                    swap(&A[k], &A[j]);
                    j++;
                    s++;
                }
            }
            else comps+=2;
        } k++;
    }
    *lp = j;
    return g;
}

// funkcja dzielaca problem sortowania na trzy podproblemy
void dualPivotQuickSort(int* A, int low, int high, bool withSelect){
    if(low < high){
        int lp, rp;
        rp = partition(A, low, high, &lp, withSelect);
        dualPivotQuickSort(A, low, lp, withSelect);
        dualPivotQuickSort(A, lp + 1, rp, withSelect);
        dualPivotQuickSort(A, rp + 1, high, withSelect);
    }
}


// Prints first n elements of array
void printArray(int* A, size_t _n){
    printf("[ ");
    for (size_t i = 0; i < _n; ++i) {
        printf("%d ", A[i]);
    }
    printf("]\n");
}



int main(void){

    size_t n;
    FILE *data = fopen("tmpdata.txt", "r");
    if(data == NULL){
        printf("Nie wygenerowano danych.\n");
        return -1;
    }
    srand(time(NULL));
    fscanf(data,"%zu", &n);
    int* A = malloc(sizeof(int[n]));
    int* copy = malloc(sizeof (int [n]));
    for (int i = 0; i < n; ++i) {
        int val;
        fscanf(data,"%d", &val);
        A[i] = val;
        copy[i] = val;
    }
    fclose(data);


    remove("tmpdata.txt");    // Remove temporary file

    sort(A, 0, n-1);
    sort(copy, 0, n-1);
    comps = 0;
    swaps = 0;

    clock_t start = clock();
    dualPivotQuickSort(A, 0, n-1, true);

    printf(" With select - %fs \nComps = %ld -  Swaps = %ld \n", (double) (clock() -start)/ CLOCKS_PER_SEC, comps, swaps);


    comps = 0;
    swaps = 0;
    start = clock();

    dualPivotQuickSort(copy, 0, n-1, false);

    printf(" Without select - %fs \nComps = %ld -  Swaps = %ld \n", (double) (clock() -start)/ CLOCKS_PER_SEC, comps, swaps);


    if(!checkIfSorted(A, n)) printf("Błąd w sortowaniu.");


    free(A);
    free(copy);

}
