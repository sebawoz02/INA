#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <stdbool.h>

// # of comparisons
int64_t comps = 0;
// # of swaps
int64_t swaps = 0;
// size of loaded array
size_t n;


// Swaps element i with element j.
void swap(int* i, int* j){
    swaps++;
    int tmp = *i;
    *i = *j;
    *j = tmp;
}

// Insertion sort on the elements of list A from index p to index q.
void sort(int* A, size_t p, size_t q){
    int i = p+1;
    while (i <=q){
        size_t j = i;
        while (j > p && A[j-1] > A[j]){
            comps++;
            swap(&A[j], &A[j-1]);
            j--;
        }
        if(j>p) comps++;
        i++;
    }

}

//Returns smaller number
int min(int a, int b){
    return a > b ? b : a;
}

// Prints first n elements of array
void printArray(int* A, size_t _n){
    printf("[ ");
    for (size_t i = 0; i < _n; ++i) {
        printf("%d ", A[i]);
    }
    printf("]\n");
}

size_t lomutoPartition(int* A, size_t p, size_t q){
    int pivot = A[q];
    size_t i = p-1;
    for(size_t j = p; j <= q-1; j++){
        comps++;
        if(A[j] <= pivot) {
            i++;
            swap(&A[i], &A[j]);
        }
    }
    i++;
    swap(&A[i], &A[q]);
    return i;
}


/* SELECT ------------------------------------------------ */

// Returns pointer to the k-th order (0 - n-1) statistic from array A. The pivot is the median of the medians.
// 'groupSize' is the size of the groups from which the medians will be selected.
int selectM(int* A, size_t p, size_t q, size_t k, signed char groupSize, bool printSteps) {
    if(p == q)  return A[p];
    size_t numOfMedians = (q-p + 1) % groupSize == 0 ? (q-p + 1)/groupSize : (q-p + 1)/groupSize + 1;
    // compute medians and move them to the left
    int* medians = malloc(numOfMedians* sizeof(int));
    size_t j = 0;
    for (size_t i = p; i <= q; i+=groupSize) {
        size_t subR = min(i + groupSize - 1, q);
        sort(A, i, subR);   // sort in O(1)
        medians[j] = A[i + (subR-i+1)/2];
        ++j;
    }
    // median of medians
    int pivot = selectM(medians, 0,  numOfMedians - 1, (numOfMedians - 1)/2, groupSize, false);
    for(size_t i = p; i<=q; i++){
        comps++;
        if(A[i]==pivot){
            swap(&A[q], &A[i]);
            break;
        }
    }
    size_t pivotIdx = lomutoPartition(A, p , q);
    free(medians);
    if(printSteps) printArray(A, n);
    if(k  == pivotIdx) return pivot;
    if(k  < pivotIdx) return selectM(A, p , pivotIdx - 1 , k , groupSize, printSteps);
    return selectM(A, pivotIdx + 1, q, k, groupSize, printSteps);
}

/* SELECT ------------------------------------------------ */

/* RANDOMIZED SELECT ------------------------------------------------ */

size_t randomizedPartition(int* A, size_t p, size_t q){
    size_t pivIdx = (random() % (q+1-p) ) + p;
    swap(&A[pivIdx], &A[q]);
    return lomutoPartition(A, p, q);
}

// Returns k-th order statistic (1-n) from array A. Pivot is random.
int randomizedSelect(int* A, size_t p, size_t q, size_t k, bool printSteps){
    if(p==q) return A[p];
    size_t pivIdx = randomizedPartition(A, p, q);
    if(printSteps) printArray(A, n);
    size_t i = pivIdx - p + 1;
    if(k == i) return A[pivIdx];
    else if(k < i) return randomizedSelect(A, p, pivIdx - 1, k, printSteps);
    else return randomizedSelect(A, pivIdx + 1, q, k - i, printSteps);
}
/* RANDOMIZED SELECT ------------------------------------------------ */

int main(int argc, char** argv) {
    if(argc < 3){
        printf("Za mało argumentów \n");
        return -1;
    }
    FILE *data = fopen("tmpdata.txt", "r");
    if(data == NULL){
        printf("Nie wygenerowano danych.\n");
        return -1;
    }
    srand(time(NULL));
    fscanf(data,"%zu", &n);
    size_t k = atoi(argv[1]);
    signed char groupsize = *argv[2];
    int* A = malloc(sizeof(int[n]));
    int* copy = malloc(sizeof(int[n]));
    for (int i = 0; i < n; ++i) {
        int val;
        fscanf(data,"%d", &val);
        A[i] = val;
        copy[i] = val;
    }

    fclose(data);
    bool print = n < 50;

    remove("tmpdata.txt");    // Remove temporary file
    //FILE* randomData = fopen("randdata.txt", "a");
    //FILE* selectData = fopen("selecdata.txt", "a");

    // RANDOMIZED SELECT tests
    printf("----- RANDOMIZED SELECT -----\n");
    if(print) {
        printf("\nTablica wejściowa: ");
        printArray(A, n);
    }
    clock_t start = clock();
    printf("\nOtrzymana %zu statystyka pozycyjna: %d\n", k,randomizedSelect(A,0, n-1,k, print));
    printf("Liczba porównań: %ld\nLiczba swapów: %ld\n", comps, swaps);
    /*
    fprintf(randomData, "%f ", (double) (clock() - start) / CLOCKS_PER_SEC);
    fclose(randomData);
    */
    // SELECT tests
    comps = 0;
    swaps = 0;
    printf("\n----- SELECT -----\n");
    if(print) {
        printf("\nTablica wejściowa: ");
        printArray(copy, n);
    }
    printf("\nOtrzymana %zu statystyka pozycyjna: %d\n", k,selectM(copy,0, n-1,k - 1, groupsize, print));
    printf("Liczba porównań: %ld\nLiczba swapów: %ld\n", comps, swaps);
    //fprintf(selectData, "%ld %ld ", comps, swaps);
    if(print) {
        sort(A, 0, n - 1);
        printf("\nPosortowana tablica: ");
        printArray(A, n);
    }
    //fclose(selectData);

    free(A);
    free(copy);
    return 0;
}
