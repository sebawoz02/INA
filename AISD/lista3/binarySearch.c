#include <stdlib.h>
#include <stdio.h>

// Swaps element i with element j.
void swap(int* i, int* j){
    int tmp = *i;
    *i = *j;
    *j = tmp;
}


// Quick sort on the elements of list A from index p to index q.
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
            swap(&A[i], &A[j]);
        } else break;
    }
    sort(A, p, j);
    sort(A, j+1 , q);
}


// Return 1 if v found in array A , else returns 0
int binarySearch(const int* A, int v, size_t l, size_t r){
    if(l>r){
        return 0;
    }
    size_t mid = (l + r)/2;
    if(v == A[mid]) return 1;
    if(v < A[mid]) return binarySearch(A, v, l, mid - 1);
    return binarySearch(A, v, mid+1 , r);
}

int main(int argc, char** argv){
    if(argc < 2){
        printf("Za mało argumentów \n");
        return -1;
    }
    FILE *data = fopen("tmpdata.txt", "r");
    if(data == NULL){
        printf("Nie wygenerowano danych.\n");
        return -1;
    }
    int v = atoi(argv[1]);
    size_t n;
    fscanf(data, "%zu", &n);
    int *A = malloc(sizeof(int[n]));
    for(size_t i = 0; i < n; ++i){
        int val;
        fscanf(data, "%d", &val);
        A[i] = val;
    }
    sort(A, 0, n-1);
    printf("[ ");
    for(size_t i = 0 ; i<n; ++i){
        printf("%d ", A[i]);
    }
    printf("]\n");

    int result = binarySearch(A, v, 0, n-1);
    if(result == 1) printf("Znaleziono element %d w tablicy.\n", v);
    else printf("Nie znaleziono %d w tablicy.\n", v);



    fclose(data);
    remove("tmpdata.txt");
    free(A);
    return 0;
}
