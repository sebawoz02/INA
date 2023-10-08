#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <math.h>

#include "fibonacci_heap.h"

// Struktura wierzchołka Fibonacci Heap
struct fib_node {
    int key;
    int degree;
    fib_node *child;
    fib_node *left;
    fib_node *right;
};

struct fib_heap {
    fib_node *min;
    int num_nodes;
};

// Tworzenie nowego wierzchołka Fibonacci Heap
static struct fib_node *create_node(int key) {
    fib_node *node = malloc(sizeof(*node));
    node->key = key;
    node->degree = 0;
    node->child = NULL;
    node->left = node;
    node->right = node;
    return node;
}

// Linking the heap nodes in parent child relationship
static void Fibonnaci_link(fib_heap* heap,fib_node* ptr2, fib_node* ptr1)
{
    (ptr2->left)->right = ptr2->right;
    (ptr2->right)->left = ptr2->left;
    if (ptr1->right == ptr1)
        heap->min = ptr1;
    ptr2->left = ptr2;
    ptr2->right = ptr2;
    if (ptr1->child == NULL)
        ptr1->child = ptr2;
    ptr2->right = ptr1->child;
    ptr2->left = (ptr1->child)->left;
    ((ptr1->child)->left)->right = ptr2;
    (ptr1->child)->left = ptr2;
    if (ptr2->key < (ptr1->child)->key)
        ptr1->child = ptr2;
    ptr1->degree++;
}

// Konsolidacja drzew w Fibonacci Heap
static void consolidate(fib_heap *heap) {
    int temp1;
    double temp2 = (log(heap->num_nodes)) / (log(2));
    int temp3 = (int) round(temp2);
    fib_node* arr[temp3+1];
    for (int i = 0; i <= temp3; i++)
        arr[i] = NULL;
    fib_node* ptr1 = heap->min;
    fib_node* ptr2;
    fib_node* ptr3;
    fib_node* ptr4 = ptr1;
    do {
        ptr4 = ptr4->right;
        temp1 = ptr1->degree;
        while (arr[temp1] != NULL) {
            ptr2 = arr[temp1];
            if (ptr1->key > ptr2->key) {
                ptr3 = ptr1;
                ptr1 = ptr2;
                ptr2 = ptr3;
            }
            if (ptr2 == heap->min)
                heap->min = ptr1;
            Fibonnaci_link(heap, ptr2, ptr1);
            if (ptr1->right == ptr1)
                heap->min = ptr1;
            arr[temp1] = NULL;
            temp1++;
        }
        arr[temp1] = ptr1;
        ptr1 = ptr1->right;
    } while (ptr1 != heap->min);
    heap->min = NULL;
    for (int j = 0; j <= temp3; j++) {
        if (arr[j] != NULL) {
            arr[j]->left = arr[j];
            arr[j]->right = arr[j];
            if (heap->min != NULL) {
                (heap->min->left)->right = arr[j];
                arr[j]->right = heap->min;
                arr[j]->left = heap->min->left;
                heap->min->left = arr[j];
                if (arr[j]->key < heap->min->key)
                    heap->min = arr[j];
            }
            else {
                heap->min = arr[j];
            }
            if (heap->min == NULL)
                heap->min = arr[j];
            else if (arr[j]->key < heap->min->key)
                heap->min = arr[j];
        }
    }
}


// Inicjalizacja pustego Fibonacci Heap
fib_heap *make_fib_heap(void) {
    fib_heap *heap = malloc(sizeof(*heap));
    heap->min = NULL;
    heap->num_nodes = 0;
    return heap;
}

// Wstawianie wierzchołka do Fibonacci Heap
void fib_heap_insert(fib_heap *heap, int key) {
    fib_node* node = create_node(key);
    if (heap->min == NULL) {
        heap->min = node;
    } else {
        node->left = heap->min;
        node->right = heap->min->right;
        heap->min->right->left = node;
        heap->min->right = node;
        if (node->key < heap->min->key) {
            heap->min = node;
        }
    }
    heap->num_nodes++;
}

// Łączenie dwóch Fibonacci Heap w jeden
fib_heap *fib_heap_union(fib_heap *heap1, fib_heap *heap2) {
    fib_heap *new_heap = make_fib_heap();
    new_heap->min = heap1->min;
    if (heap1->min != NULL && heap2->min != NULL) {
        heap1->min->right->left = heap2->min;
        heap2->min->right->left = heap1->min;
        fib_node *tmp = heap1->min->right;
        heap1->min->right = heap2->min->right;
        heap2->min->right = tmp;
        if (heap2->min->key < heap1->min->key) {
            new_heap->min = heap2->min;
        }
    } else if (heap2->min != NULL) {
        new_heap->min = heap2->min;
    }
    new_heap->num_nodes = heap1->num_nodes + heap2->num_nodes;
    free(heap1);
    free(heap2);
    return new_heap;
}

// Wyszukiwanie i usuwanie wierzchołka o najmniejszym kluczu w Fibonacci Heap
fib_node *fib_heap_extract_min(fib_heap *heap) {
    fib_node* min = heap->min;
    if (heap->min == NULL)
        printf("Heap is empty\n");
    else {
        fib_node *temp = heap->min;
        fib_node *pntr;
        fib_node *x = NULL;
        if (temp->child != NULL) {
            x = temp->child;
            do {
                pntr = x->right;
                (heap->min->left)->right = x;
                x->right = heap->min;
                x->left = heap->min->left;
                heap->min->left = x;
                if (x->key < heap->min->key)
                    heap->min = x;
                x = pntr;
            } while (pntr != temp->child);
        }
        (temp->left)->right = temp->right;
        (temp->right)->left = temp->left;
        heap->min = temp->right;
        if (temp == temp->right && temp->child == NULL)
            heap->min = NULL;
        else {
            heap->min = temp->right;
            consolidate(heap);
        }
        heap->num_nodes--;
    }
    return min;
}

void fib_print_node(fib_node* node)
{
    printf("V: %i\n", node->key);
}

