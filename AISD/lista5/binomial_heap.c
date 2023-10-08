#include <stdio.h>
#include <stdlib.h>
#include <limits.h>

#include "binomial_heap.h"

typedef struct BinomialNode
{
    int value;
    int degree;
    struct BinomialNode *child;
    struct BinomialNode *sibling;
} BinomialNode;

// Structure for binomial heap
struct BinomialHeap {
    struct BinomialNode* head;
};


static BinomialNode* binomial_create_node(int value)
{
    BinomialNode* newNode = malloc(sizeof(BinomialNode));
    newNode->value = value;
    newNode->degree = 0;
    newNode->child = NULL;
    newNode->sibling = NULL;
    return newNode;
}

static BinomialNode* binomial_merge_tree(BinomialNode *h1, BinomialNode *h2)
{
    if (h1->value > h2->value) {
        BinomialNode* temp = h1;
        h1 = h2;
        h2 = temp;
    }
    h2->sibling = h1->child;
    h1->child = h2;
    h1->degree++;
    return h1;
}

static void binomial_display_tree(BinomialNode* node, size_t level) {
    if (node == NULL)
        return;
    BinomialNode* current = node;
    size_t t = 1;
    while (current != NULL) {
        if(level == 0){
            printf("Tree %zu\n", t);
        }
        printf("%d\t", current->value);
        binomial_display_tree(current->child, level + 1);
        printf("\n");
        for (size_t i = 0; i < level; i++)
            printf("\t");
        current = current->sibling;
        t++;
    }
}

static BinomialNode* find_min_node(BinomialHeap* heap) {
    BinomialNode* minNode = heap->head;
    BinomialNode* curr = heap->head;

    while (curr != NULL) {
        if (curr->value < minNode->value) {
            minNode = curr;
        }
        curr = curr->sibling;
    }

    return minNode;
}

// Function to reverse the sibling list of a binomial heap
BinomialNode* reverse_list(BinomialNode* head) {
    if (head == NULL || head->sibling == NULL)
        return head;

    struct BinomialNode* prev = NULL;
    struct BinomialNode* curr = head;
    struct BinomialNode* next = NULL;

    while (curr != NULL) {
        next = curr->sibling;
        curr->sibling = prev;
        prev = curr;
        curr = next;
    }

    return prev;
}

BinomialHeap* binomial_create_heap(void) {
    BinomialHeap* newHeap = malloc(sizeof(*newHeap));
    newHeap->head = NULL;
    return newHeap;
}

BinomialHeap* binomial_union_heap(BinomialHeap* heap1, BinomialHeap* heap2) {
    if (heap1 == NULL || heap1->head == NULL)
        return heap2;
    if (heap2 == NULL || heap2->head == NULL)
        return heap1;

    BinomialNode* currNode1 = heap1->head;
    BinomialNode* currNode2 = heap2->head;

    BinomialHeap* newHeap = malloc(sizeof(*newHeap));
    BinomialNode* prevMergeNode = NULL;
    BinomialNode* currMergeNode = NULL;

    while (currNode1 != NULL && currNode2 != NULL) {
        if (currNode1->degree <= currNode2->degree) {
            currMergeNode = currNode1;
            currNode1 = currNode1->sibling;
        } else {
            currMergeNode = currNode2;
            currNode2 = currNode2->sibling;
        }

        if (prevMergeNode == NULL) {
            newHeap->head = currMergeNode;
        } else {
            prevMergeNode->sibling = currMergeNode;
        }

        prevMergeNode = currMergeNode;
    }

    if (currNode1 != NULL)
        prevMergeNode->sibling = currNode1;
    else
        prevMergeNode->sibling = currNode2;

    BinomialNode* currMergeSibling = newHeap->head;
    BinomialNode* prevMergeSibling = NULL;
    BinomialNode* nextMergeSibling = NULL;

    while (currMergeSibling != NULL && currMergeSibling->sibling != NULL) {
        nextMergeSibling = currMergeSibling->sibling;
        if (currMergeSibling->degree != nextMergeSibling->degree ||
            (nextMergeSibling->sibling != NULL && nextMergeSibling->sibling->degree == currMergeSibling->degree)) {
            prevMergeSibling = currMergeSibling;
            currMergeSibling = nextMergeSibling;
        } else if (currMergeSibling->value <= nextMergeSibling->value) {
            currMergeSibling->sibling = nextMergeSibling->sibling;
            currMergeSibling = binomial_merge_tree(currMergeSibling, nextMergeSibling);
        } else {
            if (prevMergeSibling == NULL)
                newHeap->head = nextMergeSibling;
            else
                prevMergeSibling->sibling = nextMergeSibling;
            currMergeSibling = binomial_merge_tree(nextMergeSibling, currMergeSibling);
        }
    }
    return newHeap;
}

BinomialHeap* binomial_insert_node(BinomialHeap* heap, int key) {
    BinomialNode* newNode = binomial_create_node(key);
    BinomialHeap* newHeap = binomial_create_heap();
    newHeap->head = newNode;
    return binomial_union_heap(heap, newHeap);
}


void binomial_extract_min(BinomialHeap** heap) {
    if ((*heap)->head == NULL) {
        printf("Heap is empty\n");
        return;
    }

    BinomialNode* minNode = find_min_node(*heap);
    BinomialNode* prevNode = NULL;
    BinomialNode* currNode = (*heap)->head;

    while (currNode->value != minNode->value) {
        prevNode = currNode;
        currNode = currNode->sibling;
    }

    if (prevNode == NULL) {
        (*heap)->head = currNode->sibling;
    } else {
        prevNode->sibling = currNode->sibling;
    }

    BinomialNode* childHead = reverse_list(currNode->child);
    BinomialHeap* childHeap = binomial_create_heap();
    childHeap->head = childHead;
    *heap = binomial_union_heap(*heap, childHeap);

    printf("%i\n", minNode->value);

    free(currNode);
}

// Function to display the binomial heap
void binomial_display_heap(BinomialHeap* heap) {
    BinomialNode* node = heap->head;
    printf("Binomial Heap:\n");
    binomial_display_tree(node, 0);
    printf("\n");
}
