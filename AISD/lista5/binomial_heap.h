
#ifndef AISD_BINOMIAL_HEAP_H
#define AISD_BINOMIAL_HEAP_H

typedef struct BinomialHeap BinomialHeap;

BinomialHeap* binomial_union_heap(BinomialHeap* heap1, BinomialHeap* heap2);

BinomialHeap* binomial_create_heap(void);

BinomialHeap* binomial_insert_node(BinomialHeap* heap, int key);

void binomial_extract_min(BinomialHeap** heap);

void binomial_display_heap(BinomialHeap* heap);

#endif //AISD_BINOMIAL_HEAP_H
