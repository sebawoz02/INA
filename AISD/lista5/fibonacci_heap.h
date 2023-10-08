

#ifndef AISD_FIBONACCI_HEAP_H
#define AISD_FIBONACCI_HEAP_H

typedef struct fib_heap fib_heap;
typedef struct fib_node fib_node;

fib_heap *make_fib_heap(void);

void fib_heap_insert(fib_heap *heap, int key);

fib_node *fib_heap_extract_min(fib_heap *heap);

fib_heap *fib_heap_union(fib_heap *heap1, fib_heap *heap2);

void fib_print_node(fib_node* node);

#endif //AISD_FIBONACCI_HEAP_H
