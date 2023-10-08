#include <stdio.h>
#include <string.h>
#include <stdlib.h>


typedef struct QueueElement {
    void *data;
    struct QueueElement *nextElement;
}QueueElement;

typedef struct Queue {
    QueueElement* head;
} Queue;


void fifoPush(Queue* fifo, void *data){
    QueueElement* tail = fifo->head;
    // looking for the tail if queue is not empty
    if(tail != NULL) {
        while (tail->nextElement!=NULL){
            tail = tail->nextElement;}
    }
    QueueElement* newTail;
    newTail = malloc(sizeof(QueueElement));
    newTail->data = malloc(sizeof(*(data)));
    memcpy(newTail->data, data, sizeof(*(data)));
    if(tail == NULL) fifo->head = newTail;     // Queue was empty - head created
    else tail->nextElement = newTail;       // Queue wasn't empty -  new tail created
}


void lifoPush(Queue* lifo, void *data){
    QueueElement* newHead;
    newHead = malloc(sizeof(QueueElement));
    newHead->data = malloc(sizeof(*(data)));
    memcpy(newHead->data, data, sizeof(*data));
    if(lifo->head==NULL) lifo->head = newHead;  // Stack was empty - head created
    else{
        newHead->nextElement = lifo->head;  // Stack wasn't empty - new head created on top of previous one
        lifo->head = newHead;
    }
}


void* poppedValue = NULL;
// removes head from the queue/stack
// returns NULL if queue/stack was empty, else returns value of the removed head
void* pop(Queue* queue){
    if(queue->head != NULL){
        QueueElement* currHead = queue->head;
        queue->head = queue->head->nextElement;
        poppedValue = realloc(poppedValue, sizeof(*(currHead->data)));
        memcpy(poppedValue, currHead->data, sizeof(*(currHead->data)));
        free(currHead->data);
        free(currHead);
        return poppedValue;
    }
    return NULL;
}


int main(){
    Queue lifo = {NULL};
    Queue fifo = {NULL};
    //adding int 100 times to the lifo stack and fifo queue
    printf("Dodaje do LIFO i FIFO elementy: ");
    for(int i = 0; i<100; i++){
        printf("%d ", i);
        lifoPush(&lifo, &i);
        fifoPush(&fifo, &i);
    }
    //removing everything from the lifo stack
    printf("\nUsuwam z LIFO elementy: ");
    int* value = pop(&lifo);
    while (value != NULL){
        printf("%d ", *(value));
        value = pop(&lifo);
    }
    //removing everything from the fifo queue
    printf("\nUsuwam z FIFO elementy: ");
    value = pop(&fifo);
    while (value != NULL){
        printf("%d ", *(value));
        value = pop(&fifo);
    }

    // test on char
    char testChar1 = 'A';
    char testChar2 = 'B';
    char testChar3 = 'C';
    printf("\n\nDodaje do FIFO i LIFO elementy: %c %c %c", testChar1, testChar2, testChar3);
    fifoPush(&fifo, &testChar1);
    fifoPush(&fifo, &testChar2);
    fifoPush(&fifo, &testChar3);
    lifoPush(&lifo, &testChar1);
    lifoPush(&lifo, &testChar2);
    lifoPush(&lifo, &testChar3);
    printf("\nUsuwam z FIFO elementy: ");
    value = pop(&fifo);
    while (value != NULL){
        printf("%c ", *(value));
        value = pop(&fifo);
    }

    printf("\nUsuwam z LIFO elementy: ");
    value = pop(&lifo);
    while (value != NULL){
        printf("%c ", *(value));
        value = pop(&lifo);
    }
    
    free(poppedValue);
}
