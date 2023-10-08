#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>


typedef struct ListElement {
    void *data;
    struct ListElement *nextElement;
}ListElement;

typedef struct List {
    ListElement* head;
} List;


void pushBack(List* list,void *data){
    ListElement* tail = list->head;
    // looking for the tail if queue is not empty
    if(tail != NULL) {
        while (tail->nextElement!=NULL){
            tail = tail->nextElement;}
    }
    ListElement * newTail;
    newTail = malloc(sizeof(ListElement ));
    newTail->data = malloc(sizeof(*(data)));
    memcpy(newTail->data, data, sizeof(*(data)));
    if(tail == NULL) list->head = newTail;     // Queue was empty - head created
    else tail->nextElement = newTail;
}


void pushFront(List* list, void *data){
    ListElement* newHead = malloc(sizeof(ListElement));
    newHead->data = malloc(sizeof(*data));
    memcpy(newHead->data, data, sizeof(*data));
    newHead->nextElement = list->head;
    list->head = newHead;
}


void* poppedValue = NULL;

// pops the value at the end of the list
// returns popped value or NULL if list was empty
void* popBackValue(List* list){
    ListElement* tail = list->head;
    if(tail != NULL){
        while(tail->nextElement!=NULL){
            tail = tail->nextElement;
        }
        poppedValue = realloc(poppedValue, sizeof(*(tail->data)));
        memcpy(poppedValue, tail->data, sizeof(*(tail->data)));
        free(tail->data);
        free(tail);
        return poppedValue;
    }
    return NULL;
}

// pops the value at the beggining of the list
// returns popped value or NULL if list was empty
void* popFrontValue(List* list){
    if(list->head != NULL){
        ListElement* currHead = list->head;
        list->head = list->head->nextElement;
        poppedValue = realloc(poppedValue, sizeof(*(currHead->data)));
        memcpy(poppedValue, currHead->data, sizeof(*(currHead->data)));
        free(currHead->data);
        free(currHead);
        return poppedValue;
    }
    return NULL;
}

// shows the value at a specific index, returns NULL if it doesn't exist
void* showValue(List* list, int index){
    int counter = 0;
    ListElement* element = list->head;
    while(counter != index){
        if(element==NULL) return NULL;
        counter++;
        element = element->nextElement;
    }
    return element->data;
}

// prints all the values in the list, function adapted only to type int
void printList(List* list){
    ListElement* element = list->head;
    printf("[");
    while(element!=NULL){
        printf(" %d,",*(int*) element->data);
        element = element->nextElement;
    }
    printf("]\n");
}


void merge(List* list1, List* list2){
    ListElement* node = list1->head;
    if(node!=NULL && list2->head!=NULL){
        while(node->nextElement!=NULL){
            node = node->nextElement;
        }
        node->nextElement = list2->head;
    }
}


// returs time needed to get element under given index
double find(List* list, int index){
    clock_t start = clock();
    int counter = index;
    if(list->head==NULL) return 0;
    ListElement* node = list->head;
    while(counter>0){
        node = node->nextElement;
        counter--;
    }
    clock_t end = clock();
    double time = (double) (end - start) / (double)CLOCKS_PER_SEC;
    return time;
}

int len(List* list){
    if(list->head==NULL) return 0;
    ListElement* node = list->head;
    int len = 0;
    node = node->nextElement;
    len++;
    while(node!=NULL){
        len++;
        node = node->nextElement;
    }
    return len;
}

//if index<0 then checks for random indices
double getAvgAccessTime(int index, int numOfTests, List *list){
    double avgtime = 0;
    if(index<0){
        for(int i = 0; i<numOfTests; i++){
            index = rand() % 20000;
            avgtime += find(list, index);
        }
        avgtime /= numOfTests;
        return avgtime;
    }
    for(int i = 0; i<numOfTests; i++){
        avgtime += find(list, index);
    }
    avgtime /= numOfTests;
    return avgtime;
}

int main(){
    List list1 = {NULL};
    List list2 = {NULL};
    int value = 0;
    for(int i = 0; i < 10000; i++) {
        value = rand();
        pushFront(&list1, &value);
        value = rand();
        pushBack(&list2, &value);
    }
    printf("%d \n", len(&list1));
    merge(&list1, &list2);
    printf("%d \n", len(&list1));

    // access time measurement

    double avgtime;
    int index = 3000;
    int numOfTests = 5000;

    avgtime = getAvgAccessTime(index, numOfTests,&list1);
    printf("Sredni czas dostepu do elementu pod indexem %d to %f s\n", index, avgtime);

    index = 10000;
    avgtime = getAvgAccessTime(index, numOfTests,&list1);
    printf("Sredni czas dostepu do elementu pod indexem %d to %f s\n", index, avgtime);

    index = 16000;
    avgtime = getAvgAccessTime(index, numOfTests,&list1);
    printf("Sredni czas dostepu do elementu pod indexem %d to %f s\n", index, avgtime);

    // random index
    avgtime = getAvgAccessTime(-1, numOfTests, &list1);
    printf("Sredni czas dostepu do elementu pod losowym indexem to %f s", avgtime);

}
