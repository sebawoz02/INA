#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>

typedef struct ListElement {
    void *data;
    struct ListElement *nextElement;
    struct ListElement *prevElement;
}ListElement;

typedef struct List {
    ListElement* firstElement;
    int size;
} List;


void push(List* list, void* data){
    ListElement* newElement;
    newElement = malloc(sizeof(ListElement));
    newElement->data = malloc(sizeof(*data));
    memcpy(newElement->data, data, sizeof(*data));

    if(list->firstElement!=NULL){
        ListElement* prev = list->firstElement->prevElement;
        if(prev == list->firstElement){
            list->firstElement->nextElement = newElement;
            list->firstElement->prevElement = newElement;
            newElement->nextElement = list->firstElement;
            newElement->prevElement = list->firstElement;
        }
        else{
                prev->nextElement = newElement;
                list->firstElement->prevElement = newElement;
                newElement->nextElement = list->firstElement;
                newElement->prevElement = prev;
        }
    }
    else{
        newElement->nextElement = newElement;
        newElement->prevElement = newElement;
        list->firstElement = newElement;
    }
    list->size++;
}

void* poppedValue;
void* pop(List* list){
    ListElement* toPop = list->firstElement;
    if(toPop!=NULL) {
        toPop->prevElement->nextElement = toPop->nextElement;
        toPop->nextElement->prevElement = toPop->prevElement;
        poppedValue = realloc(poppedValue, sizeof(toPop->data));
        memcpy(poppedValue, toPop->data, sizeof(toPop->data));
        free(toPop->data);
        free(toPop);
        list->size--;
        return poppedValue;
    }
    return NULL;
}



int len(List* list){
    if(list->firstElement==NULL) return 0;
    ListElement* node = list->firstElement;
    int len = 0;
    node = node->nextElement;
    len++;
    while(node!=list->firstElement){
        len++;
        node = node->nextElement;
    }
    return len;
}


void merge(List* list1, List* list2){
    if(list1->firstElement!=NULL && list2->firstElement!=NULL){
        list1->firstElement->prevElement->nextElement = list2->firstElement;
        list1->firstElement->prevElement = list2->firstElement->prevElement;
        list2->firstElement->prevElement->nextElement = list1->firstElement;
        list2->firstElement->prevElement = list1->firstElement->prevElement;
        list1->size = len(list1);
        list2->size = len(list2);
    }
}

// returs time needed to get element under given index
double find(List* list, int index){
    clock_t start = clock();

    int counter = index;
    if(list->firstElement==NULL) return 0;
    ListElement* node = list->firstElement;
    int listLen = list->size;
    if(index < listLen/2) {
        while (counter > 0) {
            node = node->nextElement;
            counter--;
        }
    } else{
        int maxIndex = listLen - 1;
        for(int i = maxIndex; i >=index; i--){
            node = node->prevElement;
        }
    }
    clock_t end = clock();
    double time = (double) (end - start) / (double) CLOCKS_PER_SEC;
    return time;
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
    for(int i = 0; i<10000; i++){
        int value = rand() % 1234;
        push(&list1, &value);
        value = rand() % 1234;
        push(&list2, &value);
    }
    printf("%d \n", list1.size);
    merge(&list1, &list2);
    printf("%d\n", list1.size);

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
