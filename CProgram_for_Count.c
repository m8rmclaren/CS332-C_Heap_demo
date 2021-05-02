#include "c_heapsort.h"
int main() {
    struct timeval startTime, endTime;
    gettimeofday(&startTime, 0);
    FILE* fp = fopen(FILE_PATH, "r");
    if (fp == NULL) {
        perror(FILE_PATH);
        return 1;
    }
    HEAP* heap = constructHeapFromFP(fp);
    fputc('\n', stdout);
    fclose(fp);
    deconstructHeap(heap);
    gettimeofday(&endTime, 0);
    long seconds = endTime.tv_sec - startTime.tv_sec;
    long microseconds = endTime.tv_usec - startTime.tv_usec;
    double elapsed = seconds + microseconds*1e-6;
    printf("Runtime = %li microseconds.\n", microseconds);
    printf("Runtime = %f seconds.\n", elapsed);
    return 0;
}
#include "c_heapsort.h"
HN *getNewNode(Datatype_h *word, int size) {
    HN *temp = (HN*)malloc(sizeof(HN));
    temp->word = strcpy(malloc(size), word);
    temp->size = size;
    return temp;
}
HEAP *createHeap() {
    HEAP *h = (HEAP*)malloc(sizeof(HEAP));
    h->count = 1;
    h->theHeap = (HN*)malloc(MAXSIZE*sizeof(HN));
    return h;
}
void insertNode(HEAP *h, HN *newNode) {
    if (h->count < MAXSIZE) {
        h->theHeap[h->count] = *newNode;
        bubbleUp(h, h->count);
        h->count++;
    }
}
void bubbleUp(HEAP *h, int theIndex) {
    HN tempNode;
    int parent = (theIndex) / 2;
    if (h->theHeap[parent].size > h->theHeap[theIndex].size) {
        tempNode = h->theHeap[parent];
        h->theHeap[parent] = h->theHeap[theIndex];
        h->theHeap[theIndex] = tempNode;
        bubbleUp(h, parent);
    }
}
void siftDown(HEAP *h, int parent) {
    HN tempNode;
    int leftIndex = parent * 2;
    int rightIndex = parent * 2 + 1;
    int smallest;
    if ((leftIndex < 1) || (leftIndex >= h->count))
        leftIndex = 0;
    if ((rightIndex < 1) || (rightIndex >= h->count))
        rightIndex = 0;
    if ((leftIndex != 0) && (h->theHeap[parent].size > h->theHeap[leftIndex].size))
        smallest = leftIndex;
    else
        smallest = parent;
    if ((rightIndex != 0) && (h->theHeap[smallest].size > h->theHeap[rightIndex].size))
        smallest = rightIndex;
    if (parent != smallest) {
        tempNode = h->theHeap[smallest];
        h->theHeap[smallest] = h->theHeap[parent];
        h->theHeap[parent] = tempNode;
        siftDown(h, smallest);
    }
}
HN *removeNode(HEAP *h) {
    HN *removedNode = (HN*)malloc(sizeof(HN));
    if (h->count == 1) {
        printf("\nCannot remove - heap is empty\n");
        return removedNode;
    }
    *removedNode = h->theHeap[1];
    h->theHeap[1] = h->theHeap[h->count-1];
    h->count--;
    siftDown(h, 1);
    return removedNode;
}
void deconstructHeap(HEAP *heap) {
    while (heap->count > 1) {
        HN* temp = removeNode(heap);
        printf("%d %s\n",temp->size, temp->word);
    }
}
HEAP *constructHeapFromFP(FILE *fp) {
    Datatype_h letter[1024];
    HEAP* heap = createHeap();
    while (fscanf(fp, " %1023s", letter) == 1) {
        int i;
        for (i = 0; letter[i] != '\0'; i++);
        insertNode(heap, getNewNode(letter, i));
    }
    return heap;
}
#ifndef C_HEAP_H
#define C_HEAP_H
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <sys/time.h>
#define MAXSIZE 10000
#define FILE_PATH "./clean_text.txt"
typedef char Datatype_h;
typedef struct hNode {
    Datatype_h *word;
    int size;
}   HN;
typedef struct littleEndUpHeap {
    HN *theHeap;
    int count;
}   HEAP;
HN *getNewNode(Datatype_h *word, int size);
HEAP *createHeap();
void insertNode(HEAP *h, HN *newNode);
void bubbleUp(HEAP *h, int theIndex);
void siftDown(HEAP *h, int parent);
HN *removeNode(HEAP *h);
HEAP *constructHeapFromFP(FILE *fp);
void deconstructHeap(HEAP *heap);
#endif