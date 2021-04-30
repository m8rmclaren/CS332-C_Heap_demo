/**********************************************************************
// Authors: Colton Van Orsdel, Hayden Roszell
// File:    c_heapsort.h
// Purpose: Includes the function prototypes and global variables for
//          the C version of the heapsort algorithm
**********************************************************************/
#ifndef C_HEAP_H
#define C_HEAP_H

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

#define MAXSIZE 10000
#define FILE_PATH "/home/students/vanorsdc/CS332 Final Project/CProgram/clean_text.txt"

typedef char Datatype_h; // Typedef char for datatype, demonstrate extensibility of C

typedef struct hNode
{
    Datatype_h *word;
    int size;
}	HN;

typedef struct littleEndUpHeap{
    HN *theHeap; // heap array to store HN data
    int count; // current size of HN 
}	HEAP;

HEAP *constructHeapFromFP(FILE *fp);
void deconstructHeap(HEAP *heap);
HN *getNewNode(Datatype_h *word, int size);
HEAP *createHeap();
void insertNode(HEAP *h, HN *newNode);
void bubbleUp(HEAP *h, int theIndex);
void siftDown(HEAP *h, int parent);
HN *removeNode(HEAP *h);

#endif // C_HEAP_H