#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

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
void cleanTextFile();

HEAP *constructHeapFromFP(FILE *fp);
void deconstructHeap(HEAP *heap);

HN *getNewNode(Datatype_h *word, int size);
HEAP *createHeap();
void insertNode(HEAP *h, HN *newNode);
void bubbleUp(HEAP *h, int theIndex);
void siftDown(HEAP *h, int parent);
HN *removeNode(HEAP *h);

#define MAXSIZE 10000
#define FILE_PATH "/home/students/vanorsdc/CS332 Final Project/CProgram/clean_text.txt"
int main() {
    FILE* fp = fopen(FILE_PATH, "r");
    if (fp == NULL) {
        perror(FILE_PATH);
        return 1;
    }
    HEAP* heap = constructHeapFromFP(fp);
    fputc('\n', stdout);
    fclose(fp);
    deconstructHeap(heap);
    return 0;
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

/***********************************************************************************/
HN *getNewNode(Datatype_h *word, int size)
{
    HN *temp = (HN*)malloc(sizeof(HN));

    temp->word = strcpy(malloc(size), word); // Assign key of new node to user's input
    temp->size = size; // Assign data of new node to user's input

    return temp;
}
/***********************************************************************************/
HEAP *createHeap()
{
    HEAP *h = (HEAP*)malloc(sizeof(HEAP));

    h->count = 1; // initialize heap's array index to 1 (not 0!)
    h->theHeap = (HN*)malloc(MAXSIZE*sizeof(HN));

    return h;
}
/***********************************************************************************/
void insertNode(HEAP *h, HN *newNode)
{
    if (h->count < MAXSIZE)
    {
        h->theHeap[h->count] = *newNode; // insert new node into heap array
        bubbleUp(h, h->count); // sort until heap properties are met
        h->count++; // increment size of heap for indexing
    }
}
/***********************************************************************************/
void bubbleUp(HEAP *h, int theIndex)
{
    HN tempNode;
    int parent = (theIndex) / 2; // as per algorithm on slide 9

    if (h->theHeap[parent].size > h->theHeap[theIndex].size)
    {
        tempNode = h->theHeap[parent]; // set temp to current parent
        h->theHeap[parent] = h->theHeap[theIndex]; // new parent is now index from call
        h->theHeap[theIndex] = tempNode; // index from call is now old parent

        bubbleUp(h, parent); // recursive call until parent is minimum
    }
}
/***********************************************************************************/
void siftDown(HEAP *h, int parent)
{
    HN tempNode;
    int leftIndex = parent * 2; // left child of parent
    int rightIndex = parent * 2 + 1; // right child of parent
    int smallest; // smallest value of heap

    if ((leftIndex < 1) || (leftIndex >= h->count)) // if there is no left child
        leftIndex = 0;
    if ((rightIndex < 1) || (rightIndex >= h->count)) // if there is no right child
        rightIndex = 0;

    if ((leftIndex != 0) && (h->theHeap[parent].size > h->theHeap[leftIndex].size)) // if left child is less than parent
        smallest = leftIndex; // left child is minimum
    else
        smallest = parent; // otherwise, parent is already minimum

    if ((rightIndex != 0) && (h->theHeap[smallest].size > h->theHeap[rightIndex].size)) // if right child is less than parent
        smallest = rightIndex; // right child is minimum

    if (parent != smallest) // if parent isn't already minimum
    {
        tempNode = h->theHeap[smallest]; // set temp node to the minimum node as determined by above indexing
        h->theHeap[smallest] = h->theHeap[parent]; // set min node to parent node
        h->theHeap[parent] = tempNode; // set the parent to tempNode

        siftDown(h, smallest); // recursive call until parent is minimum
    }
}
/***********************************************************************************/
HN *removeNode(HEAP *h)
{
    HN *removedNode = (HN*)malloc(sizeof(HN));

    if (h->count == 1) // simple error-checking to see if heap has any nodes remaining before removal
    {
        printf("\nCannot remove - heap is empty\n");
        return removedNode;
    }
    *removedNode = h->theHeap[1]; // store removed node for return
    h->theHeap[1] = h->theHeap[h->count-1]; // decrement index of heap
    h->count--; // decrement size of heap

    siftDown(h, 1); // recursive call until heap properties are met

    return removedNode;
}

void cleanTextFile() {
    FILE* fp = fopen(FILE_PATH, "r");
    if (fp == NULL) {
        perror(FILE_PATH);
        return;
    }
    Datatype_h letter[1024];
    while (fscanf(fp, " %1023s", letter) == 1) {
        int i, j, len;
        for (i = 0, j = 0, len = (int) strlen(letter); i < len; i++) {
            if (!ispunct(letter[i])) {
                letter[j] = tolower(letter[i]);
                j++;
            }
        }
        letter[j] = '\0';
        if (j > 0) {
            printf("%s\n", letter);
        }
    }
    fclose(fp);
}