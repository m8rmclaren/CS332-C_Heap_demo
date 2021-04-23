#include <stdio.h>
#include <stdlib.h>

typedef char Datatype_h; // Typedef char for datatype, demonstrate extensibility of C

typedef struct hNode {
    struct hNode *leftNode, *rightNode;
    Datatype_h* word;
    int size;
} HN;

void insertHeap(HN **heap, HN *newNode);
HN *removeHeap(HN **heap);
HN *getNewNode(Datatype_h* word, int size);



int main() {
    printf("Hello, World!\n");
    return 0;
}

void insertHeap(HN **heap, HN *newNode) {
    HN *node = newNode;
    ++heap[0]->size;
    int count = heap[0]->size;
    int search = 1;
    heap[count] = node;

    while (search == 1) {
        if (count % 2 == 0) {
            if (heap[count/2]->size > node->size && count/2 > 0) {
                HN *temp = heap[count / 2];
                heap[count / 2] = heap[count];
                heap[count] = temp;
                count /= 2;
            } else
                search = 0;
        } else {
            if (heap[(count-1)/2]->size > node->size && (count-1)/2 > 0) {
                HN *temp = heap[(count - 1) / 2];
                heap[(count-1)/2] = heap[count];
                heap[count] = temp;
                count -= 1;
                count /= 2;
            } else
                search = 0;
        }
    }
}

HN *removeHeap(HN **heap) {
    int index = 1;
    int search = 1;

    if (heap[0]->size < 1) {
        printf("Heap is empty!\n");
        return heap[0];
    }

    HN *returnVal = heap[1];

    heap[1] = heap[heap[0]->size];
    --heap[0]->size;

    while (search == 1 && heap[0]->size > 0) {
        if (index*2 > heap[0]->size)
            search = 0;
        else if (heap[index * 2]->size < heap[(index * 2) + 1]->size && heap[index * 2]->size < heap[index]->size && index*2 < heap[0]->size) {
            HN *temp = heap[index * 2];
            heap[index * 2] = heap[index];
            heap[index] = temp;
            index *= 2;

        } else if (heap[index * 2]->size > heap[(index * 2) + 1]->size && heap[(index * 2) + 1]->size < heap[index]->size && index*2 < heap[0]->size) {
            HN *temp = heap[(index * 2) + 1];
            heap[(index * 2) + 1] = heap[index];
            heap[index] = temp;
            index *= 2;
            index += 1;
        } else {
            heap[heap[0]->size+1] = NULL;
            return returnVal;
        }
    }
    heap[heap[0]->size+1] = NULL;
    return returnVal;
}

HN *getNewNode(Datatype_h* word, int size) {
    HN *new = (HN*)malloc(sizeof(HN));
    new->leftNode = new->rightNode = NULL;
    new->word = word;
    new->size = size;

    return new;
}
