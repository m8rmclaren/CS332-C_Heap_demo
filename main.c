#include <stdio.h>
#include <stdlib.h>

typedef struct hNode {
    struct hNode *leftNode, *rightNode;
    char* word;
    int frequency;
} HN;

void insertHeap(HN **heap, HN *newNode);
HN *removeHeap(HN **heap);
HN *getNewNode(char* word, int frequency);

int main() {
    printf("Hello, World!\n");
    return 0;
}

void insertHeap(HN **heap, HN *newNode) {
    HN *node = newNode;
    ++heap[0]->frequency;
    int count = heap[0]->frequency;
    int search = 1;
    heap[count] = node;

    while (search == 1) {
        if (count % 2 == 0) {
            if (heap[count/2]->frequency > node->frequency && count/2 > 0) {
                HN *temp = heap[count / 2];
                heap[count / 2] = heap[count];
                heap[count] = temp;
                count /= 2;
            } else
                search = 0;
        } else {
            if (heap[(count-1)/2]->frequency > node->frequency && (count-1)/2 > 0) {
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

    if (heap[0]->frequency < 1) {
        printf("Heap is empty!\n");
        return heap[0];
    }

    HN *returnVal = heap[1];

    heap[1] = heap[heap[0]->frequency];
    --heap[0]->frequency;

    while (search == 1 && heap[0]->frequency > 0) {
        if (index*2 > heap[0]->frequency)
            search = 0;
        else if (heap[index * 2]->frequency < heap[(index * 2) + 1]->frequency && heap[index * 2]->frequency < heap[index]->frequency && index*2 < heap[0]->frequency) {
            HN *temp = heap[index * 2];
            heap[index * 2] = heap[index];
            heap[index] = temp;
            index *= 2;

        } else if (heap[index * 2]->frequency > heap[(index * 2) + 1]->frequency && heap[(index * 2) + 1]->frequency < heap[index]->frequency && index*2 < heap[0]->frequency) {
            HN *temp = heap[(index * 2) + 1];
            heap[(index * 2) + 1] = heap[index];
            heap[index] = temp;
            index *= 2;
            index += 1;
        } else {
            heap[heap[0]->frequency+1] = NULL;
            return returnVal;
        }
    }
    heap[heap[0]->frequency+1] = NULL;
    return returnVal;
}

HN *getNewNode(char* word, int frequency) {
    HN *new = (HN*)malloc(sizeof(HN));
    new->leftNode = new->rightNode = NULL;
    new->word = word;
    new->frequency = frequency;

    return new;
}