/**********************************************************************
// Authors: Hayden Roszell, Colton Van Orsdel
// File:    main.c
// Purpose: Calls all functions to perform the heapsort operation
**********************************************************************/
#include "c_heapsort.h"

// Compile using: gcc -o c_heapsort main.c c_heapsort.c -I.

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