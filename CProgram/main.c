/**********************************************************************
// Authors: Hayden Roszell, Colton Van Orsdel, Arseniy Gorbanev
// File:    main.c
// Purpose: Calls all functions to perform the heapsort operation
// Compile using: gcc -std=c11 -o c_heapsort main.c c_heapsort.c -I.
**********************************************************************/
#include "c_heapsort.h"

int main() {
    // timing start
    struct timeval startTime, endTime;
    gettimeofday(&startTime, NULL);
    
    FILE* fp = fopen(FILE_PATH, "r");
    if (fp == NULL) {
        perror(FILE_PATH);
        return 1;
    }
    HEAP* heap = constructHeapFromFP(fp);
    fputc('\n', stdout);
    fclose(fp);
    deconstructHeap(heap);
    
    // timing calculation
    gettimeofday(&endTime, NULL);
    long seconds = endTime.tv_sec - startTime.tv_sec;
    long microseconds = endTime.tv_usec - startTime.tv_usec;
    double elapsed = seconds + microseconds*1e-6;
    printf("Runtime = %li microseconds.\n", microseconds);
    printf("Runtime = %f seconds.\n", elapsed);
    
    return 0;
}