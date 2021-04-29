//
// Created by Hayden Roszell on 4/28/21.
//

#ifndef C_HEAP_CLEAN_H
#define C_HEAP_CLEAN_H

#include <stdio.h>
#include <ctype.h>
#include <string.h>

typedef char Datatype_h; // Typedef char for datatype, demonstrate extensibility of C
#define FILE_PATH "/home/students/vanorsdc/CS332 Final Project/CProgram/clean_text.txt"

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
#endif //C_HEAP_CLEAN_H
