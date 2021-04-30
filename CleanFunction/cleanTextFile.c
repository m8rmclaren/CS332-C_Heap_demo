/**********************************************************************
// Authors: Hayden Roszell, Colton Van Orsdel
// File:    cleanTextFile.h
// Purpose: Reads in text from an input file and cleans it of all 
//			capitalization, punctuation, and spaces for use in heapsort
**********************************************************************/
#include "cleanTextFile.h"

int main() {
    printf("Cleaning text file: %s \n", FILE_PATH_FROM);
    cleanTextFile();
    printf("Text file: %s has been cleaned; \nsanitized text stored in %s \n", FILE_PATH_FROM, FILE_PATH_TO);
    return 0;
}

void cleanTextFile() {
    FILE* fp_f = fopen(FILE_PATH_FROM, "r"); // open the input file for reading
    if (fp_f == NULL) { // check for errors in accessing the file
        perror(FILE_PATH_FROM);
        return;
    }
    FILE* fp_t = fopen(FILE_PATH_TO, "w"); // open the output file for writing
    if (fp_t == NULL) { // check for errors in accessing the file
        perror(FILE_PATH_FROM);
        return;
    }
    Datatype_h letter[1024];
    while (fscanf(fp_f, " %1023s", letter) == 1) { // loop to remove capitlization, punctuation, and spaces
        int i, j, len;
        for (i = 0, j = 0, len = (int) strlen(letter); i < len; i++) {
            if (!ispunct(letter[i])) {
                letter[j] = tolower(letter[i]);
                j++;
            }
        }
        letter[j] = '\0';
        if (j > 0) {
            fprintf(fp_t, "%s\n", letter); // print sanitized text to output file (FILE_PATH_TO)
        }
    }
    fclose(fp_f); // close the input file
    fclose(fp_t); // close the output file
}