/**********************************************************************
// Authors: Hayden Roszell, Colton Van Orsdel
// File:    cleanTextFile.h
// Purpose: Includes the function prototype and global variables for
//          the cleanTextFile.c function
**********************************************************************/
#ifndef C_HEAP_CLEAN_H
#define C_HEAP_CLEAN_H

#include <stdio.h>
#include <string.h>

typedef char Datatype_h; // Typedef char for datatype, demonstrate extensibility of C

#define FILE_PATH_FROM "./text.txt"
#define FILE_PATH_TO "./clean_text.txt"

void cleanTextFile();

#endif //C_HEAP_CLEAN_H
