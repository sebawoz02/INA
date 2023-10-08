//
// Created by sebastian on 10/4/23.
//

#include "knuth_morris_pratt.h"

int main(int argc, char* argv[])
{
    if(argc < 3)
    {
        printf("\x1b[31mProvide the necessary parameters.\x1b[0m\n");
        printf("Usage:\n./KMP <pattern> <file_name>\n");
        return 0;
    }
    const char* pattern = argv[1];
    const char* filename = argv[2];

    FILE* file = fopen(filename, "r");
    if( file == NULL)
    {
        printf("Could not open file %s\n", filename);
        return 0;
    }

    knuth_morris_pratt_search(pattern, file);

    fclose(file);

    return 0;
}