
#include "finite_automa.h"

int main(int argc, char* argv[])
{
    if(argc < 3)
    {
        printf("\x1b[31mProvide the necessary parameters.\x1b[0m\n");
        printf("Usage:\n./FA <pattern> <file_name>\n");
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

    finite_automata_search(pattern, file);

    fclose(file);

    return 0;
}
