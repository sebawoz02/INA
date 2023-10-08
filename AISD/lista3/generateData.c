#include <time.h>
#include <stdlib.h>
#include <stdio.h>

int main(int argc, char** argv){
    if(argc < 2) return -1;
    FILE *tmpfile = fopen("tmpdata.txt", "w");
    if (tmpfile == NULL) {
        printf("Nie mozna utworzyc pliku.\n");
        return -1;
    }
    srand(time(NULL));
    size_t n = atoi(argv[1]);
    fprintf(tmpfile,"%zu\n", n);
    for(size_t i = 0; i < n; ++i){
       fprintf(tmpfile,"%zu\n", random() % (2*n - 1));
    }
    fclose(tmpfile);
    return 0;
}