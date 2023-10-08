//
// Created by sebastian on 10/8/23.
//
#include <stdio.h>

#include "entropy.h"

int main(int argc, char* argv[])
{
    if(argc < 2)
    {
        printf("Usage: ./entropy.out <file_name>\n");
        return 0;
    }
    const char* file_name = argv[1];
    scan_file(file_name);
    return 0;
}