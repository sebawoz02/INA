//
// Created by sebastian on 10/8/23.
//
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <math.h>
#include "entropy.h"

#define NO_POSSIBLE_SYMBOLS 256     // 0b00000000 - 0b11111111

static long double calculate_entropy(const uint32_t* freq_dict, const size_t no_symbols);
static long double calculate_cond_entropy(uint32_t** cond_freq_dict,
                                          const uint32_t* freq_dict,
                                          const size_t no_symbols);

static long double calculate_entropy(const uint32_t* freq_dict, const size_t no_symbols)
{
    long double ent = 0.0;      // H = -1 * sum(for(i in freq_dict){ p(i) * log2(p(i)) })
    const long double no_sym = (const long double) no_symbols;

    for(size_t i = 0; i < NO_POSSIBLE_SYMBOLS; i++)
    {
        if(freq_dict[i] == 0)
            continue;
        const long double p = (long double)freq_dict[i]/no_sym;     // p(freq_dict[i])
        ent += p * -log2l(p);
    }
    return ent;
}

static long double calculate_cond_entropy(uint32_t** cond_freq_dict,
                                          const uint32_t* freq_dict,
                                          const size_t no_symbols)
{
    long double ent = 0.0;
    const long double no_sym = (const long double) no_symbols;

    for(size_t i = 0; i< NO_POSSIBLE_SYMBOLS; i++)
    {
        if(freq_dict[i] == 0)
            continue;

        long double sum = 0.0;
        const long double freq = (const long double) freq_dict[i];
        for(size_t j = 0; j < NO_POSSIBLE_SYMBOLS; j++)
        {
            if(cond_freq_dict[i][j] == 0)
                continue;
            const long double p = (const long double)cond_freq_dict[i][j]/freq;
            sum += p * -log2l(p);
        }
        ent += freq/no_sym * sum;
    }

    return ent;
}

void scan_file(const char* file_name)
{
    // Open file
    FILE* fptr = fopen(file_name, "rb");
    if( fptr == NULL )
    {
        printf("Could not open file %s\n", file_name);
        return;
    }

    uint32_t* freq_dict = calloc(NO_POSSIBLE_SYMBOLS, sizeof(uint32_t));
    uint32_t** cond_freq_dict = malloc(sizeof(*cond_freq_dict)* NO_POSSIBLE_SYMBOLS);
    for(size_t i = 0; i < NO_POSSIBLE_SYMBOLS; i++) {
        uint32_t* ptr = calloc(NO_POSSIBLE_SYMBOLS, sizeof(uint32_t));
        cond_freq_dict[i] = ptr;
    }
    size_t no_symbols = 0;      // Number of different 8-bit symbols found in binary file
    int16_t curr_symbol;
    int16_t prev_symbol = 0;

    while((curr_symbol = fgetc(fptr)) != EOF)
    {
        if(freq_dict[curr_symbol] == 0)
            no_symbols++;
        freq_dict[curr_symbol]++;
        cond_freq_dict[prev_symbol][curr_symbol]++;
        prev_symbol = curr_symbol;
    }

    const long double entropy = calculate_entropy(freq_dict, no_symbols);
    const long double cond_entropy = calculate_cond_entropy(cond_freq_dict, freq_dict, no_symbols);
    printf("Entropy: %Lf\n", entropy);
    printf("Conditional entropy: %Lf\n", cond_entropy);

    // Free up resources
    fclose(fptr);
    free(freq_dict);
    for(size_t i = 0; i < NO_POSSIBLE_SYMBOLS; i++)
        free(cond_freq_dict[i]);
    free(cond_freq_dict);
}
