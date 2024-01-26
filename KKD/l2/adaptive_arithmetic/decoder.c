#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <math.h>
#include <stdbool.h>

#define NO_SYMBOLS 256  // 0-255 ascii

static bool eof = false;

int parse_arguments(int argc, char* argv[], FILE** input, FILE** output);
uint8_t get_bit(uint8_t* byte, uint8_t* bit_index, FILE* input);
void decode(FILE* input, FILE* output);

int parse_arguments(int argc, char* argv[], FILE** input, FILE** output)
{
    if(argc < 3)
    {
        printf("Usage: ./decoder.out <input_file> <output_file>\n");
        return 1;
    }
    const char* input_name = argv[1];
    const char* output_name = argv[2];

    // Prepare
    *input = fopen(input_name, "rb");
    if(*input == NULL)
    {
        printf("Could not open file %s\n", input_name);
        return 1;
    }
    *output = fopen(output_name, "wb+");
    if(*output == NULL)
    {
        printf("Could not open file %s\n", output_name);
        fclose(*input);
        return 1;
    }
    return 0;
}

uint8_t get_bit(uint8_t* byte, uint8_t* bit_index, FILE* input)
{
    uint8_t bit = (*byte >> *bit_index) & 1;
    if(*bit_index == 0)
    {
        if(fread(byte, 1, 1, input) == 0)
        {
            eof = true;
        }
        *bit_index = 7;
    } else (*bit_index)--;
    return bit;
}

void decode(FILE* input, FILE* output)
{
    const uint64_t precision = 32;
    const uint64_t whole = (uint64_t)1 << precision;
    const uint64_t half = whole / 2;
    const uint64_t quart = whole / 4;

    uint64_t *occur = calloc(NO_SYMBOLS, sizeof(uint64_t));
    for (size_t i = 0; i < NO_SYMBOLS; i++)
        occur[i]++;                     // Set occurrence of each symbol to 1
    uint64_t input_size = NO_SYMBOLS;   // Total number of symbols
    uint64_t z = 0;                     // "Znacznik"

    uint8_t byte;                       // Byte_buffer
    fread(&byte, 1, 1, input);
    uint8_t decoded_sym;                // Decoded symbol
    uint8_t bit_index = 7;              // Bit index in byte_buffer

    uint64_t l = 0;                     // Lower limit
    uint64_t u = whole;                 // Upper limit

    uint64_t i = 1;
    while(i <= precision && !eof)
    {
        if(get_bit(&byte, &bit_index, input) != 0)
            z += pow(2, precision - i);
        i++;
    }

    // ------------- MAIN LOOP ----------------

    while(true){
        uint64_t cumulative_sum = 0;
        for(uint16_t sym = 0; sym < NO_SYMBOLS; sym++){
            uint64_t dist = u - l;
            cumulative_sum += occur[sym];

            uint64_t new_u = l + (uint64_t) roundl(dist * ((double)(cumulative_sum)/input_size));
            uint64_t new_l = l + (uint64_t) roundl(dist * ((double)(cumulative_sum - occur[sym])/input_size));

            if(new_l <= z && z < new_u){
                decoded_sym = sym;
                input_size++;
                occur[sym]++;
                fwrite(&decoded_sym, 1, 1, output);
                l = new_l;
                u = new_u;
                break;
            }
        }

        if(eof)
            break;

        while(true){
            if(u < half){

            }else if(l > half){
                l -= half;
                u -= half;
                z -= half;
            }else if(l > quart && u < 3 * quart){
                l -= quart;
                u -= quart;
                z -= quart;
            }
            else
                break;

            l <<= 1;
            u <<= 1;
            z <<= 1;
            if(!eof && get_bit(&byte, &bit_index, input))
                z += 1;
        }
    }
    free(occur);
}

int main(int argc, char* argv[]) {

    // ------------- INIT ----------------

    FILE *input = NULL;
    FILE *output = NULL;
    if (parse_arguments(argc, argv, &input, &output) != 0)
        return 1;

    decode(input, output);

    fclose(input);
    fclose(output);
    return 0;
}