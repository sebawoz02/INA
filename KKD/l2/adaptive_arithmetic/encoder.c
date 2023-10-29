#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>

#define NO_SYMBOLS 256  // 0-255 ascii

/** FUNCTION DECLARATIONS **/

// Functions used in encoding
void insert_to_buffer(uint8_t* bit_buffer, size_t* bit_buff_idx, uint64_t* output_size, uint8_t bit, FILE* output);

// Other
int parse_arguments(int argc, char* argv[], FILE** input, FILE** output);

/** FUNCTION DEFINITIONS **/

int parse_arguments(int argc, char* argv[], FILE** input, FILE** output)
{
    if(argc < 3)
    {
        printf("Usage: ./encoder.out <input_file> <output_file>\n");
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

void insert_to_buffer(uint8_t* bit_buffer, size_t* bit_buff_idx, uint64_t* output_size, uint8_t bit, FILE* output)
{
    if(bit)
        *bit_buffer |= (1<<(*bit_buff_idx));
    else
        *bit_buffer &= ~(1<<(*bit_buff_idx));
    if(*bit_buff_idx == 0)
    {
        fwrite(bit_buffer, 1, 1, output);
        *bit_buff_idx = 7;
        *bit_buffer = 0;
        (*output_size)++;
    }
    else
        (*bit_buff_idx)--;
}

/** MAIN FUNCTION **/

int main(int argc, char* argv[]) {

    // ------------- INIT ----------------

    FILE* input = NULL;
    FILE* output = NULL;
    if(parse_arguments(argc, argv, &input, &output) != 0)
        return 1;

    uint64_t* occur = calloc(NO_SYMBOLS, sizeof(uint64_t));
    for(size_t i = 0; i < NO_SYMBOLS; i++)
        occur[i]++;
    uint64_t input_size = NO_SYMBOLS;
    uint64_t output_size = 0;

    // Encode
    const uint64_t precision = 32;
    const uint64_t whole = (uint64_t)1 << precision;
    const uint64_t half = whole / 2;
    const uint64_t quart = whole / 4;

    uint8_t bit_buffer = 0;         // Bit buffer
    size_t bit_buff_idx = 7;        // Index in buffer
    uint64_t l = 0;                 // Lower limit
    uint64_t u = whole;             // Upper limit
    int16_t scale = 0;              // Scale

    // ------------- MAIN LOOP ----------------

    int16_t sym = 0;
    while(fread(&sym, 1, 1, input) == 1)
    {
        uint64_t cumulative_count = 0;
        for(int16_t i = 0; i < sym; i++)
            cumulative_count += occur[i];

        // Update low and up
        uint64_t dist = u - l;
        u = l + (uint64_t) roundl(dist * ((double)(cumulative_count + occur[sym])/input_size));
        l = l + (uint64_t) roundl(dist * ((double)cumulative_count/input_size));

        while(true)
        {
            if(u < half)
            {
                insert_to_buffer(&bit_buffer, &bit_buff_idx, &output_size, 0, output);
                while(scale > 0)
                {
                    insert_to_buffer(&bit_buffer, &bit_buff_idx, &output_size, 1, output);
                    scale--;
                }
            } else if(l > half){
                l -= half;
                u -= half;
                insert_to_buffer(&bit_buffer, &bit_buff_idx, &output_size, 1, output);
                while(scale > 0)
                {
                    insert_to_buffer(&bit_buffer, &bit_buff_idx, &output_size, 0, output);
                    scale--;
                }
            } else if(l > quart && u < 3*quart){
                l -= quart;
                u -= quart;
                scale++;
            } else
                break;
            u <<= 1;
            l <<= 1;
        }
        occur[sym]++;
        input_size++;
    }
    // EOF
    scale++;
    if( l <= quart )
    {
        insert_to_buffer(&bit_buffer, &bit_buff_idx, &output_size, 0, output);
        while(scale > 0)
        {
            insert_to_buffer(&bit_buffer, &bit_buff_idx, &output_size, 1, output);
            scale--;
        }
    } else{
        insert_to_buffer(&bit_buffer, &bit_buff_idx, &output_size, 1, output);
        while(scale > 0)
        {
            insert_to_buffer(&bit_buffer, &bit_buff_idx, &output_size, 0, output);
            scale--;
        }
    }
    // Flush buffer
    while (bit_buff_idx != 7)
        insert_to_buffer(&bit_buffer, &bit_buff_idx, &output_size, 0, output);


    // Cleanup
    fclose(output);
    fclose(input);

    // ------------- PRINT STATS ----------------

    double entropy = 0.0;
    for(size_t i = 0; i < NO_SYMBOLS; i++)
    {
        if(occur[i] - 1 > 0)
        {
            double p = (double)(occur[i] - 1)/(double)(input_size - NO_SYMBOLS);
            entropy -= p * log2(p);
        }
    }
    free(occur);

    double avg_len = ((double)input_size - (double)NO_SYMBOLS) / (double) output_size;
    printf("Srednia dlugosc kodowania: %f\n", 8.0/avg_len);
    printf("Stopien kompresji: %f\n", avg_len);
    printf("Entropia: %f\n", entropy);

    return 0;
}
