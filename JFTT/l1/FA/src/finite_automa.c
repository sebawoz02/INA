//
// Created by sebastian on 10/4/23.
//

#include <string.h>
#include <stdint.h>

#include "finite_automa.h"

#define NO_OF_CHARS 256 // ASCII 0-255

static uint16_t compute_delta(const char* pattern, size_t no_states, size_t q, size_t x);
static void prepare_table_repr(const char* pattern, size_t no_states, uint16_t table_repr[][NO_OF_CHARS]);

static uint16_t compute_delta(const char* pattern, size_t no_states, size_t q, size_t x)
{
    if (q < no_states && x == pattern[q])
        return q+1;

    for (size_t longest_prefix_len = q; longest_prefix_len > 0; longest_prefix_len--)
    {
        if (pattern[longest_prefix_len - 1] == x)
        {
            size_t i;
            for (i = 0; i < longest_prefix_len - 1; i++)
                if (pattern[i] != pattern[q - longest_prefix_len + 1 + i])
                    break;
            if (i == longest_prefix_len - 1)
                return longest_prefix_len;
        }
    }
    return 0;
}

static void prepare_table_repr(const char* pattern, size_t no_states,
                               uint16_t table_repr[][NO_OF_CHARS])
{
    for(size_t q = 0; q <= no_states; q++)  // STATE
        for (size_t x = 0; x < NO_OF_CHARS; x++)    // LETTER OF ALPHABET
            table_repr[q][x] = compute_delta(pattern, no_states, q, x);
}

void finite_automata_search(const char* pattern, FILE* file)
{
    const size_t no_states = strlen(pattern);   // M
    if(no_states == 0){
        printf("Pattern cannot be empty!\n");
        return;
    }

    uint16_t table_repr[no_states + 1][NO_OF_CHARS];    // TRANSITION TABLE / DELTA
    prepare_table_repr(pattern, no_states, table_repr);

    size_t q = 0;   // STATE
    size_t line_number = 0;
    size_t char_index = 0;
    char ch;

    while ((ch = fgetc(file)) != EOF) {
        q = table_repr[q][ch];
        if (q == no_states) {
            printf("Pattern '%s' found at index %zu in the line %zu\n", pattern, char_index - no_states + 1, line_number);
            q = 0;
        }
        if (ch == '\n') {
            line_number++;
            char_index = 0;
        } else {
            char_index++;
        }
    }
}
