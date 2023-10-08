//
// Created by sebastian on 10/4/23.
//

#include "knuth_morris_pratt.h"

#include <string.h>
#include <stdint.h>

static void build_failure_function(const char* pattern, size_t pattern_len, size_t* failure_function);

static void build_failure_function(const char* pattern, size_t pattern_len, size_t * failure_function) {
    size_t length = 0;  // Length of the current prefix

    failure_function[0] = 0;  // The first character cannot be a proper prefix

    size_t i = 1;

    while (i < pattern_len) {
        if (pattern[i] == pattern[length]) {
            length++;
            failure_function[i] = length;
            i++;
        } else {
            if (length != 0) {
                length = failure_function[length - 1];
            } else {
                failure_function[i] = 0;
                i++;
            }
        }
    }
}

void knuth_morris_pratt_search(const char* pattern, FILE* file){
    size_t pattern_len = strlen(pattern);
    size_t line_len;
    char text[4096];  // Maximum linux file line length - 4096

    // The failure function at position j represents the length of the longest proper
    // prefix of the pattern that is also a proper suffix of the pattern up to position j.
    size_t failure_function[pattern_len];
    build_failure_function(pattern, pattern_len, failure_function);

    size_t line_num = 0;
    while (fgets(text, sizeof(text), file) != NULL) {
        line_num++;
        line_len = strlen(text);
        size_t i = 0, j = 0;   // i - cur char in text, j - cur char in pattern

        while (i < line_len) {
            if (pattern[j] == text[i]) {
                i++;
                j++;
            }

            if (j == pattern_len) {
                printf("Pattern '%s' found at index %zu in the line %zu\n", pattern, i - j, line_num - 1);
                j = failure_function[j - 1];
            } else if (i < line_len && pattern[j] != text[i]) { // Mismatch
                if (j != 0) {   // If j is not 0, update j using the failure function
                    j = failure_function[j - 1];
                } else {    // If j is 0, increment i
                    i++;
                }
            }
        }
    }
}
