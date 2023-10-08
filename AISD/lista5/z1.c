#include <stdio.h>
#include <stdint.h>

int64_t max(int64_t a, int64_t b){  return a > b ? a : b;   }

/**
 * Function that finds Longest Common Subsequence. O(m*n)
 * @param X - sequence 1
 * @param Y - sequence 2
 * @param m - length of X
 * @param n - length of Y
 * @return Longest Common Subsequence
 */
size_t lcs(char* X, char* Y, size_t m, size_t n)
{
    size_t L[m + 1][n + 1];
    for(size_t i = 0; i <= m; ++i)
    {
        for (size_t j = 0; j <= n; ++j) {
            if( i == 0 || j == 0)
                L[i][j] = 0;
            else if( X[i-1] == Y[j-1] )
                L[i][j] = L[i - 1][j - 1] + 1;
            else
                L[i][j] = max(L[i - 1][j], L[i][j - 1]);
        }
    }
    return L[m][n];
}

int main(void)
{
    char X[] = "GAAARG";
    char Y[] = "PAaAR";
    printf("%zu\n", lcs(X, Y, 6, 5));

    return 0;
}
