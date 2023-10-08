
#ifndef AOD_L4_HOPCROFKARP_H
#define AOD_L4_HOPCROFKARP_H

#include <iostream>
#include <vector>
#include <queue>
#include <climits>
#include <inttypes.h>

#include "DGraph.h"

using namespace std;

class HopcroftKarp {
    size_t n; // Liczba wierzchołków w lewej czesci
    size_t m; // Liczba wierzcholkow w prawej czesci
    DGraph* graph;
    vector<uint64_t> dist;
    vector<uint64_t> matching; // Skojarzenia
    const uint64_t NIL = 0; // Wartość oznaczająca brak skojarzenia
    const uint64_t INF = INT_MAX;

public:
    HopcroftKarp(DGraph* g, size_t n);

    bool bfs();

    bool dfs(uint64_t u);

    int findMaxMatching();
};


#endif //AOD_L4_HOPCROFKARP_H
