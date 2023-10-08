
#ifndef AOD_L4_DGRAPH_H
#define AOD_L4_DGRAPH_H

#include <cinttypes>
#include <vector>
#include <cstddef>

using namespace std;


class DGraph {
public:
    size_t no_verticies;
    pair<vector<uint64_t>, size_t>* adjList;

    explicit DGraph(size_t k, size_t i);
};


#endif //AOD_L4_DGRAPH_H
