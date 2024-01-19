#pragma once

#include <vector>
#include <cstdint>
#include <island.h>
#include <cstddef>


struct GA
{
public:
    explicit GA(Graph* g);
    ~GA();

    void solve();

    void generate_random_island();
    void generate_mst_island();

private:
    std::vector<Island*> islands;
    Graph* graph;
};
