
#ifndef L1_TABU_SEARCH_H
#define L1_TABU_SEARCH_H

#include <cstddef>
#include <data_structs/Graph.h>
#include <utility>

std::pair<uint64_t, size_t> tabu_search(size_t* tsp, const Graph& g,
                                        uint64_t init_cost);

#endif //L1_TABU_SEARCH_H
