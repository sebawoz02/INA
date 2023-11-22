
#ifndef L1_LOCAL_SEARCH_H
#define L1_LOCAL_SEARCH_H

#include <utility>
#include <cstddef>
#include <data_structs/Graph.h>

std::pair<uint64_t, size_t> local_search(size_t* tsp, const Graph& g,
                                         uint64_t init_cost);

#endif //L1_LOCAL_SEARCH_H
