//
// Created by sebawoz02 on 20.12.23.
//

#ifndef L1_SIM_ANNEALING_H
#define L1_SIM_ANNEALING_H

#include <cstddef>
#include <data_structs/Graph.h>
#include <utility>

uint64_t sim_annealing(size_t* tsp, const Graph& g, uint64_t init_cost);

#endif //L1_SIM_ANNEALING_H
