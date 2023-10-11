//
// Created by sebastian on 10/11/23.
//

#ifndef L1_EDGE_H
#define L1_EDGE_H

#include <cstdint>

class Edge {
public:
    uint16_t dest;
    uint16_t weight;

    Edge(uint16_t dest, uint16_t weight);
};


#endif //L1_EDGE_H
