//
// Created by sebastian on 10/11/23.
//

#ifndef L1_GRAPH_H
#define L1_GRAPH_H

#include <cstdlib>
#include <vector>
#include "Node.h"

class Graph {
public:
    size_t no_nodes;
    std::vector<Node*> nodes_list;
    uint32_t** dist_matrix;

    explicit Graph(size_t no_nodes);
    void addNode(uint16_t x, uint16_t y);
    void compute_matrix();
    ~Graph();
};


#endif //L1_GRAPH_H
