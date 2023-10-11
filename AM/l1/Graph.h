//
// Created by sebastian on 10/11/23.
//

#ifndef L1_GRAPH_H
#define L1_GRAPH_H

#include <list>
#include <cstdint>
#include "Edge.h"

using namespace std;

class Graph {
public:
    list<Edge*>* adjList;
    size_t no_verticies;

    explicit Graph(size_t no_verticies);
    void addEdge(uint16_t src, uint16_t dest, uint16_t weight);
    ~Graph();
};


#endif //L1_GRAPH_H
