//
// Created by sebastian on 10/11/23.
//

#include "Graph.h"

void Graph::addEdge(uint16_t src, uint16_t dest, uint16_t weight) {
    this->adjList[src].push_back(new Edge(dest, weight));
}

Graph::Graph(size_t no_verticies) {
    this->no_verticies = no_verticies;
    this->adjList = new list<Edge*>[no_verticies];
}

Graph::~Graph() {
    delete[] adjList;
}
