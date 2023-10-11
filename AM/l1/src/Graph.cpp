//
// Created by sebastian on 10/11/23.
//

#include <cmath>
#include "Graph.h"

#define nint(x)((uint32_t) x+0.5)

/*
 * Function takes two nodes and calculates Euclidean distance between
 * them. L2-metric
 * */
uint32_t calculate_distance(Node* n1, Node* n2)
{
    uint32_t xd = n1->x - n2->x;
    uint32_t yd = n1->y - n2->y;
    uint32_t dij = nint( sqrt(xd*xd + yd*yd) );
    return dij;
}

Graph::Graph(size_t no_nodes) {
    this->no_nodes = no_nodes;
    this->dist_matrix = new uint32_t*[no_nodes];
    for (size_t i = 0; i < no_nodes; i++)
    {
        this->dist_matrix[i] = new uint32_t[no_nodes];
    }
}

Graph::~Graph() {

    for (size_t i = 0; i < no_nodes; i++)
    {
        delete[] this->dist_matrix[i];
        delete this->nodes_list[i];
    }
    delete[] this->dist_matrix;
}

void Graph::addNode(size_t id, uint16_t x, uint16_t y){
    this->nodes_list.push_back(new Node(id, x, y));
}

void Graph::compute_matrix(){
    for(size_t i = 0; i < this->no_nodes; i++)
    {
        for(size_t j = i + 1; j < this->no_nodes; j++)
        {
            this->dist_matrix[i][j] = this->dist_matrix[j][i] =
                    calculate_distance(this->nodes_list[i], this->nodes_list[j]);
        }
    }
}
