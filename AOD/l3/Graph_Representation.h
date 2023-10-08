#ifndef AOD_EDGE_H
#define AOD_EDGE_H

#include <vector>
#include <limits>
#include <cstddef>
#include <list>

typedef std::pair<int, int> iPair;


class Graph{
public:
    std::list<std::pair<int, int> >* adj;   // It's like an adjecency list
    size_t no_verticies;
    int max_weight;
    explicit Graph(size_t no_verticies);
    void addEdge(int src, int dest, int w);
};


#endif //AOD_EDGE_H
