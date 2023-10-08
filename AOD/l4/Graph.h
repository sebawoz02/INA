#ifndef AOD_L4_GRAPH_H
#define AOD_L4_GRAPH_H
#include <list>
#include <cstdint>

using namespace std;

class Edge{
public:
    uint16_t dest;
    uint16_t capacity;
    uint16_t flow; // current flow

    Edge(uint16_t dest, uint16_t capacity);
};

class Graph {
public:
    size_t no_verticies;
    list<Edge*>* adjList;

    explicit Graph(size_t k);

    bool bfs(uint16_t s, uint16_t d, vector<uint16_t>& parent) const;

};

#endif //AOD_L4_GRAPH_H
