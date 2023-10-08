
#include "Graph_Representation.h"


Graph::Graph(size_t no_verticies)
{
    this->no_verticies = no_verticies;
    this->adj = new std::list<iPair>[no_verticies];
    this->max_weight = 0;

}
void Graph::addEdge(int src, int dest, int w)
{
    this->adj[src].push_back(std::make_pair(dest, w));
    this->adj[dest].push_back(std::make_pair(src, w));
}
