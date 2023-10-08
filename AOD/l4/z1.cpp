#include <iostream>
#include <cstring>
#include <queue>
#include <limits>
#include <cmath>

#include "Graph.h"

// PROGRAM SUPPORTS GRAPHS WITH THE NUMBER OF VERTICIES FROM 1 TO (2^16)-1

const uint64_t INF = numeric_limits<uint64_t>::max();

pair<uint64_t , uint64_t> Edmonds_Karp_algorithm(Graph g, uint16_t source, uint16_t dest){
    vector<uint16_t> parent(g.no_verticies, -1);
    uint64_t maxFlow = 0;
    uint64_t pathCount = 0;

    while (g.bfs(source, dest, parent)) {
        uint64_t pathFlow = INF;

        // Find the minimum residual capacity on the augmenting path
        for (uint64_t v = dest; v != source; v = parent[v]) {
            uint64_t u = parent[v];
            for (Edge* e : g.adjList[u]) {
                if (e->dest == v) {
                    pathFlow = (pathFlow > (uint64_t)(e->capacity - e->flow)) ? e->capacity - e->flow : pathFlow;
                    break;
                }
            }
        }

        // Update the flow and residual capacities
        for (uint64_t v = dest; v != source; v = parent[v]) {
            uint64_t u = parent[v];
            for (Edge* e : g.adjList[u]) {
                if (e->dest == v) {
                    e->flow += pathFlow;  // Forward edge
                    break;
                }
            }
            for (Edge* e : g.adjList[v]) {
                if (e->dest == u) {
                    e->flow -= pathFlow;  // Backward edge
                    break;
                }
            }
        }

        // Add the path flow to the total flow
        maxFlow += pathFlow;
        pathCount++;
    }

    return make_pair(maxFlow, pathCount);


}


int main(int argc, char** argv) {
    size_t k;
    bool printFlow = false;

    if(argc < 3) {
        cout << "Zbyt mała liczba parametrów" << endl;
        return 1;
    }
    for(int i = 1; i < argc; i++){
        if(strcmp(argv[i], "--size") == 0)
            k = strtol(argv[++i], nullptr, 10);
        else if(strcmp(argv[i], "--printFlow") == 0)
            printFlow = true;
    }
    clock_t start = clock();
    auto* g = new Graph(k);
    pair<uint64_t, uint64_t> results = Edmonds_Karp_algorithm(*g,0, g->no_verticies - 1);
    cout << "Maksymalny przepływ: " << results.first << endl;

    cerr << "Czas działania: "<< (double)((double) clock() - (double) start)/CLOCKS_PER_SEC <<"s"<< endl;
    cerr << "Sciezki powiekszające: " << results.second << endl;

    if(printFlow){
        cout << "Przepływ: " << endl;
        for(size_t i = 0; i<g->no_verticies; i++)
            for(Edge* e : g->adjList[i])
                cout << i << "->" << e->dest << " - " << e->flow << "/"<< e->capacity <<endl;

    }
    return 0;
}
