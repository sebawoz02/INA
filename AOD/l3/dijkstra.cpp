#include <iostream>
#include <cstring>
#include <queue>

#include "Graph_Representation.h"
#include "data_loader.h"



std::vector<int> dijkstra(Graph *graph, int source)
{
   std::priority_queue<iPair, std::vector<iPair>, std::greater<>> pq;

   std::vector<int> dist(graph->no_verticies, std::numeric_limits<int>::max());

   pq.emplace(0, source);
   dist[source] = 0;

    while (!pq.empty()){
        int u = pq.top().second;
        pq.pop();
        std::list<std::pair<int, int>>::iterator i;
        for (i = graph->adj[u].begin(); i != graph->adj[u].end(); ++i) {
            // Get vertex label and weight of current
            // adjacent of u.
            int v = (*i).first;
            int weight = (*i).second;

            // If there is shorted path to v through u.
            if (dist[v] > dist[u] + weight) {
                // Updating distance of v
                dist[v] = dist[u] + weight;
                pq.emplace(dist[v], v);
            }
        }
    }
    return dist;
}

int main(int argc, char *argv[])
{
    int mode; // 1 - time measure, 2 - shortest path
    char* data_path;
    char* sources_path;
    char* results_path;

    // load_params
    if(argc < 5)
    {
        std::cout << "No parameters given" << std::endl;
        return -1;
    }
    for(size_t i = 1; i < 5; i+=2)
    {
        if(strcmp(argv[i], "-d") == 0)
        {
            data_path = argv[i+1];
        }
        else if(strcmp(argv[i], "-ss") == 0)
        {
            sources_path = argv[i+1];
            mode = 1;
        }
        else if(strcmp(argv[i], "-p2p") == 0)
        {
            sources_path = argv[i+1];
            mode = 2;
        }
    }
    Graph* g = create_graph_from_path(data_path);

    std::list<int> src = get_sources(sources_path, mode);

    if(mode == 1){
        while(!src.empty()){
            int s = src.back();
            std::vector<int> dist = dijkstra(g, s);
            src.pop_back();
        }
    }
    else if(mode == 2){
        while(!src.empty()){
            int dest = src.back();
            src.pop_back();
            int s = src.back();
            std::vector<int> dist =  dijkstra(g, s);
            src.pop_back();
            std::cout << dist[dest] << std::endl;
        }
    }

    return 0;
}
