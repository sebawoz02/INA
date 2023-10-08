#include <iostream>
#include <cstring>
#include <queue>

#include "Graph_Representation.h"
#include "data_loader.h"


std::vector<int> dial(Graph *graph, int source){
    std::vector<std::pair<int, std::list<int>::iterator>> dist(graph->no_verticies);
    for (size_t i = 0; i < graph->no_verticies; ++i) {
        dist[i].first = std::numeric_limits<int>::max();
    }
    dist[source].first = 0;
    std::vector<std::list<int>> buckets(graph->max_weight*3);
    buckets[0].push_back(source);
    long unsigned long idx = 0;
    while(1) {
        while (buckets[idx].size() == 0 && idx < buckets.size())
            idx++;
        if(idx == buckets.size())
            break;

        int u = buckets[idx].front();
        buckets[idx].pop_front();

        for(auto i = graph->adj[u].begin(); i != graph->adj[u].end(); ++i){
            int v = (*i).first;
            int weight = (*i).second;

            int du = dist[u].first;
            int dv = dist[v].first;

            if(dv > du + weight) {
                if (dv != std::numeric_limits<int>::max()) {
                    buckets[dv].erase(dist[v].second);
                }
                dist[v].first = du + weight;
                dv = dist[v].first;
                if(dv > buckets.size()){
                    buckets.resize(dv+graph->max_weight*2);
                }
                buckets[dv].push_front(v);
                dist[v].second = buckets[dv].begin();
            }
        }
    }

    std::vector<int> d(graph->no_verticies);
    for(size_t i = 0; i < graph->no_verticies; ++i){
        d[i] = dist[i].first;
    }
    return d;
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
            std::vector<int> dist = dial(g, s);
            src.pop_back();
        }
    }
    else if(mode == 2){
        while(!src.empty()){
            int dest = src.back();
            src.pop_back();
            int s = src.back();
            std::vector<int> dist =  dial(g, s);
            src.pop_back();
            std::cout << dist[dest] << std::endl;
        }
    }

    return 0;
}
