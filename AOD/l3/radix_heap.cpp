#include <iostream>
#include <cstring>
#include <queue>
#include <cmath>

#include "Graph_Representation.h"
#include "data_loader.h"

struct bucketsradix {
    std::list<int> v_list;
    int range_a;
    int range_b;
};

std::vector<int> dijkstra_radix(Graph *graph, int source){
    std::vector<int> dist(graph->no_verticies);
    for (size_t i = 0; i < graph->no_verticies; ++i) {
        dist[i] = std::numeric_limits<int>::max();
    }
    dist[source] = 0;
    size_t no_buck = log2(std::numeric_limits<int>::max());

    std::vector<bucketsradix> buckets(no_buck);

    // 0 - 0, 1 - 1, 2 - 2-3 , 3 - 4-7 ,...
    for(size_t i = 0; i< no_buck; i++){
        buckets[i].range_a = floor(pow(2, i - 1));
        buckets[i].range_b = pow(2, i) - 1;
    }
    buckets[0].v_list.push_front(source);
    long unsigned long idx;
    while (1){
        idx = 0;
        while(buckets[idx].v_list.size() == 0 && idx < buckets.size())
            idx++;
        if (idx == buckets.size())
            break;

        int u = buckets[idx].v_list.front();
        // Only remove front element if bucket width == 1
        if(buckets[idx].range_b - buckets[idx].range_a + 1 == 1){
            buckets[idx].v_list.pop_front();
        }else{
            int minv;
            int mindist = std::numeric_limits<int>::max();
            //find the smallest element
            for (auto v = buckets[idx].v_list.begin(); v != buckets[idx].v_list.end(); ++v) {
                if (dist[*v] < mindist) {
                    mindist = dist[*v];
                    minv = *v;
                }
            }
            u = minv;
            for(auto i = buckets[idx].v_list.begin(); i != buckets[idx].v_list.end(); i++) {
                if(*i == u) {
                    buckets[idx].v_list.erase(i);
                    break;
                }
            }
            //distribute the range of buckets
            for(int i = 0; i < idx; i++){
                buckets[i].range_a = mindist +  floor(pow(2, i - 1));
                buckets[i].range_b =  mindist + pow(2, i) - 1;
            }
            buckets[idx -1].range_b = buckets[idx].range_b;
            //determine the correct buckets
            for(auto v = buckets[idx].v_list.begin(); v != buckets[idx].v_list.end(); ++v){
                for(int i = idx - 1; i>=0; i--){
                    if(dist[*v] >= buckets[i].range_a && dist[*v] <= buckets[i].range_b){
                        buckets[i].v_list.push_front(*v);
                        break;
                    }
                }
            }
            // mark bucket as empty
            buckets[idx].range_a = 1;
            buckets[idx].range_b = 0;
            buckets[idx].v_list.clear();
        }

        for(auto i = graph->adj[u].begin(); i!=graph->adj[u].end(); i++){
            int v = (*i).first;
            int weight = (*i).second;
            int dv = dist[v];
            int du = dist[u];
            if(dv > du + weight){
                if (dv != std::numeric_limits<int>::max()) {
                    int tmp = 0;
                    // check where was v before
                    while (!(buckets[tmp].range_a <= dv && buckets[tmp].range_b >= dv))
                        tmp++;
                    // remove from prev bucket
                    for(auto j = buckets[tmp].v_list.begin(); j != buckets[tmp].v_list.end(); j++) {
                        if(*j == v) {
                            buckets[tmp].v_list.erase(j);
                            break;
                        }
                    }
                }

                int b = 0;
                // check where to put v
                while (!(buckets[b].range_a <= du + weight && buckets[b].range_b >= du + weight))
                    b++;
                dist[v] = du + weight;    // new dist
                buckets[b].v_list.push_front(v);    // new bucket
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
            std::vector<int> dist = dijkstra_radix(g, s);
            src.pop_back();
        }
    }
    else if(mode == 2){
        while(!src.empty()){
            int dest = src.back();
            src.pop_back();
            int s = src.back();
            std::vector<int> dist =  dijkstra_radix(g, s);
            src.pop_back();
            std::cout << dist[dest] << std::endl;
        }
    }

    return 0;
}
