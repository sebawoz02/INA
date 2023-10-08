#include <random>

#include "DGraph.h"

DGraph::DGraph(size_t k, size_t i) {
    regenerate:

    random_device rd;
    mt19937 gen(rd());

    const uint64_t no_v = 2 << k; // 2^k * 2
    this->no_verticies = no_v;
    this->adjList = new pair<vector<uint64_t>, size_t>[no_v];
    for(size_t j = 0; j < no_v; j++)
        this->adjList[j].second = 0;

    vector<uint64_t> ed(no_v >> 1);
    for(int j = 0; j < (int)ed.size(); j++)
        *(ed.begin() + j) = j;

    for(uint64_t vertex = no_v >> 1; vertex < no_v; vertex++)
    {
        size_t no_ad = 0;
        vector<uint64_t> copy(ed.size());
        for(uint64_t ii : ed)
            copy.push_back(ii);

        while (no_ad != i)
        {
            if(copy.empty()) {
                delete[] this->adjList;
                goto regenerate;
            }
            uniform_int_distribution<int> dist(0, (int)copy.size() - 1);

            int idx = dist(gen);
            uint64_t v_idx = copy[idx];
            copy.erase(copy.begin() + idx);

            ++this->adjList[v_idx].second;
            if(this->adjList[v_idx].second == i)
                for(auto ii = ed.begin(); ii != ed.end(); ii++)
                    if(*ii == v_idx){
                        ed.erase(ii);
                        break;
                    }
            this->adjList[v_idx].first.push_back(vertex);

            this->adjList[vertex].second++;
            this->adjList[vertex].first.push_back(v_idx);

            no_ad++;
        }
    }
}
