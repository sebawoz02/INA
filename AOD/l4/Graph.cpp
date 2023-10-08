#include <cmath>
#include <random>
#include <vector>
#include <queue>

#include "Graph.h"

random_device rd;
mt19937 gen(rd());

// Calculate hamming weight in O(1)
size_t hammingWeight(uint16_t x) {
    x -= (x >> 1) & 0x5555555555555555;
    x = (x & 0x3333333333333333) + ((x >> 2) & 0x3333333333333333);
    x = (x + (x >> 4)) & 0x0f0f0f0f0f0f0f0f;
    return (x * 0x0101010101010101) >> 56;
}

uint16_t randCapacity(size_t upper_bound){
    uniform_int_distribution<int> dist(1, (int)upper_bound);
    return dist(gen);
}


Graph::Graph(size_t k) {
    this->no_verticies = (size_t) pow(2, k);
    this->adjList = new list<Edge*>[this->no_verticies];
    for(size_t vertex = 0; vertex < this->no_verticies; vertex++){
        for (size_t i = 0; i < k; i++) {
            int mask = 1 << i;
            uint16_t modifiedNum = vertex ^ mask;
            if(modifiedNum < vertex)
                continue;

            size_t h_1 = hammingWeight(modifiedNum);
            size_t h_2 = hammingWeight(vertex);

            size_t max_capacity = max(max(max(h_1, h_2), k-h_1), k-h_2);
            if(hammingWeight(modifiedNum) > hammingWeight(vertex)){
                Edge* e = new Edge(modifiedNum, randCapacity((size_t)pow(2, max_capacity)));
                this->adjList[vertex].push_back(e);
            } else{
                Edge* e = new Edge(vertex, randCapacity((size_t)pow(2, max_capacity)));
                this->adjList[modifiedNum].push_back(e);
            }
        }
    }
}

bool Graph::bfs(uint16_t s, uint16_t t, vector<uint16_t>& parent) const {
    vector<bool> visited(this->no_verticies, false);
    queue<uint16_t> q;
    q.push(s);
    visited[s] = true;
    parent[s] = -1;

    while (!q.empty()) {
        uint16_t u = q.front();
        q.pop();

        for (Edge* e : adjList[u]) {
            uint16_t v = e->dest;
            if (!visited[v] && e->capacity > e->flow) {
                q.push(v);
                visited[v] = true;
                parent[v] = u;
            }
        }
    }

    // Return true if there is a path from source to sink
    return visited[t];
}

Edge::Edge(uint16_t dest, uint16_t capacity){
    this->dest = dest;
    this->capacity = capacity;
    this->flow = 0;
}
