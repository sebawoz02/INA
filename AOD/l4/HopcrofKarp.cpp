
#include "HopcrofKarp.h"

HopcroftKarp::HopcroftKarp(DGraph* g, size_t n){
    this->n = n >> 1;
    this->m = n >> 1;
    this->graph = g;
}

bool HopcroftKarp::bfs(){
    queue<::uint64_t> q;
    dist.assign(n + 1, INF);

    for (size_t u = 1; u <= n; ++u) {
        if (matching[u] == NIL) {
            dist[u] = 0;
            q.push(u);
        }
    }

    while (!q.empty()) {
        ::uint64_t u = q.front();
        q.pop();

        if (dist[u] < dist[NIL]) {
            for (::uint64_t v : graph->adjList[u].first) {
                if (dist[matching[v]] == INF) {
                    dist[matching[v]] = dist[u] + 1;
                    q.push(matching[v]);
                }
            }
        }
    }

    return (dist[NIL] != INF);
}

bool HopcroftKarp::dfs(uint64_t u){
    if (u == NIL) {
        return true;
    }

    for (uint64_t v : graph->adjList[u].first) {
        if (dist[matching[v]] == dist[u] + 1 && dfs(matching[v])) {
            matching[u] = v;
            matching[v] = u;
            return true;
        }
    }

    dist[u] = INF;
    return false;
}

int HopcroftKarp::findMaxMatching() {
    matching.assign(n + m + 1, NIL);
    int maxMatching = 0;

    while (bfs()) {
        for (size_t u = 1; u <= n; ++u) {
            if (matching[u] == NIL && dfs(u)) {
                ++maxMatching;
            }
        }
    }

    return maxMatching;
}
