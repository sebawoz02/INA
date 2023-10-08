#include <iostream>
#include <bits/stdc++.h>
#include <list>
#include <vector>
#include <cstring>

using namespace std;


struct graphEdge{
    int startV, endV;
};

class Graph{
private:
    void dfs(int vertex){
        visited[vertex] = true;
        cout<< vertex + 1 << "->";
        list<int> adjList = verticesList.at(vertex);
        list<int>::iterator j;
        for(j = adjList.begin(); j!= adjList.end(); j++){
            if(!visited[*j]) dfs(*j);
        }
    }

    void bfs(int vertex){
        visited[vertex] = true;
        list<int> queue;

        queue.push_back(vertex);
        while(!queue.empty()){
            vertex = queue.front();
            cout << vertex+1 << "->";
            queue.pop_front();
            for(auto adjacency : verticesList.at(vertex)){
                if(!visited[adjacency]){
                    visited[adjacency] = true;
                    queue.push_back(adjacency);
                }
            }
        }
    }

public:
    map<int, list<int>> verticesList;
    bool* visited;
    int numOfVertices;

    Graph(int V, list<graphEdge> edges, bool directed) {
        visited = new bool[V];
        this->numOfVertices = V;
        for (int i = 0; i < V; i++) {
            verticesList[i].push_back(i);
        }
        list<graphEdge>::iterator i;
        for(i = edges.begin(); i!=edges.end();i++){
            graphEdge edge = *i;
            int u = edge.startV;
            int v = edge.endV;
            if(!directed) verticesList[v-1].push_back(u-1);
            verticesList[u-1].push_back(v-1);
        }
    }

    void DFS(int startVertex){
        this->visited = new bool[numOfVertices];
        dfs(startVertex-1);
    }

    void BFS(int startVertex){
        this->visited = new bool[numOfVertices];
        bfs(startVertex-1);
    }

};



int main(int argc, char *argv[]) {
    if(argc<3){
        cout << "Nie podano odpowiedniej liczby argumentow";
        return 1;
    }

    // Graf z Listy0 cwiczenia
    list<graphEdge> edges = {{1, 2}, {1, 3}, {1, 5}, {2, 4}, {2, 5}, {3, 5}, {3, 6}, {4, 5}, {4, 8},
                         {5, 6}, {6, 7}, {6, 9}, {7, 4}, {7, 5}, {7, 8}, {9, 7}, {9, 8}};
    Graph directedGraph(9, edges, true);
    Graph undirectedGraph(9, edges, false);

    // Wlasny przyklad grafu skierowanego
    list<graphEdge> edges2 = {{1, 2}, {1, 6}, {1, 8}, {1, 9}, {2, 3}, {2, 6},
                              {2, 9}, {2, 13}, {3, 4}, {3, 7}, {3, 10}, {4, 1}, {4, 2}, {4, 11},
                              {5, 7}, {5, 9}, {5, 12}, {6, 3}, {6, 4}, {6, 12},
                              {7, 8}, {7, 9}, {7, 10}, {7, 11}, {9, 8}, {9, 13}, {10 , 1}, {10, 3}, {11, 13}, {12, 4}, {12, 7},
                              {13, 5}, {13, 8}};

    Graph myDirecedGraph(13, edges2, true);

    //Wlasny przyklad grafy nieskierowanego

    char *p;

    char dfs[] = "dfs";
    char bfs[] = "bfs";
    int vertex;
    try {
        vertex = (int) strtol(argv[2], &p, 10);
    }
    catch(exception &err){
        cout << "Podano bledny argument";
        return 1;
    }


    if(strcmp(dfs, argv[1]) == 0){
        cout << "DFS grafu skierowanego:"<< endl;
        directedGraph.DFS(vertex);
        cout << endl << "DFS grafu nieskierowanego:" << endl;
        undirectedGraph.DFS(vertex);
    }
    else if(strcmp(bfs, argv[1]) == 0){
        cout << "BFS grafu skierowanego:"<< endl;
        directedGraph.BFS(vertex);
        cout << endl << "BFS grafu nieskierowanego:" << endl;
        undirectedGraph.BFS(vertex);
    }
    else{
        cout << " Bledny argument"<< endl;
        return 1;
    }

}
