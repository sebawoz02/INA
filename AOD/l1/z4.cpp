
#include <iostream>
#include <bits/stdc++.h>
#include <list>
#include <vector>
#include <string>
#include <cstring>

using namespace std;

struct graphEdge{
    int startV, endV;
};


class Graph {

public:
    map<int, list<int>> verticesList;
    int numOfVertices;

    Graph(int V, list<graphEdge> edges, bool directed) {
        this->numOfVertices = V;
        list<graphEdge>::iterator i;
        for (i = edges.begin(); i != edges.end(); i++) {
            graphEdge edge = *i;
            int u = edge.startV;
            int v = edge.endV;
            if (!directed) verticesList[v - 1].push_back(u - 1);
            verticesList[u - 1].push_back(v - 1);
        }
    }

    // time complexity: O(|V|+|E|)
    bool isBipartite(){
        int colors[numOfVertices];
        for(int i = 0; i<numOfVertices; i++) colors[i] = -1;     // -1 grey color, 0 blue, 1 red
        queue<int> bfsQueue;

        // loop to visit all the verticies
        for(int i = 0; i < numOfVertices; i++){
            if(colors[i]!=-1) continue; // already visited
            colors[i] = 1;
            bfsQueue.push(i);
            while(!bfsQueue.empty()){
                int v = bfsQueue.front();
                bfsQueue.pop();
                list<int>::iterator u;
                for(u = verticesList[v].begin(); u != verticesList[v].end(); u++){
                    if(colors[v] == colors[*u]){    // neighbours in the same color
                        cout << "Graf nie jest dwudzielny." <<endl;
                        return false;
                    }
                    if(colors[*u]==-1){
                        colors[*u] = 1 - colors[v];
                        bfsQueue.push(*u);
                    }
                }
            }

        }
        cout << "Graft jest dwudzielny" << endl;
        if(numOfVertices<=200){
            cout << "V_0: ";
            for(int i = 0; i < numOfVertices; i++){
                if(colors[i] == 0) cout << i+1 << ", ";
            }
            cout << endl << "V_1: ";
            for(int i = 0; i < numOfVertices; i++){
                if(colors[i] == 1) cout << i+1 << ", ";
            }
        }

        return true;
    }
};


int main(){
    // Wczytywanie danych z pliku
    clock_t start = clock();
    cout << "Wczytywanie danych...";
    string line;
    string space_delimiter = " ";

    ifstream file("E:/STUDIA/AOD/list1/project1/aod_testy1/4/u4b-1.txt");

    int i = 1;
    bool directed;
    int V, E;

    list<graphEdge> edges;

    while(getline(file, line)){
        vector<string> values{};

        if(i==1){
            char* dir = new char[1];
            strcpy(dir, line.c_str());
            if(dir[0] == 'D') directed = true;
            else directed = false;
        } else if(i==2){
            V = stoi(line);
        }
        else if(i==3){
            E = stoi(line);
        }
        else{
            size_t pos = 0;
            while ((pos = line.find(space_delimiter)) != string::npos){
                values.push_back(line.substr(0, pos));
                line.erase(0, pos + space_delimiter.length());
            }
            values.push_back(line);
            graphEdge edge = {stoi(values[0]), stoi(values[1])};
            edges.push_back(edge);
        }
        i++;

    }
    file.close();
    clock_t end = clock();
    cout << (end - start)/(double)CLOCKS_PER_SEC << "s"<< endl;
    // Wczytano dane
    Graph graph(V, edges, directed);

    graph.isBipartite();
}
