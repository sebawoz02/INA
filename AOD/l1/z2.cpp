
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

class Graph{

public:
    map<int, list<int>> verticesList;
    int numOfVertices;

    Graph(int V, list<graphEdge> edges, bool directed) {
        this->numOfVertices = V;
        list<graphEdge>::iterator i;
        for(i = edges.begin(); i!=edges.end();i++){
            graphEdge edge = *i;
            int u = edge.startV;
            int v = edge.endV;
            if(!directed) verticesList[v-1].push_back(u-1);
            verticesList[u-1].push_back(v-1);
        }
    }
    // funkcja rekurencyjna szukajaca cyklu sposobem DFS rozpoczynajac z podanego wierzcholka + liczaca stopnie wychodzace wierzcholkow
    bool recursiveHelper(int vertex, bool visited[], bool *stack, int vINd[]){
        if(!visited[vertex]){
            visited[vertex] = true;
            stack[vertex] = true;

            list<int>::iterator i;
            for(i = verticesList[vertex].begin(); i != verticesList[vertex].end(); i++){
                vINd[*i]++;
                if(!visited[*i] && recursiveHelper(*i, visited, stack, vINd)) return true;
                else if(stack[*i]) return true;
            }
        }
        stack[vertex] = false;
        return false;
    }

    // funkcja szukajaca cyklu sposobem DFS w roznych drzewach az wszystkie wiercholki zostana odwiedzone
    bool isCyclic(int vINd[]){
        bool *visited = new bool[numOfVertices]; // odwiedzone wiercholki
        bool *stack = new bool[numOfVertices];
        for(int i = 0; i < numOfVertices; i++){
            visited[i] = false;
            stack[i] = false;
        }
        for(int i = 0; i < numOfVertices; i++){
            if(!visited[i] && recursiveHelper(i, visited, stack, vINd)) return true;
        }
        return false;
    }

    list<int> topologicalSorting(){

        int* vINd = new int[numOfVertices]; // tablica stopni wchodzących
        list<int>::iterator j;
        list<int> noIncomingEdges;  // lista zawiera wierzcholki o stopniu 0
        list<int> ordering; //lista z posortowanymi elementami

        for(int i = 0; i<numOfVertices;i++){
            vINd[i] = 0;
        }
        if(isCyclic(vINd)){
            cout << "Graf zawiera skierowany cykl"<<endl;
            return ordering;
        } else{
            cout << "Graf nie zawiera skierowanego cyklu" << endl;
            for(int i = 0; i < numOfVertices;i++){
                if(vINd[i] == 0) noIncomingEdges.push_back(i);
            }

            while(!noIncomingEdges.empty()){
                int vertex = noIncomingEdges.front();
                ordering.push_back(vertex + 1);
                noIncomingEdges.pop_front();
                for(j = verticesList[vertex].begin(); j!= verticesList[vertex].end(); j++){
                    vINd[*j]--;
                    if (vINd[*j] == 0) noIncomingEdges.push_back(*j);
                }
            }
            return ordering;
        }
    }

};

int main(){
    // Wczytywanie danych z pliku
    clock_t start = clock();
    cout << "Wczytywanie danych...";
    string line;
    string space_delimiter = " ";

    ifstream file("------");

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

    start = clock();
    list<int> ordering = graph.topologicalSorting();
    end = clock();
    cout<< endl << "Czas:" << (end - start)/(double)CLOCKS_PER_SEC << "s"<< endl;

    if(V <= 200){
        for(list<int>::iterator i = ordering.begin(); i!=ordering.end();i++){
            cout << *i << ", ";
        }
    }

    return 0;

}
