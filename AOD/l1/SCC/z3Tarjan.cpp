#include <iostream>
#include <bits/stdc++.h>
#include <list>
#include <vector>
#include <string>
#include <cstring>

using namespace std;


class Graph{
private:
    void SSCUt(int u, int* disc, int* low, list<int>* stack, bool* isOnStack){
        static int time = 0;
        disc[u] = low[u] = time++;
        stack->push_front(u);
        isOnStack[u] = true;
        for(auto i = verticesList[u].begin(); i != verticesList[u].end(); i++){
            int v = *i;
            if(disc[v] == -1){
                SSCUt(v, disc, low, stack, isOnStack);
                low[u] = min(low[u], low[v]);
            }
            else if(isOnStack[v]){
                low[u] = min(low[u], disc[v]);
            }
        }

        int popped;
        int numOfComponents = 0;
        if(low[u] == disc[u]){
            while(*(stack->begin()) != u){
                numOfComponents++;
                popped = *(stack->begin());
                if(numOfVertices<=200) cout << popped + 1 << " ";
                isOnStack[popped] = false;
                stack->pop_front();
            }
            popped = *(stack->begin());
            numOfComponents++;
            if(numOfVertices<=200) cout << popped + 1 << " ";
            isOnStack[popped] = false;
            stack->pop_front();
            cout << "#" << numOfComponents << endl;
        }

    }

public:
    vector<int> *verticesList;
    int numOfVertices;

    explicit Graph(int V) {
        this->numOfVertices = V;
        verticesList = new vector<int>[V];
    }

    void addEgde(int v, int u){
        verticesList[v-1].push_back(u-1);
    }

    void findSCC(){
        int* disc = new int[numOfVertices];
        int* low = new int[numOfVertices];
        bool* isOnStack = new bool[numOfVertices];
        list<int> stack;

        for(int i = 0; i < numOfVertices; i++){
            disc[i] = -1;
            low[i] = -1;
            isOnStack[i] = false;
        }
        for(int i = 0; i< numOfVertices; i++){
            if(disc[i] == -1){
                SSCUt(i, disc, low, &stack, isOnStack);
            }
        }

    }

};

int main(){
    // Wczytywanie danych z pliku
    clock_t start = clock();
    cout << "Wczytywanie danych...";
    string line;
    string space_delimiter = " ";

    ifstream file("E:/STUDIA/AOD/list1/project1/aod_testy1/3/g3-4.txt");

    int i = 1;
    int V;

    Graph* graph;
    while(getline(file, line)){
        vector<string> values{};

        if(i==2){
            V = stoi(line);
            graph = new Graph(V);
        }
        else if(i!=1 && i!=3){
            size_t pos;
            while ((pos = line.find(space_delimiter)) != string::npos){
                values.push_back(line.substr(0, pos));
                line.erase(0, pos + space_delimiter.length());
            }
            values.push_back(line);
            graph->addEgde(stoi(values[0]), stoi(values[1]));
        }
        i++;

    }
    file.close();
    clock_t end = clock();
    cout << (end - start)/(double)CLOCKS_PER_SEC << "s"<< endl;
    // Wczytano dane

    graph->findSCC();

    return 0;
}
