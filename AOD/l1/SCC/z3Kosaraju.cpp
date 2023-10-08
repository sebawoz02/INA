#include <iostream>
#include <bits/stdc++.h>
#include <list>
#include <vector>
#include <string>
#include <cstring>

using namespace std;


class Graph{
private:
    // step 1 dfs
    void fillOrder(bool* visited, int startVertex, list<int> *stk) const{
        stack<int> dfsStack;
        list<int> order;
        dfsStack.push(startVertex);
        order.push_front(startVertex);
        while(!dfsStack.empty()){
            int s = dfsStack.top();
            dfsStack.pop();
            order.push_front(s);
            if(!visited[s]) visited[s] = true;
            int found = 0;
            for(int & i : verticesList[s]){
                if(!visited[i]){
                    found++;
                    dfsStack.push(i);
                }
            }
            if(found == 0){
                stk->push_front(s);
                order.pop_front();
                while(!order.empty()){
                    int prev = order.front();
                    found = 0;
                    for(int & i : verticesList[prev]) {
                        if(!visited[i]) {
                            found++;
                            break;
                        }
                    }
                    if(found == 0){
                        stk->push_front(prev);
                        order.pop_front();
                    }
                    else break;
                }
            }
        }
    }

    // step 2
    Graph* getTranspose(){
        auto* transpose = new Graph(this->numOfVertices);
        for(int v = 0; v<numOfVertices; v++){
            for(auto i = verticesList[v].begin(); i!=verticesList[v].end(); i++){
                transpose->addEgde(*i+1, v+1);
            }
        }
        return transpose;
    }

    // step 3 dfs
    void dfsT(int vertex, bool* visited) const{
        int numOfComponents = 0;
        stack<int> dfsStack;
        dfsStack.push(vertex);
        while(!dfsStack.empty()){
            int s = dfsStack.top();
            dfsStack.pop();
            if(!visited[s]){
                if(numOfVertices<=200) cout << s+1 << " ";
                visited[s] = true;
                numOfComponents++;
            }
            for(int & i : verticesList[s]){
                if(!visited[i]){
                    dfsStack.push(i);
                }
            }
        }
        cout << endl << "# of components: "<< numOfComponents << endl;
    }



public:
    list<int>* verticesList;
    int numOfVertices;


    explicit Graph(int V){
        this->numOfVertices = V;
        verticesList = new list<int>[V];
    }

    void addEgde(int v, int u) const{
        verticesList[v-1].push_back(u-1);
    }

    void findSCC(){
        cout << "SCC:" <<endl;
        list<int> stack;
        int sccFound = 0;
        bool* visited = new bool[numOfVertices];
        for(int i = 0; i< numOfVertices; i++) visited[i] = false;

        // step 1: fill order
        for(int i = 0; i< numOfVertices; i++) if(!visited[i]) fillOrder(visited, i, &stack);

        // step 2: get transpose graph
        Graph* tr = getTranspose();
        for(int i = 0; i< numOfVertices; i++) visited[i] = false;

        // step 3: process vertices on stack
        while(!stack.empty()){
            int v = stack.front();
            stack.pop_front();
            // print ssc
            if(!visited[v]){
                tr->dfsT(v, visited);
                sccFound++;
            }
        }
        cout << "SSC found: "<< sccFound;

    }

};


int main(){
    // Wczytywanie danych z pliku
    clock_t start = clock();
    cout << "Wczytywanie danych...";
    string line;
    string space_delimiter = " ";

    ifstream file("E:/STUDIA/AOD/list1/project1/aod_testy1/3/g3-6.txt");

    int i = 1;
    int V;
    Graph *graph;

    while(getline(file, line)){
        vector<string> values{};

        if(i==2){
            V = stoi(line);
            graph = new Graph(V);
        }
        else if(i!=1 && i!=3){
            size_t pos ;
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

    start = clock();
    graph->findSCC();
    cout <<endl << "Czas szukania SCC:"<< (clock()- start)/(double)CLOCKS_PER_SEC << "s"<< endl;
}
