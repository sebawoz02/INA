#include <fstream>
#include <iostream>
#include <string.h>

#include "data_loader.h"


Graph* create_graph_from_path(char* path){
    std::fstream file(path);
    Graph* g = new Graph(1);
    std::string text;
    while (getline(file, text))
    {
        char* p;
        char* line = new char [text.length() + 1];
        strcpy(line, text.c_str());
        p = strtok(line, " ");
        if(strcmp(p, "a") == 0){
            p = strtok(NULL, " ");
            int src = atoi(p);
            p = strtok(NULL, " ");
            int dest = atoi(p);
            p = strtok(NULL, " ");
            int weight = atoi(p);
            if(weight > g->max_weight){
                g->max_weight = weight;
            }
            g->addEdge(src-1, dest-1, weight);
        }
        else if(strcmp(p, "p") == 0){
            p = strtok(NULL, " ");
            p = strtok(NULL, " ");
            g = new Graph(atoi(p));
        }
        delete[] line;
    }
    file.close();
    return g;
}


std::list<int> get_sources(char* path, int mode){
    std::fstream file(path);
    std::list<int> src;
    std::string text;
    while (getline(file, text)){
        char* p;
        char* line = new char [text.length() + 1];
        strcpy(line, text.c_str());
        p = strtok(line, " ");

        if(mode == 1){
            if(strcmp(p, "s") == 0){
                p = strtok(NULL, " ");
                src.push_back(atoi(p) - 1);
            }
        }
        else if(mode == 2){
            if(strcmp(p, "q") == 0){
                p = strtok(NULL, " ");
                src.push_back(atoi(p) - 1);
                p = strtok(NULL, " ");
                src.push_back(atoi(p) - 1);
            }
        }
        delete[] line;
    }
    file.close();
    return src;
}
