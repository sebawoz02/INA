#ifndef AOD_DATA_LOADER_H
#define AOD_DATA_LOADER_H
#include <list>

#include "Graph_Representation.h"

Graph* create_graph_from_path(char* path);

std::list<int> get_sources(char* path, int mode);

#endif //AOD_DATA_LOADER_H
