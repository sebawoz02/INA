#ifndef L1_GRAPH_H
#define L1_GRAPH_H

#include "Node.h"
#include <cstdlib>
#include <vector>

class Graph {
public:
  size_t no_nodes;
  std::vector<Node*> nodes_list;
  uint64_t** dist_matrix;

  explicit Graph(size_t no_nodes);
  void addNode(uint16_t x, uint16_t y);
  void compute_matrix();
  ~Graph();
};

#endif //L1_GRAPH_H
