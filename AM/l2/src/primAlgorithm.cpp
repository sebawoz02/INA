#include <bits/stdc++.h>
#include <primAlgorithm.h>

static size_t minKeyId(const uint32_t minKey[], const bool visited[],
                       size_t no_nodes);

// Returns the id of the Node to which the shortest path leads
static size_t minKeyId(const uint32_t minKeys[], const bool visited[],
                       size_t no_nodes)
{
  // Initialize min value
  uint32_t min = UINT32_MAX;
  size_t min_index;

  for(size_t v = 0; v < no_nodes; v++)
    if(!visited[v] && minKeys[v] < min)
      min = minKeys[v], min_index = v;

  return min_index;
}

std::vector<TreeNode*> prim_find_MST(const Graph* const graph)
{
  // V
  const size_t no_nodes = graph->no_nodes;
  // Minimum spanning tree
  std::vector<TreeNode*> mst;
  // List of parents
  int64_t parents[no_nodes];
  // Nodes already in MST
  bool visited[no_nodes];
  // Used to pick min value for all Nodes in MST
  uint32_t min_keys[no_nodes];

  mst.reserve(no_nodes);
  for(size_t i = 0; i < no_nodes; i++) {
      min_keys[i] = UINT32_MAX;
    visited[i] = false;
    mst.push_back(new TreeNode(i, graph->nodes_list[i]->x,
                               graph->nodes_list[i]->y, 0, nullptr));
  }
    min_keys[0] = 0;
  parents[0] = -1;

  for(size_t i = 0; i < no_nodes - 1; i++) {
    const size_t id = minKeyId(min_keys, visited, no_nodes);

    visited[id] = true;
    for(size_t v = 0; v < no_nodes; v++) {
      if(graph->dist_matrix[id][v] != 0 && !visited[v] &&
         graph->dist_matrix[id][v] < min_keys[v]) {
          min_keys[v] = graph->dist_matrix[id][v];
        parents[v] = (int64_t)id;
      }
    }
  }
  uint64_t tree_cost = 0;
  // Set parents of TreeNodes in vector
  for(size_t i = 1; i < no_nodes; i++) {
    mst[i]->parent = mst[parents[i]];
    mst[i]->cost = graph->dist_matrix[parents[i]][i];
    tree_cost += mst[i]->cost;
    mst[i]->parent->addEdge(mst[i]);
    // Add parents to children for task purpose
    mst[i]->addEdge(mst[i]->parent);
  }
  // std::cout << "MST: " << tree_cost << std::endl;
  return mst;
}
