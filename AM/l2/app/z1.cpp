#include <algorithm>
#include <iostream>
#include <random>
#include <stack>
#include <vector>

#include <local_search.h>
#include <DataParser.h>
#include <primAlgorithm.h>

static size_t* solve_tsp(std::vector<TreeNode*>& nodes, const Graph& g,
                         uint64_t* cost, size_t node_id);

static size_t* solve_tsp(std::vector<TreeNode*>& nodes, const Graph& g,
                         uint64_t* cost, const size_t node_id)
{
  uint64_t total_cost = 0;
  std::stack<TreeNode*> stk;
  auto* tsp = new size_t[g.no_nodes];

  // Start from random node
  stk.push(nodes[node_id]);
  TreeNode* prev = nullptr;
  size_t it = 0;
  bool visited[g.no_nodes];
  for(size_t i = 0; i < g.no_nodes; i++) {
    visited[i] = false;
  }
  // Pre-order walk to solve TSP
  while(!stk.empty()) {
    auto* node = stk.top();
    stk.pop();
    tsp[it] = node->id;
    visited[node->id] = true;
    if(prev != nullptr) {
      total_cost += g.dist_matrix[prev->id][node->id];
    }
    prev = node;
    std::sort(node->children.begin(), node->children.end(),
              [](TreeNode* a, TreeNode* b) { return a->id < b->id; });
    for(auto i: node->children) {
      if(!visited[i->id])
        stk.push(i);
    }
    it++;
  }
  total_cost += g.dist_matrix[nodes[node_id]->id][prev->id];
  *cost = total_cost;
  return tsp;
}


int main(int argc, char* argv[])
{
  if(argc < 2) {
    std::cerr << "Please provide file_name as parameter" << std::endl;
    return 1;
  }
  char* file_name = argv[1];

  // Pre-processing
  auto* dp = new DataParser(file_name);
  Graph* g = dp->parse();
  delete dp;
  if(g == nullptr) {
    return 1;
  }
  g->compute_matrix();

  long double avg_cost = 0.0;
  double avg_steps = 0.0;
  uint64_t best_cost = UINT64_MAX;
  std::random_device rd;
  std::mt19937 gen(rd());
  std::uniform_int_distribution<> distribution(0, g->no_nodes - 1);
  // Find MST
  std::vector<TreeNode*> mst = prim_find_MST(g);        // MST
  auto iterations = (size_t)ceil(sqrt(g->no_nodes));    // Num of iterations
  // size_t* best_tsp = nullptr;   // To visualize it later
  for(size_t i = 0; i < iterations; i++) {
    // Pre-order walk starting from random node
    uint64_t cost = 0;
    size_t* tsp = solve_tsp(mst, *g, &cost, distribution(gen));
    // Local search
    auto data = local_search(tsp, *g, cost);
    cost = data.first;
    avg_cost +=
      static_cast<long double>(cost) / static_cast<long double>(iterations);
    avg_steps +=
      static_cast<double>(data.second) / static_cast<double>(iterations);
    if(best_cost > cost) {
     best_cost = cost;
      // delete[] best_tsp;
      // best_tsp = tsp;
    }//else {
        delete[] tsp;
    //}
  }
  std::cout << "Avg. solution cost: " << avg_cost << std::endl;
  std::cout << "Avg. steps: " << avg_steps << std::endl;
  std::cout << "The best solution: " << best_cost << std::endl;
  // Cleanup
  for(auto& i: mst) {
    delete i;
  }
  //delete[] best_tsp;
  delete g;
  return 0;
}
