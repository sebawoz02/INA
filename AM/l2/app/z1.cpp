#include <algorithm>
#include <iostream>
#include <random>
#include <stack>
#include <vector>

#include <DataParser.h>
#include <primAlgorithm.h>

static size_t* solve_tsp(std::vector<TreeNode*>& nodes, const Graph& g,
                         uint64_t* cost, size_t node_id);
static std::pair<uint64_t, size_t> local_search(size_t* tsp, const Graph& g,
                                                uint64_t init_cost);
static uint64_t invert(const size_t* tsp, size_t i, size_t j,
                       uint64_t** dist_matrix, uint64_t cost, size_t max_n);

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
  *cost = total_cost;
  return tsp;
}

static std::pair<uint64_t, size_t> local_search(size_t* tsp, const Graph& g,
                                                uint64_t init_cost)
{
  size_t n = g.no_nodes;
  uint64_t cost = init_cost;
  size_t steps = 0;
  while(true) {
    uint64_t candidate = UINT64_MAX;
    size_t best_i, best_j;
    for(size_t i = 0; i < n - 1; i++) {
      for(size_t j = i + 1; j < n; j++) {
        uint64_t tmp = invert(tsp, i, j, g.dist_matrix, cost, n);
        if(tmp < candidate) {
          best_i = i;
          best_j = j;
          candidate = tmp;
        }
      }
    }
    if(candidate < cost) {
      cost = candidate;
      size_t tmp = tsp[best_i];
      tsp[best_i] = tsp[best_j];
      tsp[best_j] = tmp;
      steps++;
    } else {
      break;
    }
  }
  return std::make_pair(cost, steps);
}

static uint64_t invert(const size_t* tsp, const size_t i, const size_t j,
                       uint64_t** dist_matrix, uint64_t cost,
                       const size_t max_n)
{
  size_t i0 = tsp[i];
  size_t j0 = tsp[j];
  if(i != 0) {
    size_t tmp = tsp[i - 1];
    cost -= dist_matrix[tmp][i0];
    cost += dist_matrix[tmp][j0];
  }
  if(i + 1 != j) {
    size_t tmp1 = tsp[i + 1];
    size_t tmp2 = tsp[j - 1];
    cost -= dist_matrix[i0][tmp1];
    cost += dist_matrix[j0][tmp1];

    cost -= dist_matrix[tmp2][j0];
    cost += dist_matrix[tmp2][i0];
  }
  if(j != max_n - 1) {
    size_t tmp = tsp[j + 1];
    cost -= dist_matrix[j0][tmp];
    cost += dist_matrix[i0][tmp];
  }
  return cost;
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
  std::vector<TreeNode*> mst = primFindMST(g);
  auto iterations = (size_t)ceil(sqrt(g->no_nodes));
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
    }
    delete[] tsp;
  }
  std::cout << "Avg. solution cost: " << avg_cost << std::endl;
  std::cout << "Avg. steps: " << avg_steps << std::endl;
  std::cout << "The best solution: " << best_cost << std::endl;

  // Cleanup
  for(auto& i: mst) {
    delete i;
  }
  delete g;
  return 0;
}
