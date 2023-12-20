#include <DataParser.h>
#include <algorithm>
#include <iostream>
#include <random>
#include <sim_annealing.h>

int main(int argc, char* argv[])
{
  if(argc < 2) {
    std::cerr << "Please provide file_name as parameter" << std::endl;
    return 1;
  }
  char* file_name = argv[1];
  std::random_device rd;
  std::mt19937 gen(rd());

  // Pre-processing
  auto* dp = new DataParser(file_name);
  Graph* g = dp->parse();
  delete dp;
  if(g == nullptr) {
    return 1;
  }
  g->compute_matrix();
  long double avg_cost = 0.0;
  uint64_t best_cost = UINT64_MAX;
  size_t V = g->no_nodes;
  size_t max_it = 100;
  for(size_t i = 0; i < max_it; i++) {
    auto* tsp = new size_t[V];
    for(size_t j = 0; j < V; j++) {
      tsp[j] = j;
    }
    // Random permutation
    std::shuffle(tsp, tsp + V, gen);
    // Calculate init_cost
    uint64_t total_cost = 0;
    for(size_t j = 1; j < V; j++) {
      total_cost += g->dist_matrix[tsp[j - 1]][tsp[j]];
    }
    total_cost += g->dist_matrix[tsp[0]][tsp[V - 1]];
    // Annealing
    total_cost = sim_annealing(tsp, *g, total_cost);
    avg_cost +=
      static_cast<long double>(total_cost) / static_cast<long double>(max_it);
    if(best_cost > total_cost) {
      best_cost = total_cost;
    }
    std::cout << i << std::endl;
    delete[] tsp;
  }
  std::cout << "Avg. solution cost: " << avg_cost << std::endl;
  std::cout << "The best solution: " << best_cost << std::endl;

  delete g;
  return 0;
}
