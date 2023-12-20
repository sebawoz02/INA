#include <string>
#include <tabu_search.h>
#include <unordered_set>
#include <iostream>

static int32_t invert(const size_t* tsp, size_t i, size_t j,
                      uint64_t** dist_matrix, size_t max_n);

static std::string array_to_str(const size_t* tsp, size_t len);

static std::string array_to_str(const size_t* tsp, const size_t len)
{
  std::string str;
  for(size_t i = 0; i < len; i++) {
    str += std::to_string(tsp[i]) + " ";
  }
  return str;
}

static int32_t invert(const size_t* tsp, const size_t i, const size_t j,
                      uint64_t** dist_matrix, const size_t max_n)
{
  uint64_t plus = 0;
  uint64_t minus = 0;
  if(i != 0) {
    size_t tmp = tsp[i - 1];
    minus += dist_matrix[tmp][tsp[i]];
    plus += dist_matrix[tmp][tsp[j]];
  } else if(j >= max_n - 2) {
    return 0;
  } else {
    size_t tmp = tsp[max_n - 1];
    plus += dist_matrix[tsp[j]][tmp];
    minus += dist_matrix[tsp[i]][tmp];
  }
  if(j != max_n - 1) {
    size_t tmp = tsp[j + 1];
    minus += dist_matrix[tmp][tsp[j]];
    plus += dist_matrix[tmp][tsp[i]];
  } else if(i == 1) {
    return 0;
  } else {
    size_t tmp = tsp[0];
    plus += dist_matrix[tsp[i]][tmp];
    minus += dist_matrix[tsp[j]][tmp];
  }
  return static_cast<int32_t>(plus) - static_cast<int32_t>(minus);
}

std::pair<uint64_t, size_t> tabu_search(size_t* tsp, const Graph& g,
                                        uint64_t init_cost)
{
  size_t V = g.no_nodes;

  // Parameters
  size_t TABU_CAPACITY = V/5;
  size_t MAX_IT_WITHOUT_NEW_BEST = TABU_CAPACITY/7;
  // ------

  uint64_t cost = init_cost;
  size_t steps = 0;
  std::unordered_set<std::string> tabu;
  size_t best_solution[V];

  size_t it_without_new_best = 0;
  size_t tabu_len = 0;

  while(true) {
    uint64_t candidate = init_cost;
    size_t best_i = 0;
    size_t best_j = 0;
    for(size_t i = 0; i < V - 1; i++) {
      for(size_t j = i + 1; j < V; j++) {
        int32_t tmp = invert(tsp, i, j, g.dist_matrix, V);
        uint64_t neigh_cost = cost + tmp;
        if(neigh_cost < candidate) {
          // Check if it is on tabu list
          // Make a copy
          size_t tsp_copy[V];
          for(size_t it = 0; it < V; it++) {
            tsp_copy[it] = tsp[it];
          }
          // Invert from i to j
          for(size_t it = 0; i + it < j - it; it++) {
            size_t tmp3 = tsp_copy[i + it];
            tsp_copy[i + it] = tsp_copy[j - it];
            tsp_copy[j - it] = tmp3;
          }
          // Make it str and check in tabu list
          std::string str_tsp_copy = array_to_str(tsp_copy, V);
          if(tabu.find(str_tsp_copy) != tabu.end())
            continue;
          best_i = i;
          best_j = j;
          candidate = neigh_cost;
        }
      }
    }
    // Invert vector
    for(size_t it = 0; best_i + it < best_j - it; it++) {
      size_t tmp = tsp[best_i + it];
      tsp[best_i + it] = tsp[best_j - it];
      tsp[best_j - it] = tmp;
    }
    tabu.insert(array_to_str(tsp, V));
    tabu_len++;

    if(candidate < cost) {
      it_without_new_best = 0;
      cost = candidate;
      for(size_t i = 0; i < V; i++)
        best_solution[i] = tsp[i];
    } else {
      it_without_new_best++;
      if(it_without_new_best == MAX_IT_WITHOUT_NEW_BEST) {
          std::cout << "MAX iterations" << std::endl;
          break;
      }
    }
    steps++;
    if(tabu_len == TABU_CAPACITY) // Tabu list is full
    {
        std::cout << "MAX tabu capacity" << std::endl;
        break;
    }
  }

  // Copy best solution
  for(size_t i = 0; i < V; i++)
    tsp[i] = best_solution[i];
  return std::make_pair(cost, steps);
}
