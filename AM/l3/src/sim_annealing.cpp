//
// Created by sebawoz02 on 20.12.23.
//

#include <cmath>
#include <iostream>
#include <random>
#include <sim_annealing.h>

static int32_t calc_invert_cost(const size_t* tsp, size_t i, size_t j,
                                uint64_t** dist_matrix, size_t max_n);
static void invert(size_t* tsp, size_t i, size_t j);

static int32_t calc_invert_cost(const size_t* tsp, const size_t i,
                                const size_t j, uint64_t** dist_matrix,
                                const size_t max_n)
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

static void invert(size_t* tsp, const size_t i, const size_t j)
{
  // Invert vector
  for(size_t it = 0; i + it < j - it; it++) {
    size_t tmp = tsp[i + it];
    tsp[i + it] = tsp[j - it];
    tsp[j - it] = tmp;
  }
}

uint64_t sim_annealing(size_t* tsp, const Graph& g, uint64_t init_cost)
{
  size_t V = g.no_nodes;

  // Parameters
  long double T = init_cost;
  long double delta_t = 0.95f;
  size_t EPOCH_IT = V * 0.3;
  size_t MAX_EPOCHS = 10*V;
  size_t MAX_EPOCHS_WITHOUT_NEW_BEST = MAX_EPOCHS/10;
  // ------

  uint64_t cur_cost = init_cost;
  uint64_t best_cost = init_cost;
  size_t epochs_without_new_best = 0;

  std::random_device rd;
  std::mt19937 gen(rd());
  std::uniform_int_distribution<size_t> dist_i(0, V - 2);
  std::uniform_real_distribution<double> pr_dist(0.0, 1.0);

  for(size_t epoch = 1; epoch <= MAX_EPOCHS; epoch++) {
    bool new_best_this_epoch = false;
    for(size_t it = 0; it < EPOCH_IT; it++) {
      size_t i = dist_i(gen);
      std::uniform_int_distribution<size_t> dist_j(i, V - 1);
      size_t j = dist_j(gen);

      int32_t cost = calc_invert_cost(tsp, i, j, g.dist_matrix, V);

      // Change to neighbour
      if(cost < 0 || std::exp((double(-cost)) / T) > pr_dist(gen)) {
        invert(tsp, i, j);
        cur_cost += cost;
        if(cur_cost < best_cost) {
          best_cost = cur_cost;
          new_best_this_epoch = true;
        }
      }
    }
    // Decrease temperature
    T *= delta_t;
    // End condition
    if(!new_best_this_epoch) {
      if(++epochs_without_new_best >= MAX_EPOCHS_WITHOUT_NEW_BEST)
        break;
    } else {
      epochs_without_new_best = 0;
    }
  }
  return best_cost;
}
