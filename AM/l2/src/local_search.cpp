#include <local_search.h>

static int32_t invert(const size_t* tsp, size_t i, size_t j,
                      uint64_t** dist_matrix, size_t max_n);

static int32_t invert(const size_t* tsp, const size_t i, const size_t j,
                      uint64_t** dist_matrix, const size_t max_n)
{
  uint64_t plus = 0;
  uint64_t minus = 0;
  if(i != 0)
  {
    size_t tmp = tsp[i - 1];
    minus += dist_matrix[tmp][tsp[i]];
    plus += dist_matrix[tmp][tsp[j]];
  }
  else if(j >= max_n - 2)
  {
    return 0;
  }
  else
  {
    size_t tmp = tsp[max_n - 1];
    plus += dist_matrix[tsp[j]][tmp];
    minus += dist_matrix[tsp[i]][tmp];
  }
  if(j != max_n - 1)
  {
    size_t tmp = tsp[j + 1];
    minus += dist_matrix[tmp][tsp[j]];
    plus += dist_matrix[tmp][tsp[i]];
  }
  else if(i == 1)
  {
      return 0;
  }
  else
  {
    size_t tmp = tsp[0];
    plus += dist_matrix[tsp[i]][tmp];
    minus += dist_matrix[tsp[j]][tmp];
  }
  return static_cast<int32_t>(plus) - static_cast<int32_t>(minus);
}

std::pair<uint64_t, size_t> local_search(size_t* tsp, const Graph& g,
                                         uint64_t init_cost)
{
  size_t n = g.no_nodes;
  uint64_t cost = init_cost;
  size_t steps = 0;

  while(true) {
    uint64_t candidate = UINT64_MAX;
    size_t best_i = 0;
    size_t best_j = 0;
    for(size_t i = 0; i < n - 1; i++) {
      for(size_t j = i + 1; j < n; j++) {
        int32_t tmp = invert(tsp, i, j, g.dist_matrix, n);
        uint64_t tmp2 = cost + tmp;
        if(tmp2 < candidate) {
          best_i = i;
          best_j = j;
          candidate = tmp2;
        }
      }
    }
    if(candidate < cost) {
      cost = candidate;
      // Invert vector
      for(size_t it = 0; best_i + it < best_j - it; it++) {
        size_t tmp = tsp[best_i + it];
        tsp[best_i + it] = tsp[best_j - it];
        tsp[best_j - it] = tmp;
      }
      steps++;
    } else {
      break;
    }
  }
  return std::make_pair(cost, steps);
}
