#include <local_search.h>

static uint64_t invert(const size_t* tsp, size_t i, size_t j,
                       uint64_t** dist_matrix, uint64_t cost, size_t max_n);

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

std::pair<uint64_t, size_t> local_search(size_t* tsp, const Graph& g,
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

