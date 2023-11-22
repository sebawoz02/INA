#include <local_search.h>

static int32_t invert(const size_t* tsp, size_t i, size_t j,
                       uint64_t** dist_matrix, size_t max_n);
static void vector_inv(size_t* tsp, size_t i, size_t j);

static int32_t invert(const size_t* tsp, const size_t i, const size_t j,
                       uint64_t** dist_matrix,
                       const size_t max_n)
{
    uint64_t plus = 0;
    uint64_t minus = 0;
    if(i != 0)
    {
        size_t tmp = tsp[i-1];
        minus += dist_matrix[tmp][tsp[i]];
        plus += dist_matrix[tmp][tsp[j]];
    }
    if(j != max_n - 1)
    {
        size_t tmp = tsp[j+1];
        minus += dist_matrix[tmp][tsp[j]];
        plus += dist_matrix[tmp][tsp[i]];
    }
    return static_cast<int32_t>(plus) - static_cast<int32_t>(minus);
}

static void vector_inv(size_t* tsp, const size_t i, const size_t j)
{
    size_t dist  = ((i+j)/2) - i + 1;
    for(size_t it = 0; it < dist; it++)
    {
        size_t tmp = tsp[i+it];
        tsp[i+it] = tsp[j-it];
        tsp[j-it] = tmp;
    }
}


std::pair<uint64_t, size_t> local_search(size_t* tsp, const Graph& g,
                                                uint64_t init_cost)
{
    size_t n = g.no_nodes;
    uint64_t cost = init_cost;
    size_t steps = 0;
    auto** invert_function = new int32_t*[n-1];
    for(size_t i = 0; i < n - 1; i++)
    {
        invert_function[i] = new int32_t[n];
        for(size_t j = i; j < n; j++)
        {
            invert_function[i][j] = invert(tsp, i, j, g.dist_matrix, n);
        }
    }

    size_t prev_i, prev_j;
    while(true) {
        uint64_t candidate = UINT64_MAX;
        size_t best_i, best_j;
        for(size_t i = 0; i < n - 1; i++) {
            for(size_t j = i + 1; j < n; j++) {

                if(steps && j >= prev_i && i <= prev_j)
                {
                    invert_function[i][j] = invert(tsp, i, j, g.dist_matrix, n);
                }
                int32_t tmp = invert_function[i][j];
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
            vector_inv(tsp, best_i, best_j);
            steps++;
            prev_i = best_i;
            prev_j = best_j;
        } else {
            break;
        }
    }
    for(size_t i = 0; i < n - 1; i++)
    {
        delete[] invert_function[i];
    }
    delete[] invert_function;
    return std::make_pair(cost, steps);
}

