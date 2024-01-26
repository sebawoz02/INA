#include <DataParser.h>
#include <iostream>
#include <random>
#include <algorithm>
#include <local_search.h>

int main(int argc, char* argv[]) {
    if (argc < 2) {
        std::cerr << "Please provide file_name as parameter" << std::endl;
        return 1;
    }
    char *file_name = argv[1];
    std::random_device rd;
    std::mt19937 gen(rd());

    // Pre-processing
    auto *dp = new DataParser(file_name);
    Graph *g = dp->parse();
    delete dp;
    if (g == nullptr) {
        return 1;
    }
    g->compute_matrix();
    long double avg_cost = 0.0;
    double avg_steps = 0.0;
    uint64_t best_cost = UINT64_MAX;
    size_t max_it = g->no_nodes > 1000 ? 100 : g->no_nodes;
    for(size_t i = 0; i < max_it; i++)
    {
        auto* tsp = new size_t[g->no_nodes];
        for(size_t j = 0; j < g->no_nodes; j++)
        {
            tsp[j] = j;
        }
        // Random permutation
        std::shuffle(tsp, tsp + g->no_nodes, gen);
        // Calculate init_cost
        uint64_t total_cost = 0;
        for(size_t j = 1; j < g->no_nodes; j++)
        {
            total_cost += g->dist_matrix[tsp[j-1]][tsp[j]];
        }
        total_cost += g->dist_matrix[tsp[0]][tsp[g->no_nodes - 1]];
        // Local search
        auto result = local_search(tsp, *g, total_cost);
        total_cost = result.first;
        avg_cost +=
                static_cast<long double>(total_cost) / static_cast<long double>(max_it);
        avg_steps +=
                static_cast<double>(result.second) / static_cast<double>(max_it);
        if(best_cost > total_cost) {
            best_cost = total_cost;
        }
        std::cout << i << std::endl;
        delete[] tsp;
    }
    std::cout << "Avg. solution cost: " << avg_cost << std::endl;
    std::cout << "Avg. steps: " << avg_steps << std::endl;
    std::cout << "The best solution: " << best_cost << std::endl;

    delete g;
    return 0;
}
