#include <genetic_algorithm.h>
#include <algorithm>
#include <primAlgorithm.h>
#include <stack>

static size_t* solve_tsp(std::vector<TreeNode*>& nodes, const Graph& g,
                         uint64_t* cost, const size_t node_id)
{
    uint64_t total_cost = 0;
    std::stack<TreeNode*> stk;
    auto* tsp = new size_t[g.no_nodes];

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


void GA::generate_random_island()
{
    std::random_device rand;
    std::mt19937 gen(rand());

    std::vector<Person*> persons;
    for(size_t i = 0; i < graph->no_nodes; i++) {
        auto* genotype = new size_t[graph->no_nodes];
        for (size_t j = 0; j < graph->no_nodes; j++) {
            genotype[j] = j;
        }
        // Random permutation
        std::shuffle(genotype, genotype + graph->no_nodes, gen);
        // Calculate phenotype
        uint64_t phenotype = 0;
        for(size_t j = 1; j < graph->no_nodes; j++)
        {
            phenotype += graph->dist_matrix[genotype[j-1]][genotype[j]];
        }
        phenotype += graph->dist_matrix[genotype[0]][genotype[graph->no_nodes - 1]];

        persons.push_back(new Person(genotype, phenotype));
    }

    std::sort(persons.begin(), persons.end(), Person::compare_by_phenotype);
    islands.push_back(new Island(persons, graph));
}

void GA::generate_mst_island()
{
    std::vector<Person*> persons;
    // Find MST
    std::vector<TreeNode*> mst = prim_find_MST(graph);        // MST
    for(size_t i = 0; i < graph->no_nodes; i++) {
        uint64_t cost = 0;
        size_t *tsp = solve_tsp(mst, *graph, &cost, i);
        persons.push_back(new Person(tsp, cost));
    }
    islands.push_back(new Island(persons, graph));
}

void GA::solve() {
    for(size_t i = 0; i < islands.size(); i++)
    {
        islands[i]->start(graph->dist_matrix, graph->no_nodes, i+1);
    }
}

GA::GA(Graph* g) {
    graph = g;
}

GA::~GA() {
    for( auto & island : islands)
    {
        delete island;
    }
}

