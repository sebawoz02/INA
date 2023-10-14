#include <Visualizer.h>
#include <iostream>
#include <fstream>
#include <stack>
#include <algorithm>

Visualizer::Visualizer(std::vector<TreeNode*> nodes) {
    this->nodes = nodes;
}

uint64_t Visualizer::visualize_mst() {
    uint64_t total_cost = 0;

    std::ofstream dotFile;
    dotFile.open("/home/sebastian/CLionProjects/INA/AM/l1/graph.dot");
    if(!dotFile.is_open())
        std::cerr << "Visualizer failed to open .dot file" << std::endl;

    dotFile << "graph G {\n";

    // Generate nodes
    for(size_t i = 0; i < nodes.size(); i++)
    {
        dotFile << " " << i + 1;
        dotFile << R"( [fillcolor="darkblue", style="filled")";
        dotFile << ", pos=\"" << nodes[i]->x << "," << nodes[i]->y << "!\"];\n";
        total_cost += nodes[i]->cost;
    }

    dotFile << "\n";
    // Generate edges
    for(size_t i = 1; i < nodes.size(); i++)
    {
        dotFile << " " << nodes[i]->parent->id + 1 << " -- " << i + 1 << " [color=\"red\", penwidth=8];\n";
    }
    dotFile << "}\n";
    dotFile.close();

    return total_cost;
}

uint64_t Visualizer::visualize_tsp(uint32_t** dist_matrix) {
    uint64_t total_cost = 0;

    std::ofstream dotFile;
    dotFile.open("/home/sebastian/CLionProjects/INA/AM/l1/graph.dot");
    if(!dotFile.is_open())
        std::cerr << "Visualizer failed to open .dot file" << std::endl;

    dotFile << "graph G {\n";

    // Generate nodes
    for(size_t i = 0; i < nodes.size(); i++)
    {
        dotFile << " " << i + 1;
        dotFile << R"( [fillcolor="darkblue", style="filled")";
        dotFile << ", pos=\"" << nodes[i]->x << "," << nodes[i]->y << "!\"];\n";
    }

    // Pre-order walk to solve TSP
    std::stack<TreeNode*> stk;
    stk.push(nodes[0]);
    TreeNode* prev = nullptr;
    while ( !stk.empty() ){
        auto* node = stk.top();
        std::cout << node->id + 1 << " ";
        stk.pop();
        if(prev != nullptr)
        {
            total_cost += dist_matrix[prev->id][node->id];
            dotFile << " " << prev->id + 1 << " -- " << node->id + 1 << " [color=\"red\", penwidth=8];\n";
        }
        prev = node;
        std::sort(node->children.begin(), node->children.end(), [](TreeNode* a, TreeNode* b){
            return a->id < b->id;
        });
        for(auto i : node->children)
            stk.push(i);
    }
    dotFile << " " << prev->id + 1 << " -- " << nodes[0]->id + 1 << " [color=\"red\", penwidth=8];\n";
    dotFile << "}\n";
    dotFile.close();
    std::cout << std::endl;
    total_cost += dist_matrix[prev->id][0];
    return total_cost;
}
