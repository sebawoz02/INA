#include <Visualizer.h>
#include <iostream>
#include <fstream>

Visualizer::Visualizer(std::vector<TreeNode*> nodes) {
    this->nodes = nodes;
}

void Visualizer::visualize_mst() {
    std::ofstream dotFile;
    dotFile.open("/home/sebastian/CLionProjects/INA/AM/l1/graph.dot");
    if(!dotFile.is_open())
        std::cerr << "Visualizer failed to open .dot file" << std::endl;

    dotFile << "graph G {\n";

    // Generate nodes
    for(size_t i = 0; i < nodes.size(); i++)
    {
        dotFile << " " << i;
        dotFile << R"( [fillcolor="darkblue", style="filled")";
        dotFile << ", pos=\"" << nodes[i]->x << "," << nodes[i]->y << "!\"];\n";
    }

    dotFile << "\n";
    // Generate edges
    for(size_t i = 1; i < nodes.size(); i++)
    {
        dotFile << " " << nodes[i]->parent->id << " -- " << i << " [color=\"red\", penwidth=8];\n";
    }
    dotFile << "}\n";
    dotFile.close();
}

void Visualizer::visualize_tsp() {

}

