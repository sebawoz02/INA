#include <iostream>
#include <DataParser.h>
#include <genetic_algorithm.h>


int main(int argc, char** argv)
{
    if (argc < 2) {
        std::cerr << "Please provide file_name as parameter" << std::endl;
        return 1;
    }
    char *file_name = argv[1];

    // Pre-processing
    auto *dp = new DataParser(file_name);
    Graph *g = dp->parse();
    delete dp;
    if (g == nullptr) {
        return 1;
    }
    g->compute_matrix();

    GA* genetic_algorithm = new GA(g);

    genetic_algorithm->generate_mst_island();

    genetic_algorithm->solve();

    delete g;
    delete genetic_algorithm;

    return 0;
}
