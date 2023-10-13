#include <iostream>
#include <DataParser.h>
#include <primAlgorithm.h>

using namespace std;

int main(int argc, char* argv[]) {
    if(argc < 2)
    {
        cerr << "Please provide file_name as parameter" << endl;
        return 1;
    }
    char* file_name = argv[1];

    // Pre-processing
    auto* dp = new DataParser(file_name);
    Graph* g = dp->parse();
    delete dp;
    if(g == nullptr) {
        return 1;
    }

    g->compute_matrix();

    // Prim's MST
    vector<Node*> mst = primFindMST(g);

    // Visualize

    delete g;
    return 0;
}
