#include <iostream>
#include <DataParser.h>
#include <primAlgorithm.h>
#include <stack>
#include <cstring>
#include <cstdlib>
#include <Visualizer.h>

using namespace std;

int main(int argc, char* argv[]) {
    if(argc < 2)
    {
        cerr << "Please provide file_name as parameter" << endl;
        return 1;
    }
    char* file_name = argv[1];
    char* visualize_type = nullptr;
    if(argc < 3)
        visualize_type = argv[2];

    // Pre-processing
    auto* dp = new DataParser(file_name);
    Graph* g = dp->parse();
    delete dp;
    if(g == nullptr) {
        return 1;
    }
    g->compute_matrix();

    // Prim's MST
    vector<TreeNode*> mst_root = primFindMST(g);

    // Visualize
    const char* graphviz_command =
            "neato -Tpng /home/sebastian/CLionProjects/INA/AM/l1/graph.dot -o /home/sebastian/CLionProjects/INA/AM/l1/graph.png";

    auto* v = new Visualizer(mst_root);
    if(visualize_type == nullptr || strcmp(visualize_type, "MST") == 0)
    {
        v->visualize_mst();
        int ret = system(graphviz_command);
        if( ret != 0 )
            cerr << "Failed to run Graphviz" << endl;
    }
    else if(strcmp(visualize_type, "TSP") == 0) {
        v->visualize_tsp();
        int ret = system(graphviz_command);
        if( ret != 0 )
            cerr << "Failed to run Graphviz" << endl;
    }

    // Cleanup
    mst_root[0]->deleteTree();
    delete g;
    delete v;
    return 0;
}
