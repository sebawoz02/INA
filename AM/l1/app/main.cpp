#include <iostream>
#include <vector>
#include <cstring>
#include <cstdlib>
#include <fstream>
#include <algorithm>
#include <random>

#include <DataParser.h>
#include <primAlgorithm.h>
#include <Visualizer.h>
#include <timer.h>

using namespace std;

int main(int argc, char* argv[]) {
    if(argc < 2)
    {
        cerr << "Please provide file_name as parameter" << endl;
        return 1;
    }
    char* visualize_type = nullptr;
    bool visualize = false;
    bool random_permutation = false;
    size_t no_groups;
    size_t reps;
    for (int i = 2; i < argc; ++i) {
        if (strcmp(argv[i], "-t") == 0) {
            if (i + 1 < argc) {
                visualize_type = argv[i+1];
                i++;
            } else {
                cerr << "Missing value for -t (MST or TSP)" << endl;
            }
        } else if (strcmp(argv[i], "-v") == 0) {
            // Check for the -v flag
            visualize = true;
        } else if (strcmp(argv[i], "--random") == 0)
        {
            random_permutation = true;
            if (i + 2 < argc) {
                no_groups = atoi(argv[i+1]);
                reps = atoi(argv[i+2]);
                i += 2;
            } else {
                cerr << "Missing values for --random <no_groups> <reps>" << endl;
            }
        }
        else {
            cerr << "Unknown option: " << argv[i] << endl;
        }
    }

    auto* timer = new Timer();

    char* file_name = argv[1];

    // Pre-processing
    timer->start("Processing data");
    auto* dp = new DataParser(file_name);
    Graph* g = dp->parse();
    delete dp;
    if(g == nullptr) {
        return 1;
    }
    g->compute_matrix();
    timer->stop();

    if(!random_permutation) {
        // Prim's MST
        timer->start("Finding MST");
        vector<TreeNode *> mst_root = primFindMST(g);
        timer->stop();

        // Visualize
        const char *graphviz_command =
                "neato -Tpng /home/sebastian/CLionProjects/INA/AM/l1/graph.dot -o /home/sebastian/CLionProjects/INA/AM/l1/graph.png";

        auto *v = new Visualizer(mst_root);
        uint64_t total_cost;
        if (visualize_type == nullptr || strcmp(visualize_type, "MST") == 0) {
            timer->start("Creating visualization of MST");
            total_cost = v->visualize_mst();
            cout << "\nMSP total weight: " << total_cost << endl;
            if (visualize) {
                int ret = system(graphviz_command);
                if (ret != 0)
                    cerr << "Failed to run Graphviz" << endl;
            }
        } else if (strcmp(visualize_type, "TSP") == 0) {
            timer->start("Creating visualization of TSP");
            total_cost = v->visualize_tsp(g->dist_matrix);
            cout << "TSP total weight: " << total_cost << endl;
            if (visualize) {
                int ret = system(graphviz_command);
                if (ret != 0)
                    cerr << "Failed to run Graphviz" << endl;
            }
        }
        timer->stop();
        mst_root[0]->deleteTree();
        delete v;
    } else
    {
        random_device rd;
        mt19937 rng(rd());
        const size_t no_nodes = g->no_nodes;
        fstream file;
        file.open("/home/sebastian/CLionProjects/INA/AM/l1/am_l1.txt");
        for(size_t i = 0; i < no_groups; i++)
        {
            for(size_t ii = 0; ii < reps; ii++){
                for(size_t perm = 0; perm < 1000; perm++)
                {
                    vector<size_t> numbers(no_nodes);
                    for(size_t j = 0; j < no_nodes; j++)
                        numbers[j] = j;

                    shuffle(numbers.begin(), numbers.end(), rng);

                    uint64_t cycle_cost = g->dist_matrix[numbers[no_nodes-1]][numbers[0]];
                    for(size_t j = 1; j < no_nodes; j++)
                        cycle_cost += g->dist_matrix[numbers[j-1]][numbers[j]];

                    file << cycle_cost << endl;
                }
            }
        }
        file.close();
    }
    // Cleanup
    delete g;
    delete timer;
    return 0;
}
