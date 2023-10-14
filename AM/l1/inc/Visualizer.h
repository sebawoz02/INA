#ifndef L1_VISUALIZER_H
#define L1_VISUALIZER_H

#include <data_structs/TreeNode.h>

class Visualizer{
private:
    std::vector<TreeNode*>  nodes;
public:
    explicit Visualizer(std::vector<TreeNode*>  node);
    void visualize_mst();
    void visualize_tsp();
};

#endif //L1_VISUALIZER_H
