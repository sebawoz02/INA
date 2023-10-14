#ifndef L1_VISUALIZER_H
#define L1_VISUALIZER_H

#include <data_structs/TreeNode.h>

class Visualizer{
private:
    std::vector<TreeNode*>  nodes;
public:
    explicit Visualizer(std::vector<TreeNode*>  node);
    uint64_t visualize_mst();
    uint64_t visualize_tsp(uint32_t** dist_matrix);
};

#endif //L1_VISUALIZER_H
