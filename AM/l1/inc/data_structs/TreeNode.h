

#ifndef L1_TREENODE_H
#define L1_TREENODE_H

#include <cstdint>
#include <vector>

class TreeNode{
public:
    TreeNode* parent;
    std::size_t id;
    uint16_t x;
    uint16_t y;
    std::vector<TreeNode*> children;

    TreeNode(std::size_t id ,uint16_t x, uint16_t y, TreeNode* parent);

    void addEdge(TreeNode* child);
    void removeEdge(TreeNode* child);
    void deleteTree();
};
#endif //L1_TREENODE_H
