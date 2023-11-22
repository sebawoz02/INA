#include "data_structs/TreeNode.h"
#include <algorithm>
#include <stack>

TreeNode::TreeNode(const size_t id, const uint16_t x, const uint16_t y,
                   const uint32_t cost, TreeNode* parent)
{
  this->id = id;
  this->x = x;
  this->y = y;
  this->cost = cost;
  this->parent = parent;
}

void TreeNode::addEdge(TreeNode* child)
{
  this->children.push_back(child);
}

void TreeNode::deleteTree()
{
  std::stack<TreeNode*> stk;
  for(auto i: this->children)
    stk.push(i);
  while(!stk.empty()) {
    TreeNode* top = stk.top();
    stk.pop();
    for(auto i: top->children)
      stk.push(i);
    delete top;
  }
  delete this;
}
