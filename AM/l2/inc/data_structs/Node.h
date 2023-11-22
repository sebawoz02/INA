#ifndef L1_NODE_H
#define L1_NODE_H

#include <cstdint>

class Node {
public:
  uint16_t x;
  uint16_t y;

  Node(uint16_t x, uint16_t y);
  ~Node();
};

#endif //L1_NODE_H
