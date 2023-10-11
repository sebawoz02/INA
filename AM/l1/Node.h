//
// Created by sebastian on 10/11/23.
//

#ifndef L1_NODE_H
#define L1_NODE_H

#include <cstdint>

class Node {
public:
    std::size_t id;
    uint16_t x;
    uint16_t y;

    Node(std::size_t id, uint16_t x, uint16_t y);
    ~Node();
};


#endif //L1_NODE_H
