//
// Created by sebastian on 10/11/23.
//

#include "Node.h"

Node::Node(std::size_t id, uint16_t x, uint16_t y) {
    this->id = id;
    this->x = x;
    this->y = y;
}

Node::~Node() = default;
