#pragma once

#include <pixel.hpp>
#include <string>
#include <vector>

struct Node;

class LBG_tree {
public:
  LBG_tree(std::vector<std::vector<Pixel>>& image, size_t size);
  Pixel next_node(Pixel pixel);
  static int distance(Pixel p1, Pixel p2);

private:
  void linde_buzo_gray(const std::vector<Pixel>& pixels, Node* node);

  Node* root;
  size_t height;
};
