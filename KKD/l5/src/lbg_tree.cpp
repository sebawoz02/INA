//
// Created by sebawoz02 on 13.12.23.
//
#include <algorithm>
#include <cmath>
#include <lbg_tree.h>
#include <random>

static void set_avg_distance(Node* node, std::vector<Pixel>& pixels);

struct Node {
  std::string code;
  Pixel pixel;
  Node* left;
  Node* right;
};

static void set_avg_distance(Node* node, std::vector<Pixel>& pixels)
{
  uint64_t red = 0;
  uint64_t green = 0;
  uint64_t blue = 0;
  size_t no_pixels = pixels.size();

  for(Pixel p: pixels) {
    red += p.red;
    green += p.green;
    blue += p.blue;
  }

  if(no_pixels > 0) {
    node->pixel.red = red / no_pixels;
    node->pixel.green = green / no_pixels;
    node->pixel.blue = blue / no_pixels;
  }
}

LBG_tree::LBG_tree(std::vector<std::vector<Pixel>>& image, size_t size)
{
  this->root =
    new Node{.code = "", .pixel = Pixel(), .left = nullptr, .right = nullptr};
  this->height = size;

  size_t img_height = image.size();
  size_t img_width = image[0].size();
  std::vector<Pixel> pixels(img_height * img_width);

  // Load original pixels
  for(size_t i = 0; i < img_height; i++) {
    for(size_t j = 0; j < img_width; j++) {
      pixels[i * img_width + j] = image[i][j];
    }
  }

  set_avg_distance(this->root, pixels); // Avg color
  this->linde_buzo_gray(pixels, this->root);
}

void LBG_tree::linde_buzo_gray(const std::vector<Pixel>& pixels, Node* node)
{
  if(node->code.size() == this->height)
    return;

  std::random_device rd;
  std::mt19937 generator(rd());
  std::uniform_int_distribution<int> dist(-2, 2);

  node->left = new Node{.code = node->code + "0",
                        .pixel = node->pixel,
                        .left = nullptr,
                        .right = nullptr};
  node->right = new Node{.code = node->code + "1",
                         .pixel = node->pixel,
                         .left = nullptr,
                         .right = nullptr};

  // Slightly change colors. Repeat if left and right pixels are the same color
  do {
    node->right->pixel.red =
      std::max(0, std::min(255, (int)node->right->pixel.red + dist(generator)));
    node->right->pixel.green = std::max(
      0, std::min(255, (int)node->right->pixel.green + dist(generator)));
    node->right->pixel.blue = std::max(
      0, std::min(255, (int)node->right->pixel.blue + dist(generator)));
  } while(node->left->pixel.red == node->right->pixel.red &&
          node->left->pixel.green == node->right->pixel.green &&
          node->left->pixel.blue == node->right->pixel.blue);

  std::vector<Pixel> left_pixels; // Pixels more similar to left node pixel
  std::vector<Pixel> right_pixels; // Pixels more similar to right node pixel

  Pixel prev_left_pixel;
  Pixel prev_right_pixel;

  do {
    prev_left_pixel = node->left->pixel;
    prev_right_pixel = node->right->pixel;

    left_pixels.clear();
    right_pixels.clear();

    for(Pixel pixel: pixels) {
      // Decide on which side this pixel should be
      if(LBG_tree::distance(pixel, node->left->pixel) <
         LBG_tree::distance(pixel, node->right->pixel)) {
        left_pixels.push_back(pixel);
      } else {
        right_pixels.push_back(pixel);
      }
    }

    set_avg_distance(node->left, left_pixels);
    set_avg_distance(node->right, right_pixels);
    // Repeat if it's the same color as it was before
  } while(!node->left->pixel.are_equal(prev_left_pixel) ||
          !node->right->pixel.are_equal(prev_right_pixel));

  // Get more colors if num of colors not reached yet
  this->linde_buzo_gray(left_pixels, node->left);
  this->linde_buzo_gray(right_pixels, node->right);
}

Pixel LBG_tree::next_node(Pixel pixel)
{
  Node* node = this->root;
  while(node->left != nullptr) {
    if(LBG_tree::distance(pixel, node->left->pixel) <
       LBG_tree::distance(pixel, node->right->pixel)) {
      node = node->left;
    } else {
      node = node->right;
    }
  }
  return node->pixel;
}

int LBG_tree::distance(Pixel p1, Pixel p2)
{
  return abs(p1.red - p2.red) + abs(p1.green - p2.green) +
         abs(p1.blue - p2.blue);
}
