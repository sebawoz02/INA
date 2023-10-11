#include <iostream>
#include "Graph.h"

int main(int argc, char* argv[]) {
    (void) argv;
    (void) argc;
    std::cout << "Hello, World!" << std::endl;

    auto* g = new Graph(10);

    delete g;
    return 0;
}
