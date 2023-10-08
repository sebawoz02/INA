#include <iostream>

#include "DGraph.h"
#include "HopcrofKarp.h"

int main(int argc, char** argv)
{
    size_t k = 7;
    cout << "Generowanie grafu..." << endl;
    auto* g = new DGraph(k, 3);
    cout << "Obliczanie maksymalnego skojarzenia..." << endl;
    auto* hk = new HopcroftKarp(g, g->no_verticies);

    cout << "Maksymalne skojarzenie: " << hk->findMaxMatching() << endl;

    return 0;
}
