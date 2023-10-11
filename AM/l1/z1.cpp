#include <iostream>
#include "DataParser.h"

using namespace std;

int main(int argc, char* argv[]) {
    if(argc < 2)
    {
        cerr << "Please provide file_name as parameter" << endl;
        return 1;
    }
    char* file_name = argv[1];
    auto* dp = new DataParser(file_name);
    auto* g = dp->parse();

    if(g != nullptr) {
        g->compute_matrix();
        delete g;
    } else
        return 1;

    delete dp;
    return 0;
}
