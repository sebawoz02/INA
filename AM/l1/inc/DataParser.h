//
// Created by sebastian on 10/11/23.
//

#ifndef L1_DATAPARSER_H
#define L1_DATAPARSER_H

#include "Graph.h"

class DataParser {
public:
    char* file_name;

    explicit DataParser(char* file_name);

    Graph* parse() const;
};

#endif //L1_DATAPARSER_H
