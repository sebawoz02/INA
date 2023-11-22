#ifndef L1_DATAPARSER_H
#define L1_DATAPARSER_H

#include "data_structs/Graph.h"

class DataParser {
public:
  char* file_name;

  explicit DataParser(char* file_name);

  Graph* parse() const;
};

#endif //L1_DATAPARSER_H
