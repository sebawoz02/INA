//
// Created by sebastian on 10/11/23.
//
#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <vector>
#include <regex>
#include "DataParser.h"

DataParser::DataParser(char *file_name) {
    this->file_name = file_name;
}

Graph *DataParser::parse() const
{
    std::ifstream inputFile(this->file_name);
    if (!inputFile.is_open()) {
        std::cerr << "Failed to open the input file." << std::endl;
        return nullptr;
    }

    std::string line;
    std::regex dimRegex("DIMENSION : (\\d+)");
    size_t no_nodes = 0;
    Graph* g = nullptr;

    while (std::getline(inputFile, line)) {
        std::smatch match;
        if (no_nodes == 0 && std::regex_search(line, match, dimRegex) && match.size() == 2) {
            // Extract the value from "DIMENSION: <value>"
            no_nodes = std::stoi(match[1]);
            if(no_nodes == 0)
            {
                std::cerr << "Number of nodes cannot be equal to 0" << std::endl;
                return nullptr;
            }
            g = new Graph(no_nodes);
        }
        else if (line.find("NODE_COORD_SECTION") != std::string::npos && g != nullptr) {
            // Start reading data
            size_t id;
            uint16_t x, y;
            while (std::getline(inputFile, line)) {
                std::istringstream ss(line);
                if(ss >> id) {
                    ss >> x;
                    ss >> y;
                    g->addNode(x, y);
                }
            }
        }
    }
    inputFile.close();
    if(g == nullptr)
    {
        std::cerr << "DIMENSION not found in file" << std::endl;
    }
    return g;
}
