#pragma once

#include <vector>
#include <data_structs/Graph.h>
#include <person.h>
#include <random_device.h>


struct Island{
public:
    std::vector<Person*> persons;

    Island(std::vector<Person*> ps, Graph* g, size_t size);

    void start(Graph* g, size_t id);

private:
    Random_Device* rd;
    size_t population_size;
    std::pair<Person*, Person*> pmx_crossing(size_t i, size_t j, uint64_t** dist_matrix, size_t n);
    std::pair<Person*, Person*> ox_crossing(size_t i, size_t j, uint64_t** dist_matrix, size_t n);
};
