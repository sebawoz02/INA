#pragma once

#include <cstdint>
#include <cstddef>

struct Person
{
    size_t* genotype;
    uint64_t phenotype;

    Person(size_t* gen, uint64_t phen);

    void mutate(size_t i, size_t j, uint64_t new_phenotype);

    static bool compare_by_phenotype(const Person* a, const Person* b);
};
