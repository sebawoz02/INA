#include <person.h>

void Person::mutate(size_t i, size_t j, uint64_t new_phenotype){
    for(size_t it = 0; i + it < j - it; it++) {
        size_t tmp = genotype[i + it];
        genotype[i + it] = genotype[j - it];
        genotype[j - it] = tmp;
    }
    phenotype = new_phenotype;
}


Person::Person(size_t *gen, uint64_t phen) {
    genotype = gen;
    phenotype = phen;
}


bool Person::compare_by_phenotype(const Person *a, const Person *b) {
    return a->phenotype < b->phenotype;
}