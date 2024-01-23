#include <island.h>
#include <algorithm>
#include <iostream>
#include <unordered_set>
#include <utility>
#include <local_search.h>


#define NUM_OF_ISLAND_GENERATIONS 1000

#define MUTATION_PROBABILITY 0.10f


void Island::start(Graph* g, const size_t id) {
    uint64_t best = UINT64_MAX;
    size_t NUM_OF_GENERATIONS_WITHOUT_NEW_BEST = g->no_nodes;

    size_t without_new_best = 0;
    for(;;)
    {
        // STEPS 2-4
        for(size_t island_it = 0; island_it < NUM_OF_ISLAND_GENERATIONS; island_it++)
        {

            // STEPS 2 and 3 - Selection and crossing
            std::vector<Person*> new_population;
            size_t new_population_size = 0;
            size_t i = 0;
            size_t j = 1;
            while(new_population_size < population_size)
            {
                auto kids = pmx_crossing(i, j, g->dist_matrix, g->no_nodes);
                Person* kid1 = kids.first;
                Person* kid2 = kids.second;

                // STEP 4 - Mutations
                // mutate with specified probability
                if(rd->get_random_float() < MUTATION_PROBABILITY) {
                    /*
                    auto p = rd->get_random_points();
                    int32_t tmp = calc_mutation_cost(kid1->genotype, p.first, p.second, dist_matrix, n);
                    kid1->mutate(p.first, p.second, kid1->phenotype + tmp);
                    */
                    uint64_t new_phenotype = local_search(kid1->genotype, *g, kid1->phenotype);
                    kid1->phenotype = new_phenotype;
                }

                if(rd->get_random_float() < MUTATION_PROBABILITY) {
                    /*
                    auto p = rd->get_random_points();
                    int32_t tmp = calc_mutation_cost(kid2->genotype, p.first, p.second, dist_matrix, n);
                    kid1->mutate(p.first, p.second, kid2->phenotype + tmp);
                    */
                    uint64_t new_phenotype = local_search(kid2->genotype, *g, kid2->phenotype);
                    kid2->phenotype = new_phenotype;
                }

                new_population.push_back(kid1);
                new_population.push_back(kid2);
                new_population_size += 2;

                if(i+1 == j){
                    i = 0;
                    j++;
                } else{
                    i++;
                }
            }

            // Remove all parents except the best one
            for(i = 1 ; i <= j; i++)
            {
                delete[] persons[i]->genotype;
                delete persons[i];
            }
            // Add prev generation
            for(;i < population_size;i++)
            {
                new_population.push_back(persons[i]);
            }
            new_population.push_back(persons[0]);

            // Select only the best ones
            std::sort(new_population.begin(), new_population.end(), Person::compare_by_phenotype);
            for(i = new_population.size() - 1; i >= population_size; i--)
            {
                delete[] new_population[i]->genotype;
                delete new_population[i];
                new_population.pop_back();
            }

            persons = new_population;

            if(persons[0]->phenotype < best)
            {
                best = persons[0]->phenotype;
                without_new_best = 0;
            } else{
                if(++without_new_best == NUM_OF_GENERATIONS_WITHOUT_NEW_BEST)
                    break;
            }
        }
        if(without_new_best == NUM_OF_GENERATIONS_WITHOUT_NEW_BEST)
            break;

        // ISLAND MIGRATION HERE

    }

    std::cout << "Best solution cost - island " << id << ": " << best << std::endl;
}


Island::Island(std::vector<Person *> ps, Graph* g, size_t size) {
    persons = std::move(ps);
    rd = new Random_Device(g->no_nodes);
    population_size = size;
}


std::pair<Person*, Person*> Island::pmx_crossing(size_t i, size_t j, uint64_t** dist_matrix, size_t n) {
    // Choose two random split points
    auto sp = rd->get_random_points();

    auto* genotype_1 = new size_t[n];
    auto* genotype_2 = new size_t[n];

    for(size_t it = 0; it < n; it++) {
        genotype_1[it] = persons[i]->genotype[it];
        genotype_2[it] = persons[j]->genotype[it];
    }

    uint64_t phenotype_1 = 0;
    uint64_t phenotype_2 = 0;

    size_t swaps;
    // Match section [sp.first, sp.second]
    do {
        std::unordered_set<size_t> section_1;
        std::unordered_set<size_t> section_2;
        for(size_t it = sp.first; it <= sp.second; it++)
        {
            section_1.insert(genotype_1[it]);
            section_2.insert(genotype_2[it]);
        }
        swaps = 0;
        for (size_t it = sp.first; it <= sp.second; it++) {
            bool swapped_i = false;
            bool swapped_j = false;

            size_t val_i = persons[i]->genotype[it];
            size_t val_j = persons[j]->genotype[it];
            if (section_2.find(val_i) != section_2.end())
                swapped_j = true;
            if (section_1.find(val_j) != section_1.end())
                swapped_i = true;


            for (size_t it2 = 0; it2 < n && (!swapped_i || !swapped_j); it2++) {
                if (!swapped_i && genotype_1[it2] == val_j) {
                    genotype_1[it2] = genotype_1[it];
                    genotype_1[it] = val_j;
                    swapped_i = true;
                    swaps++;
                }
                if (!swapped_j && genotype_2[it2] == val_i) {
                    genotype_2[it2] = genotype_2[it];
                    genotype_2[it] = val_i;
                    swapped_j = true;
                    swaps++;
                }

                if (it2 + 1 == sp.first) {
                    it2 = sp.second;
                }
            }
        }
    } while (swaps != 0);

    // Swap sections
    for(size_t it = sp.first; it <= sp.second; it++) {
        genotype_1[it] = persons[j]->genotype[it];
        genotype_2[it] = persons[i]->genotype[it];
    }

    // Calculate new phenotypes
    for(size_t it = 1; it < n; it++)
    {
        phenotype_1 += dist_matrix[genotype_1[it-1]][genotype_1[it]];
        phenotype_2 += dist_matrix[genotype_2[it-1]][genotype_2[it]];
    }
    phenotype_1 += dist_matrix[genotype_1[0]][genotype_1[n - 1]];
    phenotype_2 += dist_matrix[genotype_2[0]][genotype_2[n - 1]];

    auto* kid1 = new Person(genotype_1, phenotype_1);
    auto* kid2 = new Person(genotype_2, phenotype_2);
    return {kid1, kid2};
}

std::pair<Person *, Person *> Island::ox_crossing(size_t i, size_t j, uint64_t **dist_matrix, size_t n) {
    // Choose two random split points
    auto sp = rd->get_random_points();

    auto* genotype_1 = new size_t[n];
    auto* genotype_2 = new size_t[n];
    std::unordered_set<size_t> section_1;
    std::unordered_set<size_t> section_2;

    for(size_t it = sp.first; it <= sp.second; it++) {
        genotype_1[it] = persons[i]->genotype[it];
        genotype_2[it] = persons[j]->genotype[it];
        section_1.insert(persons[i]->genotype[it]);
        section_2.insert(persons[j]->genotype[it]);
    }

    // Complete kid 1
    size_t fit_idx = sp.second + 1;
    size_t idx = sp.second + 1;
    while (fit_idx != sp.first)
    {
        size_t val = persons[j]->genotype[idx];
        // Not in gray zone
        if(section_1.find(val) == section_1.end())
        {
            genotype_1[fit_idx] = val;
            if(fit_idx + 1 == n)
                fit_idx = 0;
            else
                fit_idx++;
        }
        if(idx + 1 == n)
            idx = 0;
        else
            idx++;
    }

    // Complete kid 2
    fit_idx = sp.second + 1;
    idx = sp.second + 1;
    while (fit_idx != sp.first)
    {
        size_t val = persons[i]->genotype[idx];
        // Not in gray zone
        if(section_2.find(val) == section_2.end())
        {
            genotype_2[fit_idx] = val;
            if(fit_idx + 1 == n)
                fit_idx = 0;
            else
                fit_idx++;
        }
        if(idx + 1 == n)
            idx = 0;
        else
            idx++;
    }

    uint64_t phenotype_1 = 0;
    uint64_t phenotype_2 = 0;
    // Calculate new phenotypes
    for(size_t it = 1; it < n; it++)
    {
        phenotype_1 += dist_matrix[genotype_1[it-1]][genotype_1[it]];
        phenotype_2 += dist_matrix[genotype_2[it-1]][genotype_2[it]];
    }
    phenotype_1 += dist_matrix[genotype_1[0]][genotype_1[n - 1]];
    phenotype_2 += dist_matrix[genotype_2[0]][genotype_2[n - 1]];

    auto* kid1 = new Person(genotype_1, phenotype_1);
    auto* kid2 = new Person(genotype_2, phenotype_2);
    return {kid1, kid2};
}
