#include <island.h>
#include <algorithm>
#include <iostream>
#include <unordered_set>
#include <utility>


#define NUM_OF_ITERATIONS 2000
#define NUM_OF_ISLAND_ITERATIONS 2000

#define MUTATION_PROBABILITY 0.1f


static int32_t calc_mutation_cost(const size_t* tsp, const size_t i, const size_t j,
                             uint64_t** dist_matrix, const size_t max_n)
{
    uint64_t plus = 0;
    uint64_t minus = 0;
    if(i != 0)
    {
        size_t tmp = tsp[i - 1];
        minus += dist_matrix[tmp][tsp[i]];
        plus += dist_matrix[tmp][tsp[j]];
    }
    else if(j >= max_n - 2)
    {
        return 0;
    }
    else
    {
        size_t tmp = tsp[max_n - 1];
        plus += dist_matrix[tsp[j]][tmp];
        minus += dist_matrix[tsp[i]][tmp];
    }
    if(j != max_n - 1)
    {
        size_t tmp = tsp[j + 1];
        minus += dist_matrix[tmp][tsp[j]];
        plus += dist_matrix[tmp][tsp[i]];
    }
    else if(i == 1)
    {
        return 0;
    }
    else
    {
        size_t tmp = tsp[0];
        plus += dist_matrix[tsp[i]][tmp];
        minus += dist_matrix[tsp[j]][tmp];
    }
    return static_cast<int32_t>(plus) - static_cast<int32_t>(minus);
}



void Island::start(uint64_t** dist_matrix, const size_t n, const size_t id) {
    uint64_t best = UINT64_MAX;

    for(size_t it = 0; it < NUM_OF_ISLAND_ITERATIONS/NUM_OF_ITERATIONS; it++)
    {
        // STEPS 2-4
        for(size_t island_it = 0; island_it<NUM_OF_ISLAND_ITERATIONS; island_it++)
        {

            // STEPS 2 and 3 - Selection and crossing
            std::vector<Person*> new_population;
            size_t new_population_size = 0;
            size_t i = 0;
            size_t j = 1;
            while(new_population_size < n)
            {
                auto kids = pmx_crossing(i, j, dist_matrix, n);
                Person* kid1 = kids.first;
                Person* kid2 = kids.second;

                // STEP 4 - Mutations
                // mutate with specified probability
                if(rd->get_random_float() < MUTATION_PROBABILITY) {
                    auto p = rd->get_random_points();
                    int32_t tmp = calc_mutation_cost(kid1->genotype, p.first, p.second, dist_matrix, n);
                    kid1->mutate(p.first, p.second, kid1->phenotype + tmp);
                }

                if(rd->get_random_float() < MUTATION_PROBABILITY) {
                    auto p = rd->get_random_points();
                    int32_t tmp = calc_mutation_cost(kid2->genotype, p.first, p.second, dist_matrix, n);
                    kid1->mutate(p.first, p.second, kid2->phenotype + tmp);
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

            for(i = 0 ; i < n; i++)
            {
                delete[] persons[i]->genotype;
                delete persons[i];
            }
            persons = new_population;
            std::sort(persons.begin(), persons.end(), Person::compare_by_phenotype);
            if(persons[0]->phenotype < best)
            {
                best = persons[0]->phenotype;
            }
        }

        // MULTI-ISLAND CROSSING HERE

    }

    std::cout << "Best solution cost - island " << id << ": " << best << std::endl;
}


Island::Island(std::vector<Person *> ps, Graph* g) {
    persons = std::move(ps);
    rd = new Random_Device(g->no_nodes);
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

