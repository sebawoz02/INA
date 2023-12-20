cmake_minimum_required(VERSION 3.22)
project(l1)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_COMPILER "g++")
set(CMAKE_CXX_FLAGS "-O3 -Wall")

set(SOURCES src/DataParser.cpp
        src/data_structs/Node.cpp
        src/primAlgorithm.cpp
        src/data_structs/Graph.cpp src/data_structs/TreeNode.cpp
        src/tabu_search.cpp
        src/sim_annealing.cpp
)

add_executable(tabu_random app/tabu_rand.cpp ${SOURCES})
add_executable(tabu_mst app/tabu_mst.cpp ${SOURCES})
add_executable(annealing app/annealing.cpp ${SOURCES})
target_include_directories(tabu_random PRIVATE inc)
target_include_directories(tabu_mst PRIVATE inc)
target_include_directories(annealing PRIVATE inc)