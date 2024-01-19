cmake_minimum_required(VERSION 3.22)
project(l4)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_COMPILER "g++")
set(CMAKE_CXX_FLAGS "-O3 -Wall")

set(SOURCES ../l2/src/DataParser.cpp
        ../l2/src/data_structs/Graph.cpp
        ../l2/src/data_structs/Node.cpp
        ../l2/src/primAlgorithm.cpp
        ../l2/src/data_structs/TreeNode.cpp
        src/genetic_algorithm.cpp
        src/island.cpp
        src/person.cpp
        src/random_device.cpp
)

add_executable(l4.out app/main.cpp ${SOURCES})
target_include_directories(l4.out PRIVATE inc ../l2/inc)
