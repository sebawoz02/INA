cmake_minimum_required(VERSION 3.22)
project(l1)

set(CMAKE_CXX_STANDARD 17)

set(SOURCES src/DataParser.cpp
        src/data_structs/Node.cpp
        src/primAlgorithm.cpp
        src/data_structs/Graph.cpp src/data_structs/TreeNode.cpp
        src/local_search.cpp
)

add_executable(z1.out app/z1.cpp ${SOURCES})
target_include_directories(z1.out PRIVATE inc)