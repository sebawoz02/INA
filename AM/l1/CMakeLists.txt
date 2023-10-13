cmake_minimum_required(VERSION 3.22)
project(l1)

set(CMAKE_CXX_STANDARD 17)

set(SOURCES src/DataParser.cpp
            src/Node.cpp
        src/primAlgorithm.cpp
            src/Graph.cpp)

add_executable(z1 app/z1.cpp ${SOURCES})
# add_executable(z2 app/z2.cpp ${SOURCES})

target_include_directories(z1 PRIVATE inc)
# target_include_directories(z2 PRIVATE inc)