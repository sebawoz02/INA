cmake_minimum_required(VERSION 3.22)
project(l1)

set(CMAKE_CXX_STANDARD 17)

set(SOURCES src/DataParser.cpp
        src/data_structs/Node.cpp
        src/primAlgorithm.cpp
        src/data_structs/Graph.cpp src/data_structs/TreeNode.cpp src/Visualizer.cpp inc/Visualizer.h
        )

add_executable(main.out app/main.cpp ${SOURCES})
target_link_libraries(main.out PRIVATE ${CMAKE_SOURCE_DIR}/libs/timer/timer.a)
target_include_directories(main.out PRIVATE inc ${CMAKE_SOURCE_DIR}/libs/timer/inc)
