cmake_minimum_required(VERSION 3.22)
project(l5)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_COMPILER "g++")
set(CMAKE_CXX_FLAGS "-Wall -Wextra")

set(SOURCES src/lbg_tree.cpp
        src/pixel.cpp)

add_executable(l5 app/main.cpp ${SOURCES})

target_include_directories(l5 PRIVATE include)
