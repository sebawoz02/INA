cmake_minimum_required(VERSION 3.22.1)
project(l3)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_COMPILER "g++")
set(CMAKE_CXX_FLAGS "-Wall -Wextra -Wno-unsafe-buffer-usage -Wno-char-subscripts -Wno-sign-compare")

add_executable(encode "${CMAKE_CURRENT_SOURCE_DIR}/app/encode.cpp")
add_executable(decode "${CMAKE_CURRENT_SOURCE_DIR}/app/decode.cpp")

target_include_directories(encode PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/inc")
target_include_directories(decode PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/inc")

target_sources(encode PRIVATE
    "${CMAKE_CURRENT_SOURCE_DIR}/src/enc_codes.cpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/src/analyze.cpp"
)

target_sources(decode PRIVATE
    "${CMAKE_CURRENT_SOURCE_DIR}/src/dec_codes.cpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/src/analyze.cpp"
)
