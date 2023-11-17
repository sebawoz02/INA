//
// Created by sebawoz02 on 18.11.23.
//
#pragma once

#include <cstdint>
#include <fstream>

int64_t decode_gamma(uint8_t& bit_buffer, std::size_t& curr_bit, FILE* in_file);

int64_t decode_omega(uint8_t& bit_buffer, std::size_t& curr_bit, FILE* in_file);

int64_t decode_delta(uint8_t& bit_buffer, std::size_t& curr_bit, FILE* in_file);

int64_t decode_fib(uint8_t& bit_buffer, std::size_t& curr_bit, FILE* in_file);

void prepare_fib();