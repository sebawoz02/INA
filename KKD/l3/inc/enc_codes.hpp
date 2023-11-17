#pragma once

#include <cstdint>
#include <fstream>

void flush_bits(uint8_t x, uint8_t& bit_buffer, size_t& curr_bit,
                FILE* out_file, size_t& out_size, uint32_t* out_occur);

void encode_omega(uint32_t num, uint8_t& bit_buffer, size_t& curr_bit,
                  FILE* out_file, size_t& out_size, uint32_t* out_occur);

void encode_delta(uint32_t num, uint8_t& bit_buffer, size_t& curr_bit,
                  FILE* out_file, size_t& out_size, uint32_t* out_occur);

void encode_gamma(uint32_t num, uint8_t& bit_buffer, size_t& curr_bit,
                  FILE* out_file, size_t& out_size, uint32_t* out_occur);

void encode_fib(uint32_t num, uint8_t& bit_buffer, size_t& curr_bit,
                FILE* out_file, size_t& out_size, uint32_t* out_occur);

void prepare_fib();
