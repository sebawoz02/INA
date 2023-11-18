//
// Created by sebawoz02 on 18.11.23.
//
#include <dec_codes.hpp>
#include <vector>

static int64_t read_bits(uint8_t& bit_buffer, std::size_t& curr_bit,
                         FILE* in_file);
static int64_t trim_significant_bits(uint8_t& bit_buffer, size_t& curr_bit,
                                     size_t n);

static uint32_t fib[46];

static int64_t trim_significant_bits(uint8_t& bit_buffer, size_t& curr_bit,
                                     size_t n)
{
  uint8_t most_sign_bit = 1 << (7 - curr_bit);
  int64_t solution = 0;
  while(n > 0) {
    n--;
    solution = solution << 1;
    if(bit_buffer & most_sign_bit) {
      solution += 1;
      bit_buffer -= most_sign_bit;
    }
    most_sign_bit = most_sign_bit >> 1;
  }
  return solution;
}

static int64_t read_bits(uint8_t& bit_buffer, std::size_t& curr_bit,
                         FILE* in_file)
{
  size_t count = 0;
  int64_t solution = 0;
  if(curr_bit != 0) {
    if(8 - curr_bit <= 1) {
      solution = bit_buffer;
      count += 8 - curr_bit;
      curr_bit = 0;
    } else {
      solution = trim_significant_bits(bit_buffer, curr_bit, 1);
      count += 1;
      curr_bit += 1;
    }
  }
  while(count < 1) {
    int tmp = fgetc(in_file);
    if(tmp == EOF) {
      return -1;
    }
    bit_buffer = static_cast<int8_t>(tmp);
    curr_bit = 0;
    if(1 - count >= 8) {
      solution = solution << 8;
      solution |= bit_buffer;
      count += 8;
    } else {
      solution = solution << (1 - count);
      solution |= trim_significant_bits(bit_buffer, curr_bit, 1 - count);
      curr_bit += 1 - count;
      count += 1 - count;
    }
  }
  return solution;
}

int64_t decode_gamma(uint8_t& bit_buffer, std::size_t& curr_bit, FILE* in_file)
{
  size_t n = 0;
  int64_t solution = 1;
  int64_t bit = read_bits(bit_buffer, curr_bit, in_file);
  while(bit == 0) {
    n++;
    bit = read_bits(bit_buffer, curr_bit, in_file);
  }
  if(bit == -1) {
    return -1;
  }
  for(size_t i = 0; i < n; i++) {
    solution = solution << 1;
    solution += read_bits(bit_buffer, curr_bit, in_file);
  }
  return solution;
}

int64_t decode_omega(uint8_t& bit_buffer, std::size_t& curr_bit, FILE* in_file)
{
  int64_t n = 1;
  int64_t old_n = 1;
  int64_t bit = read_bits(bit_buffer, curr_bit, in_file);
  if(bit == -1) {
    return -1;
  }
  while(bit != 0) {
    n = bit;
    for(size_t i = 0; i < old_n; i++) {
      n = n << 1;
      bit = read_bits(bit_buffer, curr_bit, in_file);
      if(bit == -1) {
        return -1;
      }
      n += bit;
    }
    bit = read_bits(bit_buffer, curr_bit, in_file);
    if(bit == -1) {
      return -1;
    }
    old_n = n;
  }
  return n;
}

int64_t decode_delta(uint8_t& bit_buffer, std::size_t& curr_bit, FILE* in_file)
{
  int64_t n = decode_gamma(bit_buffer, curr_bit, in_file);
  if(n == -1) {
    return -1;
  }
  int64_t solution = 1;
  for(int i = 0; i < n - 1; i++) {
    solution = solution << 1;
    int64_t bit = read_bits(bit_buffer, curr_bit, in_file);
    if(bit == -1) {
      return -1;
    }
    solution += bit;
  }
  return solution;
}

int64_t decode_fib(uint8_t& bit_buffer, std::size_t& curr_bit, FILE* in_file)
{
  std::vector<bool> v;
  v.push_back(read_bits(bit_buffer, curr_bit, in_file));
  v.push_back(read_bits(bit_buffer, curr_bit, in_file));
  while(v[v.size() - 1] == 0 || v[v.size() - 2] == 0) {
    int64_t bit = read_bits(bit_buffer, curr_bit, in_file);
    v.push_back(bool(bit));
    if(bit == -1) {
      return -1;
    }
  }
  int64_t solution = 0;
  for(int i = 0; i < v.size() - 1; i++) {
    if(v[i]) {
      solution += static_cast<int64_t>(fib[i]);
    }
  }
  return solution;
}

void prepare_fib()
{
  fib[0] = 1;
  fib[1] = 1;
  for(size_t i = 2; i < 46; i++) {
    fib[i] = fib[i - 1] + fib[i - 2];
  }
}
