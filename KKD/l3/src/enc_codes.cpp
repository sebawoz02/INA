//
// Created by sebawoz02 on 17.11.23.
//
#include <enc_codes.hpp>
#include <vector>

using namespace std;

static void write_bit(uint8_t bit, uint8_t& bit_buffer, size_t& curr_bit,
                      FILE* out_file, size_t& out_size, uint32_t* out_occur);
static size_t bit_length(uint32_t in);

static uint32_t fib[46];

static size_t bit_length(const uint32_t in)
{
  unsigned bits, var = in;
  for(bits = 0; var != 0; ++bits)
    var >>= 1;
  return bits;
}

static void write_bit(const uint8_t bit, uint8_t& bit_buffer, size_t& curr_bit,
                      FILE* out_file, size_t& out_size, uint32_t* out_occur)
{
  bit_buffer = bit_buffer << 1;
  if(bit) {
    bit_buffer |= 1;
  }

  curr_bit++;
  if(curr_bit == 8) {
    out_occur[bit_buffer]++;
    out_size++;
    fwrite(&bit_buffer, 1, 1, out_file);
    curr_bit = 0;
    bit_buffer = 0;
  }
}

void flush_bits(const uint8_t x, uint8_t& bit_buffer, size_t& curr_bit,
                FILE* out_file, size_t& out_size, uint32_t* out_occur)
{
  while(curr_bit)
    write_bit(x, bit_buffer, curr_bit, out_file, out_size, out_occur);
}

void encode_fib(uint32_t num, uint8_t& bit_buffer, size_t& curr_bit,
                FILE* out_file, size_t& out_size, uint32_t* out_occur)
{
  vector<bool> v;
  int32_t start = 0;
  for(int32_t i = 45; i >= 0; i--) {
    if(fib[i] <= num) {
      start = i;
      break;
    }
  }
  v.push_back(true);
  v.push_back(true);

  num -= fib[start];
  for(int32_t i = start - 1; i >= 0; i--) {
    if(fib[i] <= num && v[v.size() - 1] == 0) {
      num -= fib[i];
      v.push_back(true);
    } else {
      v.push_back(false);
    }
  }
  const auto size = static_cast<int32_t>(v.size());
  for(int32_t i = size - 1; i >= 0; i--) {
    if(v[i]) {
      write_bit(1, bit_buffer, curr_bit, out_file, out_size, out_occur);
    } else {
      write_bit(0, bit_buffer, curr_bit, out_file, out_size, out_occur);
    }
  }
}

void prepare_fib()
{
  fib[0] = 1;
  fib[1] = 1;
  for(size_t i = 2; i < 46; i++) {
    fib[i] = fib[i - 1] + fib[i - 2];
  }
}

void encode_gamma(uint32_t num, uint8_t& bit_buffer, size_t& curr_bit,
                  FILE* out_file, size_t& out_size, uint32_t* out_occur)
{
  size_t len = bit_length(num);
  int and_val = 1;
  for(size_t i = 0; i < len - 1; i++) {
    write_bit(0, bit_buffer, curr_bit, out_file, out_size, out_occur);
    and_val = and_val << 1;
  }

  for(size_t i = 0; i < len; i++) {
    if(and_val & num) {
      write_bit(1, bit_buffer, curr_bit, out_file, out_size, out_occur);
    } else {
      write_bit(0, bit_buffer, curr_bit, out_file, out_size, out_occur);
    }
    and_val = and_val >> 1;
  }
}

void encode_omega(uint32_t num, uint8_t& bit_buffer, size_t& curr_bit,
                  FILE* out_file, size_t& out_size, uint32_t* out_occur)
{
  vector<bool> v;
  v.push_back(false);
  uint32_t k = num;
  while(k > 1) {
    size_t len = bit_length(k);
    int c = 1;
    for(int i = 0; i < len; i++) {
      if(c & k) {
        v.push_back(true);
      } else {
        v.push_back(false);
      }
      c = c << 1;
    }
    k = len - 1;
  }
  for(int32_t i = static_cast<int32_t>(v.size()) - 1; i >= 0; i--) {
    if(v[i]) {
      write_bit(1, bit_buffer, curr_bit, out_file, out_size, out_occur);
    } else {
      write_bit(0, bit_buffer, curr_bit, out_file, out_size, out_occur);
    }
  }
}

void encode_delta(uint32_t num, uint8_t& bit_buffer, size_t& curr_bit,
                  FILE* out_file, size_t& out_size, uint32_t* out_occur)
{
  size_t len = bit_length(num);
  encode_gamma(len, bit_buffer, curr_bit, out_file, out_size, out_occur);
  uint32_t and_val = 1;
  for(size_t i = 0; i < len - 2; i++) {
    and_val = and_val << 1;
  }
  for(size_t i = 0; i < len - 1; i++) {
    if(and_val & num) {
      write_bit(1, bit_buffer, curr_bit, out_file, out_size, out_occur);
    } else {
      write_bit(0, bit_buffer, curr_bit, out_file, out_size, out_occur);
    }
    and_val = and_val >> 1;
  }
}
