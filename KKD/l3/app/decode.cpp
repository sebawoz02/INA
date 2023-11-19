#include <cstring>
#include <dec_codes.hpp>
#include <iostream>
#include <unordered_map>

using namespace std;

int decode(FILE* in_file, FILE* out_file,
           int64_t (*decode_number)(uint8_t&, std::size_t&, FILE*))
{
  unordered_map<size_t, string> dict;
  uint8_t bit_buffer = 0;
  size_t current_bit = 0;

  for(size_t i = 0; i <= 255; i++) {
    string tmp(1, char(i));
    dict[i] = tmp;
  }
  size_t dict_len = 256;

  int64_t curr_num;
  string prev_str;
  curr_num = decode_number(bit_buffer, current_bit, in_file) - 1;
  while(curr_num != -2) {
    if(dict.find(curr_num) == dict.end()) {
      dict[dict_len++] = prev_str + prev_str[0];
      const char* cstr = dict[curr_num].c_str();
      fwrite(cstr, sizeof(unsigned char), dict[curr_num].size(), out_file);
    } else {
      const char* cstr = dict[curr_num].c_str();
      fwrite(cstr, sizeof(unsigned char), dict[curr_num].size(), out_file);
      if(!prev_str.empty()) {
        dict[dict_len++] = prev_str + dict[curr_num][0];
      }
    }
    prev_str = dict[curr_num];
    curr_num = decode_number(bit_buffer, current_bit, in_file) - 1;
  }
  return 0;
}

int main(int argc, char** argv)
{
  string in_filename = argv[1];
  string out_filename = argv[2];

  int64_t (*decode_number)(uint8_t&, std::size_t&, FILE*) = &decode_omega;
  if(argc > 3) {
    if(strcmp(argv[3], "-d") == 0) {
      decode_number = &decode_delta;
    } else if(strcmp(argv[3], "-g") == 0) {
      decode_number = &decode_gamma;
    } else if(strcmp(argv[3], "-f") == 0) {
      decode_number = &decode_fib;
      prepare_fib();
    }
  }
  auto in_file = fopen(in_filename.c_str(), "rb");
  auto out_file = fopen(out_filename.c_str(), "wb");
  if(!out_file || !in_file) {
    cerr << "Wrong filename" << endl;
    cerr << "USAGE:\n"
            "./decode <input_file> <output_file>\n"
            "Optional params:"
            "-o (omega decoding - default)\n"
            "-d (delta decoding)\n"
            "-g (gamma decoding)\n"
            "-f (fibonacci decoding)\n"
         << endl;
    return 1;
  }

  decode(in_file, out_file, decode_number);

  fclose(in_file);
  fclose(out_file);
  return 0;
}
