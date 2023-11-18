#include <analyze.hpp>
#include <cstring>
#include <enc_codes.hpp>
#include <iostream>
#include <map>

using namespace std;

void encode(FILE* in_file, FILE* out_file,
            void (*encode_number)(uint32_t, uint8_t&, size_t&, FILE*, size_t&,
                                  uint32_t*),
            const uint8_t flush_bit)
{
  map<string, size_t> dict;
  size_t dict_len = 256;
  uint32_t out_occur[256];
  for(size_t i = 0; i <= 255; i++) {
    string tmp(1, (char)(i));
    dict[tmp] = i;
    out_occur[i] = 0;
  }

  size_t in_size = 0;
  size_t out_size = 0;

  uint8_t bit_buffer;
  size_t curr_bit = 0;
  char curr_char;

  bool eof = false;
  if(fread(&curr_char, 1, 1, in_file) != 1) {
    cerr << "File is empty." << endl;
    return;
  }
  // Main loop
  while(!eof) {
    string curr_str(1, curr_char);
    // While string is already in dict
    while(!eof && dict.find(curr_str) != dict.end()) {
      if(fread(&curr_char, 1, 1, in_file) == 1) {
        curr_str.push_back(curr_char);
        in_size++;
      } else {
        eof = true;
      }
    }
    // New string
    dict[curr_str] = dict_len;
    dict_len++;
    curr_char = curr_str[curr_str.size() - 1];
    // Encode prev string
    curr_str.pop_back();
    if(!curr_str.empty()) {
      encode_number(dict[curr_str] + 1, bit_buffer, curr_bit, out_file,
                    out_size, out_occur);
    }
  }

  // Flush buffer
  encode_number(int(curr_char) + 1, bit_buffer, curr_bit, out_file, out_size,
                out_occur);
  flush_bits(flush_bit, bit_buffer, curr_bit, out_file, out_size, out_occur);

  // Entropy and conversion rate
  cout << "Encoded size: " << out_size << endl;
  cout << "Pre-encode size: " << in_size << endl;
  cout << "Conversion rate: " << calculate_compression_rate(in_size, out_size)
       << endl;
  cout << "Encoded entropy: " << calculate_entropy(out_occur, out_size) << endl;
}

int main(int argc, char** argv)
{
  string in_filename = argv[1];
  string out_filename = argv[2];

  void (*encode_number)(uint32_t, uint8_t&, size_t&, FILE*, size_t&,
                        uint32_t*) = &encode_omega;
  uint8_t flush_bit = 1;
  if(argc > 3) {
    if(strcmp(argv[3], "-d") == 0) {
      encode_number = &encode_delta;
      flush_bit = 0;
    } else if(strcmp(argv[3], "-g") == 0) {
      encode_number = &encode_gamma;
      flush_bit = 0;
    } else if(strcmp(argv[3], "-f") == 0) {
      encode_number = &encode_fib;
      flush_bit = 0;
      prepare_fib();
    }
  }
  auto in_file = fopen(in_filename.c_str(), "rb");
  auto out_file = fopen(out_filename.c_str(), "wb");
  if(!out_file || !in_file) {
    cerr << "Wrong filename" << endl;
    cerr << "USAGE:\n"
            "./encode <input_file> <output_file>\n"
            "Optional params:"
            "-o (omega encoding - default)\n"
            "-d (delta encoding)\n"
            "-g (gamma encoding)\n"
            "-f (fibonacci encoding)\n"
         << endl;
    return 1;
  }

  encode(in_file, out_file, encode_number, flush_bit);

  fclose(in_file);
  fclose(out_file);
  in_file = fopen(in_filename.c_str(), "rb");
  auto* in_occur = static_cast<uint32_t*>(calloc(256, sizeof(uint32_t)));
  size_t in_size = 0;
  int16_t c;
  while((c = (int16_t)fgetc(in_file)) != EOF) {
    in_size++;
    in_occur[c]++;
  }
  cout << "Pre-encode entropy: " << calculate_entropy(in_occur, in_size)
       << endl;
  free(in_occur);
  fclose(in_file);
  return 0;
}