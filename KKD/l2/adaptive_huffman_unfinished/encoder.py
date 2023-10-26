import math
from KKD.l2.adaptive_huffman_unfinished.dynamic_huffman_tree import DynamicHuffmanTree


def _node_code(node) -> str:
    code = ''
    while node.parent is not None:
        p = node.parent
        if p.left is node:
            code += '0'
        else:
            code += '1'
        node = p
    return code[::-1]   # reversed


class Encoder(DynamicHuffmanTree):
    def __init__(self):
        super().__init__()

    def encode(self, symbol):
        node = self.visited[ord(symbol)]
        if node is not None:
            code = _node_code(node)
        else:
            code = _node_code(self.nyt) + bin(ord(symbol))[2:].zfill(8)
        self.insert(symbol)
        return code

    def print_stats(self, input_file, output_file):
        with (open(input_file, "rb") as inf):
            bytes_file = inf.read()
            no_bytes = len(bytes_file)
            symbols_freq = {}
            for b in bytes_file:
                if b not in symbols_freq.keys():
                    symbols_freq[b] = 1
                else:
                    symbols_freq[b] += 1
            entropy = 0
            for symbol in symbols_freq.keys():
                probability = symbols_freq[symbol] / no_bytes
                entropy -= probability * math.log2(probability)
        with open(output_file, "rb") as outf:
            encoded = outf.read()
            compression_lvl = len(bytes_file) / (len(encoded))
            avg_length = len(encoded) * 8 / len(bytes_file)

        print("Stopień kompresji: ", compression_lvl)
        print("Entropia: ", entropy)
        print("Średnia długość: ", avg_length)
