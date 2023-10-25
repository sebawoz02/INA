from node import Node


class DynamicHuffmanTree:
    def __init__(self):
        self.nyt = Node(parent=None, weight=0, symbol='NYT')
        self.root = self.nyt
        self.visited = [None] * 256  # Num of possible different symbols
        self.tree = []  # Nodes

    def insert(self, symbol: str):
        curr_node = self.visited[ord(symbol)]
        if curr_node is None:
            new_node = Node(symbol=symbol, weight=1, parent=None)
            inter = Node(symbol='', weight=1, parent=self.nyt.parent, left=self.nyt, right=new_node)
            new_node.parent = inter
            self.nyt.parent = inter

            if inter.parent is not None:
                inter.parent.left = inter
            else:
                self.root = inter
            self.tree.append(inter)
            self.tree.append(new_node)

            self.visited[ord(symbol)] = new_node
            curr_node = inter.parent
        while curr_node is not None:
            largest_node = None
            for n in self.tree:
                if n.weight == curr_node.weight:
                    largest_node = n
                    break
            if ((curr_node is not largest_node) and (curr_node is not largest_node.parent) and
                    (largest_node is not curr_node.parent)):
                self.swap(curr_node, largest_node)
            curr_node.weight += 1
            curr_node = curr_node.parent

    def swap(self, n1: Node, n2: Node):
        n1_idx = self.tree.index(n1)
        n2_idx = self.tree.index(n2)
        self.tree[n1_idx], self.tree[n2_idx] = self.tree[n2_idx], self.tree[n1_idx]  # swap

        par = n1.parent
        n1.parent = n2.parent
        n2.parent = par

        if n1.parent.left is n2:
            n1.parent.left = n1
        else:
            n1.parent.right = n1
        if n2.parent.left is n1:
            n2.parent.left = n2
        else:
            n2.parent.right = n2
