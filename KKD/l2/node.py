class Node:
    def __init__(self, parent, symbol: str, weight: int = 0, left=None, right=None):
        self.parent = parent
        self.symbol = symbol
        self.weight = weight
        self.left = left
        self.right = right
