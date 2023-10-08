class Vertex:
    def __init__(self, _id):
        self.id = _id
        self.neighbours = []

    def add_neighbour(self, u):
        self.neighbours.append(u)


class Graph:

    def __init__(self, v):
        self.V = v
        self.adjList = []

        # zmienne potrzebne do szukania SCC
        self.stack = []
        self.visited = []
        self.num_of_components = 0
        self.scc_found = 0

        for i in range(v):
            self.adjList.append(Vertex(i))
            self.visited.append(False)

    def add_edge(self, v, u):
        self.adjList[v].add_neighbour(u)

    def findSCC(self):
        # step 1: fill order
        for i in range(self.V):
            if not self.visited[i]:
                self.DFS(i)
        # step 2: get transpose graph
        self.getTranspose()
        for i in range(self.V):
            self.visited[i] = False
        # step 3: process vertices on stack
        while len(self.stack) != 0:
            v = self.stack.pop()
            if not self.visited[v]:
                self.num_of_components = 0
                self.DFSprint(v)
                print(" ")
                self.scc_found += 1
                print(f"# of Components: {self.num_of_components}")
        print(f"Total # of SCC: {self.scc_found}")

    def DFS(self, vertex):
        self.visited[vertex] = True
        for i in self.adjList[vertex].neighbours:
            if not self.visited[i]:
                self.DFS(i)
        self.stack.append(vertex)

    def getTranspose(self):
        newlist = []
        for i in range(self.V):
            newlist.append(Vertex(i))
        for i in range(self.V):
            for j in self.adjList[i].neighbours:
                newlist[j].add_neighbour(i)
        self.adjList = newlist

    def DFSprint(self, vertex):
        self.visited[vertex] = True
        self.num_of_components += 1
        if self.V <= 200:
            print(f"{vertex}, ", end=" ")
        for i in self.adjList[vertex].neighbours:
            if not self.visited[i]:
                self.DFSprint(i)


file = open("3/g3-4.txt")
iterator = 0
graph = None
for line in file.readlines():
    if iterator == 0 or iterator == 2:
        iterator += 1
    elif iterator == 1:
        iterator += 1
        v = int(line)
        graph = Graph(v)
    else:
        edge = line.split(" ")
        graph.add_edge(int(edge[0])-1, int(edge[1])-1)
file.close()

graph.findSCC()
