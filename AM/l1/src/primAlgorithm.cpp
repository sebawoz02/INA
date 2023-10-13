#include <primAlgorithm.h>
#include <bits/stdc++.h>

static size_t minKeyId(const uint32_t minKey[], const bool visited[], size_t no_nodes);
static std::vector<Node*> parseToVector(const size_t mst[], const Graph* graph, const bool visited[]);

// Returns the id of the Node to which the shortest path leads
static size_t minKeyId(const uint32_t minKeys[], const bool visited[], size_t no_nodes)
{
    // Initialize min value
    uint32_t min = UINT32_MAX;
    size_t min_index;

    for (size_t v = 0; v < no_nodes; v++)
        if (!visited[v] && minKeys[v] < min)
            min = minKeys[v], min_index = v;

    return min_index;
}

static std::vector<Node*> parseToVector(const size_t mst[], const Graph* const graph, const bool visited[])
{
    const size_t no_nodes = graph->no_nodes;
    std::vector<Node*> list_mst;
    list_mst.reserve(no_nodes);
    for(size_t i = 1; i < no_nodes; i++)
    {
        size_t id = mst[i];
        list_mst.push_back(graph->nodes_list[id]);
    }
    // Find root
    for(size_t i = 0; i < no_nodes; i++)
    {
        if(!visited[i])
        {
            list_mst.push_back(graph->nodes_list[i]);
            break;
        }
    }
    return list_mst;
}

std::vector<Node*> primFindMST(const Graph* const graph) {
    // V
    const size_t no_nodes = graph->no_nodes;
    // Minimum spanning tree
    size_t mst[no_nodes];
    // Nodes already in MST
    bool visited[no_nodes];
    // Used to pick min value for all Nodes in MST
    uint32_t minKeys[no_nodes];
    for(size_t i = 0; i < no_nodes; i++) {
        minKeys[i] = UINT32_MAX;
        visited[i] = false;
    }
    minKeys[0] = 0;

    for(size_t i = 0; i < no_nodes - 1; i++)
    {
        const size_t id = minKeyId(minKeys, visited, no_nodes);

        visited[id] = true;
        for(size_t v = 0; v < no_nodes; v++)
        {
            if(graph->dist_matrix[id][v] != 0 && !visited[v] && graph->dist_matrix[id][v] < minKeys[v])
            {
                mst[v] = id;
                minKeys[v] = graph->dist_matrix[id][v];
            }
        }
    }

    return parseToVector(mst, graph, visited);
}
