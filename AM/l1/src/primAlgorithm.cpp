#include <primAlgorithm.h>
#include <bits/stdc++.h>

static size_t minKeyId(const uint32_t minKey[], const bool visited[], size_t no_nodes);

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

std::vector<TreeNode*> primFindMST(const Graph* const graph) {
    // V
    const size_t no_nodes = graph->no_nodes;
    // Minimum spanning tree
    std::vector<TreeNode*> mst;
    mst.reserve(no_nodes);
    // Nodes already in MST
    bool visited[no_nodes];
    // Used to pick min value for all Nodes in MST
    uint32_t minKeys[no_nodes];
    for(size_t i = 0; i < no_nodes; i++) {
        minKeys[i] = UINT32_MAX;
        visited[i] = false;
        mst.push_back(nullptr);
    }
    minKeys[0] = 0;
    mst[0] = new TreeNode(0, graph->nodes_list[0]->x, graph->nodes_list[0]->y, nullptr);

    for(size_t i = 0; i < no_nodes - 1; i++)
    {
        const size_t id = minKeyId(minKeys, visited, no_nodes);

        visited[id] = true;
        for(size_t v = 0; v < no_nodes; v++)
        {
            if(graph->dist_matrix[id][v] != 0 && !visited[v] && graph->dist_matrix[id][v] < minKeys[v])
            {
                if(mst[v] != nullptr) {
                    if(mst[v]->parent != nullptr)
                        mst[v]->parent->removeEdge(mst[v]);
                    delete mst[v];
                }
                mst[v] = new TreeNode(v ,graph->nodes_list[v]->x, graph->nodes_list[v]->y, mst[id]);
                mst[v]->parent->addEdge(mst[v]);

                minKeys[v] = graph->dist_matrix[id][v];
            }
        }
    }

    return mst;
}
