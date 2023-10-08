#include <iostream>
#include <random>
#include <fstream>
#include <algorithm>

using namespace std;

int main() {
    ofstream myfile("E:/STUDIA/MPiS/ZADDOM4/binary_tree_graph_end.txt");

    int _k = 500;

    random_device rd{};
    mt19937 mt{rd()};
    for(int n = 100; n<=2000; n+=50) {
        static uniform_int_distribution<int> dist;
        dist.param(uniform_int_distribution<int>::param_type(0, 1));

        long long n_coverage_time [_k] = {};

        for(int k = 0; k<_k; k++) {
            long long time = 0;
            int visited [n] = {};
            int current_node = 0;
            int no_visited = 1;
            visited[current_node] = 1;
            while (no_visited<n) {
                int next_node = dist(mt);
                if(current_node==0) current_node++;
                else {
                    if (next_node == 1) current_node++;
                    else current_node--;
                }
                if(visited[current_node]==0){
                    visited[current_node]=1;
                    no_visited ++;
                }
                time ++;
            }
            n_coverage_time[k] = time;
        }
        cout << n << endl;
        for(long long i : n_coverage_time){
            myfile << to_string(i) << ",";
        }
        myfile << endl;
    }

    return 0;
}
