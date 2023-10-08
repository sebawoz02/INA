#include <iostream>
#include <random>
#include <fstream>
#include <algorithm>

using namespace std;

int main() {
    ofstream myfile("E:/STUDIA/MPiS/ZADDOM4/complete_graph.txt");

    random_device rd{};
    mt19937 mt{rd()};
    for(int n = 100; n<=2000; n+=50) {
        static uniform_int_distribution<int> dist;
        dist.param(uniform_int_distribution<int>::param_type(1, n));

        long long n_coverage_time [5000] = {};

        for(int k = 0; k<5000; k++) {
            long long time = 0;
            int visited [n] = {};
            int current_node = 0;
            int no_visited = 1;
            visited[0] = 1;
            while (no_visited<n) {
                int next_node = current_node;
                while (next_node==current_node){
                    next_node = dist(mt);
                }
                if(visited[next_node]==0){
                    visited[next_node]=1;
                    no_visited ++;
                }
                time ++;
                current_node = next_node;

            }
            n_coverage_time[k] = time;
        }
        cout << n << endl;
        for(int i=0; i<n;i++){
            myfile << to_string(n_coverage_time[i]) << ",";
        }
        myfile << endl;
    }

    return 0;
}
