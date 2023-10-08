#include <iostream>
#include <random>
#include <fstream>

using namespace std;


int main() {
    ofstream myfile("E:/STUDIA/MPiS/ZADDOM4/lollipop_graph.txt");

    int _k = 50;

    random_device rd{};
    mt19937 mt{rd()};
    for(int n = 100; n<=1000; n+=50) {
        static uniform_int_distribution<int> dist;
        dist.param(uniform_int_distribution<int>::param_type(0, n-1));

        long long n_coverage_time [_k] = {};

        int clique_size = floor(2*n/3);

        int path_size = n - clique_size;

        for(int k = 0; k<_k; k++) {

            int lollipop [n] = {};

            // wybieram wierzcholek niepolaczony ze sciazka ( 0 )

            int no_visited = 1;
            lollipop[0] = 1;
            int current_node = 0;
            int time = 0;
            while(no_visited<n){
                if(current_node < clique_size-1){    // bładzenie po klice
                    dist.param(uniform_int_distribution<int>::param_type(0, clique_size-1));
                    int next_node = dist(mt);
                    while(next_node == current_node) next_node = dist(mt);
                    current_node = next_node;
                    if(lollipop[current_node]==0){
                        lollipop[current_node]=1;
                        no_visited++;
                    }
                }
                else if(current_node==clique_size-1) {    // obecnie stoi na łączeniu
                    dist.param(uniform_int_distribution<int>::param_type(0, clique_size));
                    current_node = dist(mt);
                    while(current_node==clique_size-1) current_node = dist(mt);
                    if(current_node == clique_size){    // wejscie na scieżke
                        if(lollipop[clique_size]==0){
                            lollipop[clique_size] = 1;
                            no_visited++;
                        }
                    }
                    else{
                        if(lollipop[current_node]==0){
                            lollipop[current_node]=1;
                            no_visited++;
                        }
                    }
                }
                else{   // błądzenie po ścieżce
                    if(current_node==n-1){ //koniec ścieżki
                        current_node--;
                    }
                    else {
                        dist.param(uniform_int_distribution<int>::param_type(0, 1));
                        int direction = dist(mt);
                        if (direction == 1) current_node--;
                        else current_node++;
                        if (lollipop[current_node] == 0) {
                            lollipop[current_node] = 1;
                            no_visited++;
                        }
                    }
                }
                time++;
            }
            cout << k << endl;
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
