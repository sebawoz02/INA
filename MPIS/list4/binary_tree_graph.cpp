#include <iostream>
#include <random>
#include <fstream>
#include <algorithm>

using namespace std;

struct Node {
public:
    int limit;
    int visited = 0;
    int index;
    Node* leftNode = nullptr;
    Node* rightNode = nullptr;
    Node* parent = nullptr;

    Node(int n, int _limit){
        this->index = n;
        this->limit = _limit;
        if(2*n+1<this->limit){
            Node* node = new Node(2*n+1,this->limit);
            this->leftNode = node;
            node->parent = this;
        }
        if(2*n+2<this->limit){
            Node* node = new Node(2*n+2,this->limit);
            this->rightNode = node;
            node->parent = this;
        }
    }

};

int main() {
    ofstream myfile("E:/STUDIA/MPiS/ZADDOM4/binary_tree_graph.txt");

    int _k = 2000;

    random_device rd{};
    mt19937 mt{rd()};
    for(int n = 100; n<=2000; n+=50) {
        static uniform_int_distribution<int> dist;
        dist.param(uniform_int_distribution<int>::param_type(0, 2));

        long long n_coverage_time [_k] = {};

        for(int k = 0; k<_k; k++) {
            Node* root = new Node(0, n);
            Node* current_node = root;

            root->visited = 1;
            int no_visited = 1;
            int next_node;
            int time = 0;
            while (no_visited<n){

                // jezeli parent nie istnieje (lewy i prawy musza istniec przy n>=100)

                if(current_node->parent==nullptr){
                    dist.param(uniform_int_distribution<int>::param_type(0, 1));
                    next_node = dist(mt);   // 0 - left node, 1- right node
                    if(next_node==0)current_node = current_node->leftNode;
                    else current_node = current_node->rightNode;
                }

                //jezeli prawy oraz lewy node nie istnieja

                else if((current_node->leftNode==nullptr) && (current_node->rightNode==nullptr)) current_node = current_node->parent;


                // jezeli tylko prawy nie istnieje

                else if(current_node->rightNode== nullptr){
                    dist.param(uniform_int_distribution<int>::param_type(0, 1));
                    next_node = dist(mt);   // 0 - left node, 1- parent node
                    if(next_node==0)current_node = current_node->leftNode;
                    else current_node = current_node->parent;
                }

                //wszystkie istnieja

                else{
                    dist.param(uniform_int_distribution<int>::param_type(0, 2));
                    next_node = dist(mt);   // 0 - parent node, 1- left node, 2 - right node
                    if(next_node==0)current_node = current_node->parent;
                    else if(next_node==1)current_node = current_node->leftNode;
                    else current_node = current_node->rightNode;
                }

                if(current_node->visited==0){
                    no_visited++;
                    current_node->visited=1;
                }
                time++;

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
