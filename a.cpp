#include <bits/stdc++.h>
std::vector<int> visited(1000001, 0);
// graph[i] = the list of nodes that the i_th node can connect to
std::vector<std::vector<int>> graph;


void dfs(int current_node, std::vector<int> &neighbors, int &k);
int main(){

    int num_nodes_visited = 0;
    int sub_graphs = 0;
    
    int n;
    std::cin >> n;

    for(int i=1; i<=n; i++){
        int k = 0;

        dfs(i, graph.at(i), k);
        
        if(k > 0) sub_graphs++;

        num_nodes_visited += k;

        if(num_nodes_visited == n) break;
    }

    std::cout << sub_graphs - 1 << std::endl;
    return 0;
}

void dfs(int current_node, std::vector<int> &neighbors, int &k){

    if(visited[current_node] == 1) return;
    visited[current_node] = 1;
    k = k + 1;

    for(auto neighbor: neighbors){
        dfs(neighbor, graph.at(neighbor), k);
    }

    return;
}
