#include <bits/stdc++.h>
typedef long long int ll;
/*  
    NOTES to self 
    best paths so far:
     1 3 7 9 4 2 5 8 6 1 , cost 72 gen 243 pop 10000
     1 2 5 7 8 3 4 9 6 1 , cost 63 gen 121 pop 20000
     3 8 5 7 2 6 9 4 1 3 , cost 50 gen 0 pop 100000
     4 5 8 6 7 3 9 2 1 4 , cost 73 gen 13 pop 200000
     1 7 5 9 8 4 6 2 3 1 , cost 85 generation 288 pop 10000
     3 9 5 2 1 8 7 6 4 3 , cost 83 gen 15 pop 200000
     7 3 8 6 5 2 1 4 9 7 , cost 50 gen 0 pop 362880
     1 2 3 4 5 8 6 7 9 1 , cost 79 gen 29692 pop 100
     3 4 5 6 9 8 2 1 7 3 , cost 77 gen 30 pop 100000
     3 2 6 8 1 5 7 4 9 3 , cost 80 gen 30 pop 100000
    
    Worst path so far:
     4 8 7 2 5 3 6 9 1 4 , cost 101 gen 30 pop 100000


*/
// ***** Schema : Used a 2d array to represent a GRAPH 
// adjacency matrix of graph
int matrix[][10] = {
    {0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
    {0, -1, 2, 11, 3, 18, 14, 20, 12, 5}, 
    {0, 2, -1, 13, 10, 5, 3, 8, 20, 17}, 
    {0, 11, 13, -1, 5, 19, 21, 2, 5, 8}, 
    {0, 3, 10, 5, -1, 6, 4, 12, 15, 1}, 
    {0, 18, 5, 19, 6, -1, 12, 6, 9, 7}, 
    {0, 14, 3, 21, 4, 12, -1, 19, 7, 4}, 
    {0, 20, 8, 2, 12, 6, 19, -1, 21, 13}, 
    {0, 12, 20, 5, 15, 9, 7, 21, -1, 6}, 
    {0, 5, 17, 8, 1, 7, 4, 13, 6, -1}
};

// GA functions
std::string GA(std::vector<std::pair<std::string, int>> &population);
std::string reproduce(std::string, std::string); // crossover function
void mutate(std::string &);
ll fitness(std::string);

// helper functions
std::vector<std::pair<std::string, int>> generatePopulation(std::string, int);
ll population_cost(std::vector<std::pair<std::string, int>> &);
int path_cost(std::string);

int main(){
    std::string init = "123456789";
    int population_size = 50000;
    std::vector<std::pair<std::string, int>> init_population = generatePopulation(init, population_size);

    std::cout << "\n Initial Population: (first 100 individuals) \n";
    int z = 0;
    for(auto itr:init_population){
        if(z % 10 == 0) std::cout << "\n";
        else if(z > 100) break;
        std::cout << itr.first << "\t";
        z++;
    }

    std::cout << "\n";

    // test GA incomplete
    std::string route = GA(init_population);
    std::cout << " Final Route Found: \n";
    for(int i=1; i<route.length(); i++){
        std::cout << route[i-1] << " to " << route[i] << " : " << matrix[route[i-1] - '0'][route[i] - '0'] << std::endl;
    }
    std::cout << route[route.length()-1] << " to " << route[0] << " : " << matrix[route[route.length()-1] - '0'][route[0] - '0'] << std::endl;
    std::cout << " total : " << path_cost(route) << std::endl;

    return 0;
}

/*
    @param population = reference to init population
    returns returner(string representing route to take)
*/
std::string GA(std::vector<std::pair<std::string, int>> &population){
    ll total_cost = population_cost(population);
    ll mean_cost = total_cost / population.size(); // helps determine fitness threshold
    ll generation = 0; // helps identify generation period
    std::cout << "\n init population cost = " << total_cost << "\n" << std::endl;

    // assist in keeping track of time elapsed
    auto start = std::chrono::high_resolution_clock::now();
    auto duration = std::chrono::duration_cast<std::chrono::microseconds>(start - start);
    srand(time(NULL));

    do{
        std::vector<std::pair<std::string, int>> new_population;
        // sort population in order of fitness 
        std::sort(population.begin(), population.end(), 
        [](std::pair<std::string, int> &x, std::pair<std::string, int> &y){
            return x.second < y.second;
        });

        for(int i=0; i<population.size(); i++){

            // assign random parents such that their fitness is top 60%
            int cutoff = 0.60 * population.size();
            int x = rand() % cutoff, y = rand() % cutoff;
            std::string strx = population.at(x).first, stry = population.at(y).first, child;

            // produce child
            child = reproduce(strx, stry);

            // check if child needs mutation
            if(fitness(child) > mean_cost)
                mutate(child);

            // if child is 30% better than average( fit enough ) end loop
            else if(1.0*fitness(child)/mean_cost <= 0.70){
                std::cout << " Best path on generation: " << generation << std::endl;
                return child;
            }

            // add child to our new population
            new_population.push_back(std::pair<std::string, int>(child, fitness(child)));
        }

        // re-assign population to next generation
        population = new_population;
        total_cost = population_cost(population);

        // check how much time has elapsed
        auto end = std::chrono::high_resolution_clock::now();
        duration = std::chrono::duration_cast<std::chrono::microseconds>(end - start);
        generation++;
    }while(duration.count() < 5000000);
    // if more than 6 seconds pass end run
    
    // top individual is one with lowest path cost
    std::string returner = population.at(0).first;
    std::cout << " Best path on generation: " << generation << std::endl;
    return returner;
}

/*
    @param state = current individual
    returns a mutated form of state by randomly swapping 2 indices 
        via reference
*/
void mutate(std::string &state){
    int x = rand() % state.length();
    int y = rand() % state.length();
    char t = state[x];
    state[x] = state[y];
    state[y] = t;
}

/*
    @param individual = an individual passed from a population,
    returnes path cost of individual
*/
ll fitness(std::string individual){
    return path_cost(individual);
}

/*
    @param x = first parent string
    @param y = second parent string
    returns returner (a substring made of both x and y)
*/
std::string reproduce(std::string x, std::string y){
    srand(time(NULL));
    int n = x.length();
    // randomly assign c and start idx
    int c = rand() % x.length(), start = rand() % (n-c);
    int substr_val[x.length()+1]; // assist in merging parents 
    std::string substr = x.substr(start, c);

    for(int i=0; i<x.length()+1; i++) substr_val[i] = 0;
    for(char c:substr) substr_val[c - '0'] = 1;

    std::string returner = "";
    int idx = 0, i=0;
    for(i, idx; idx<start; i++){
        // add to child if curr elem is not part of random subset
        if(substr_val[y[i]-'0'] == 0){
            idx++;
            returner += y[i];
        }
    }

    returner += substr; // add substring to returner
    
    // add rest of parent y
    for(i; i<n; i++)
        if(substr_val[y[i] - '0'] == 0) returner += y[i];

    return returner;
}

/*
    @param init = initial state from which to generate permutations
    @param n = number of permutations to generate
    returns returner(vector of permutations of size n)
*/
std::vector<std::pair<std::string, int>> generatePopulation(std::string init, int n){
    std::vector<std::pair<std::string, int>> returner;
    std::string new_perm;
    int i=0;
    while(std::next_permutation(init.begin(), init.end()) && i<n){
        returner.push_back(std::pair<std::string, int>(init, path_cost(init)));
        i++;
    }

    return returner;
}

/*
    @param population = reference to current population
    returns returner (cost of all individuals)
    makes use of path_cost function to calculate cost of every individual
*/
ll population_cost(std::vector<std::pair<std::string, int>> &population){
    ll total_pop_cost = 0;

    for(auto itr = population.begin(); itr!=population.end(); itr++)
        total_pop_cost += itr->second;
    
    return total_pop_cost;
}

/* 
    @param individual = an instance of an individual from population
    returns (cost of current path)
*/
int path_cost(std::string individual){
    int cost = 0, from, to;

    for(int i=1; i<individual.length(); i++){
        from = individual[i-1] - '0';
        to = individual[i] - '0';
        cost += matrix[from][to];
    }

    // add cost to return to starting node
    int return_to = matrix[individual[individual.length()-1] - '0'][individual[0] - '0'] ;

    return cost + return_to;
}