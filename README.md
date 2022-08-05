# CS461 is the intro to AI course I took at CSUEB. We covered a number of algorithms and topics such as: BFS, DFS, A*, MINIMAX decision making and Knowledge bases
## Some of my assignments, ones I feel are most used, are in this repository

## 1. C++ BFS to find a solution(steps) to the Mission Cannibal problem
```
/*
  @param a = starting node (state)
  return child node if a solution(final state) is found
  return Null if no solution is found
*/
Node*BFS(Node*a){

    if(goal_test(a->state)){
        return a;
    }

    std::deque<Node*> frontier, explored;
    frontier.push_back(a);

    while(true){
        if (frontier.size() == 0){
            return NULL;
        }

        Node *p = frontier.front();
        explored.push_back(p);
        frontier.pop_front();

        for (std::string act : actions){
            Node* child = new Node();
            child->action = act;
            child->parent = p;
            child->boat = child->parent->boat == false ? true : false;
            child->state = do_action(child->parent->state, act, p->boat);

            // action was successful
            if(child->state != ""){
                
                if(!visited(child->state, child->boat, frontier) && !visited(child->state, child->boat, explored)){
       
                    if(goal_test(child->state)){
                        return child;
                    }
                    frontier.push_back(child);
                }
          
            }
            else delete child;
            
        }

    }

    // no solution was found
    return NULL;
}


```

## 2. LISP - The following is the core algorithm of an AI tic-tac-toe using Minimax Decision Making. It uses a 4x4 matrix to represent the game board
```
;;; returns best action calculated by the algorithm
(defun minimax (board)
    (setq actions (ACTIONS board))
    (setq best_action (car actions))
    (print "here 3")
    (loop for a in actions do
        ;;; replicate board
        (setq new_state (make-array '(4 4):displaced-to board))
        (RESULT new_state a 'o)
        (print "here 4")
        (setq a_score (Min_Value new_state a))
        (print "here 6")
        (if (> a_score best_score)(setq best_score a_score))
        (if (> a_score best_score)(setq best_action a))
    )

    (return-from minimax best_action)
)

```

## 3. C++ Genetic Algorithm to find the best path in making a loop in a 9 node graph
```
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



/*
    @param population = reference to init population
    return returner = (string representing route to take)
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
```
