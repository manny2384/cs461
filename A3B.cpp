// ****** GRAPH Breadth First Search ****** //
#include <iostream>
#include <vector>
#include <queue>
#include <deque>
#include <sstream>
#include <iomanip>

// global variables
std::string actions[] = {"mm", "cc", "cm", "m", "c"};
struct Node{
    // points to parent node
    Node *parent;
    // action that led parent node to current node
    std::string action;
    // boat goes to left(false) or right(true)
    // to reach child state
    bool boat;
    // stores a string of current state
    std::string state;
};

// solving function
Node*BFS(Node*state);
// helper function declarations
std::string do_action(std::string, std::string, bool);
int valid_action(int, int, int, int);
bool goal_test(std::string);
bool visited(std::string, bool, std::deque<Node*>&);

int main(){

    // initial state , m - missionary, c - cannibal, r - river
    std::string init_state = "rmmmccc";
    // used for reversing order of solution
    std::deque<Node*> path;

    // instantiate initial node 
    Node * init = new Node;
    init->action = "none";
    init->state = init_state;
    init->parent = NULL;
    init->boat = false;

    Node *solution = BFS(init); // find solution
    if(solution == NULL){
        std::cout << " NO SOLUTION \n";
        return 0;
    }
    
    // reverse path of solution
    while(solution != NULL){
        path.push_front(solution);
        solution = solution->parent;
    }

    // show path * note : left -> right or right -> left means action taken from one side of river - 'r', to other side
    for(std::deque<Node *>::iterator it=path.begin(); it!=path.end(); ++it){
        if((*it)->action == "none"){
            std::cout << " **** initial state : " + (*it)->state + "\n";
        }
        else{
            // if curr state boat goes left, parent had to go right
            if((*it)->boat == false){
                std::cout << "left  -> right : ";
            }
            // if curr state boat goes right, parent had to go left
            else if((*it)->boat == true){
                std::cout << "right ->  left : ";
            }
            std::cout << "parent action = " << (*it)->action 
                << ", from " << (*it)->parent->state << " to new state = " << (*it)->state << std::endl;
        }
    }

    return 0;
}

/*
    @params = node to initial state
    returns node pointing to solution
        or NULL if no solution found
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

/*
    @params = current state 
    returns true if goal state( aka 'r' = last elem)
        false if not goal state
*/
bool goal_test(std::string state){
    if(state[state.length() - 1] == 'r')
        return true;
    return false;
}

/*
    @params = curr state, action to attempt, direction of boat
    returns : 
        a string corresponding to action taken
        empty string if action is invalid
*/
std::string do_action(std::string state, std::string action, bool boat){
    std::stringstream ss(state);
    std::string returner, s, left = "", right = "";
    int left_c = 0, left_m = 0, right_c = 0, right_m = 0;
    int action_m = 0, action_c = 0;
    int tright_m = 0, tright_c = 0, tleft_m = 0, tleft_c = 0;
    while(getline(ss, s, 'r')){
        if(left == "" && state[0] != 'r') left = s;
        else right = s;
    }

    for(char c:left){
        if(c == 'm') left_m++;
        else left_c++;
    }
    for(char c:right){
        if(c == 'm') right_m++;
        else right_c++;
    }
    for(char c:action){
        if (c == 'm') action_m++;
        else action_c++;
    }

    // move right
    if(boat){
        if(!valid_action(left_c, left_m, action_c, action_m)){
            return "";
        }
        else if((left_m - action_m < left_c - action_c && left_m - action_m != 0) || (right_c + action_c > right_m + action_m && right_m + action_m != 0)){
            return "";
        }
        else{
            tright_m = right_m + action_m;
            tleft_m = left_m - action_m;
            tright_c = right_c + action_c;
            tleft_c = left_c - action_c;
        }
    }
    // move left
    else{
        if(!valid_action(right_c, right_m, action_c, action_m)){
            return "";
        }
        else if((right_m - action_m < right_c - action_c && right_m - action_m != 0) || (left_c + action_c > left_m + action_m & left_m + action_m != 0)){
            return "";
        }
        else{
            tright_m = right_m - action_m;
            tleft_m = left_m + action_m;
            tright_c = right_c - action_c;
            tleft_c = left_c + action_c;
        }
    }

    for(int i=0; i<tleft_c; i++) returner += 'c';
    for(int i=0; i<tleft_m; i++) returner += 'm';
    returner += 'r';
    for(int i=0; i<tright_c; i++) returner += 'c';
    for(int i=0; i<tright_m; i++) returner += 'm';
    
    return returner;
}

/*
    @params = number of c/m for both action and side
    returns true if num of c and m are available
        else false
*/
int valid_action(int c, int m, int ac, int am){
    if(ac <= c && am <= m) return true;
    return false;
}

/*
    @params = curr state, direction of boat, list to examin
    returns true if curr state is found in list
*/
bool visited(std::string curr_state, bool boat, std::deque<Node*>& list){
    // setup
    std::stringstream state(curr_state);
    std::string ss1, ss2;

    getline(state, ss1, 'r');


    // excercise
    for(std::deque<Node*>::iterator t=list.begin(); t != list.end(); ++t){
        std::stringstream node((*t)->state);
        getline(node, ss2, 'r');
        int ss1_c = 0, ss1_m = 0, ss2_c = 0, ss2_m = 0;

        for(int i=0; i<ss1.length(); i++)
            if(ss1[i] == 'c') ss1_c++;
            else ss1_m++;

        for(int i=0; i<ss2.length(); i++)
            if(ss2[i] == 'c') ss2_c++;
            else ss2_m++;

        if(ss1_c == ss2_c && ss1_m == ss2_m && boat == (*t)->boat)
            return true;

    } 
 
    return false;
}