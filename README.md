# GeneticAlgorithms
Aim to replicate the genetic algorithms that was used in paper (Allen&Karjalainen, 1999) using R. 

# Summary of the paper
Genetic algorithms found a combination of technical trading rules that can earn excess returns in foreigen exchange markets. 

# Introduction of the genetic algorithms

# Structure of algorithms
--> Main function: (an iteration procedure)
      1. create an initial generation of 500 random rules
      2. masure the fitness of each rule over the training period and rank according to fitness
      3. select the top-ranked rule and calculate its fitness over the selection period. save it as the initial best rule
      4. randomly select two rules, using weights attaching higher probability to more highly ranked rules. Apply the recombination operator to create a new rule, which then replaces an old rule, chosen using weights attaching higher probability to less highly ranked rules. (Repeat this procedure 500 times to create a new generation of rules).
      5. Measure the fitness of each rule in the new generation over the training period. Take the best rule in the training period and measure its fitness over the selection period. If it outperforms the previous best rule, save it as the new best rule. 
      6. Stop if no new best rule appears for 25 generations, or after 50 generations. Otherwise, return to step 4
      
      
--> Recombination (Crossover) operator
    


--> Mutation operator


--> Rule generation function
    The function generates a rule from a group of mathematical operators.
    (possibly output the code that was easily readable by machines, i.e. in gene codes instead of trees. 
    mathematical operators includes:
      * arithmetic operations: +, -, *, /, norm, avg, max, min, lag
      * Boolean operations: and, or, not, >, <
      * conditional operations: if-then, if-then-else
      * numerical constants
      * Boolean constants: True, False

--> Fitness function
    The function evaluates the excess return for a trading rule over the period from time 0 to time T
    r = sum_{t=0}^{T-1} z_t r_t + n ln( (1-c)/(1+c) )

# Definition of functions and variables

# 
