### Project background: 

This repository includes my own research about using genentic programming to search for profitable trading rules. 
This project is also a fulfilment for my degree [Master of Applied Econometrics](https://handbook.unimelb.edu.au/2018/courses/mc-appecon) thesis. 

### Available source:

* Slides for half-way presentation
* Report for final submission
* Code (TBU)
* Code user guide (TBU)
* Genetic programming introduction (TBU)


Anticipated update time: December (after my graduation)


### Implementation of the two-stage Genetic Programming searching system: 

* step 1: open 'Double_Evolution_GP.R'

* step 2: install essential R packages - TTR, plyr, zoo, xts, rlist, plotly, ggplot2, rstudioapi

* step 2: configure Genetic Programming hyper-parameters in R dictionary **GP_hyperparam**, the keys are explained in the following.
```
'number of trials': defines number of independent trading rules that the first phase will search

'number of population': defines number of trading rules in each population for evolution purpose

'number of evolution': defines iterations that a population will be evolved

'elite ratio': defines number of top trading rules are guarenteed to survive to the next generation

'mutation rate': defines probability of mutation happens during crossover

```

* step 3: source the file
```
source('Double_Evolution_GP_backend.R')
source('Source_preparation.R')
```

* step 4: wait for couple of hours or leave the computer for a night depends on the scale of the searching task.

* step 5: collect and visualise the results

The log file contains the general trading performance information about candidate trading rules. One can use the log file to identify top rule index from the saved candidate trading rules. 

For a selected trading rule, we can visualise its historical performance in the past training/validation/test periods.
```
selected_rule = top5_rules[[5]]
rule_visualisation(rule, dataframe, GP_hyperparam[['training period']], GP_hyperparam[['validation period']], GP_hyperparam[['test period']])
```
You can obtain the plot as following:
![alt text](https://github.com/ZelinC/Trading-rule-searching-Genetic-Programming-two-phase-searching-methodology/blob/master/sample_rule_performance.png)





# Reference
