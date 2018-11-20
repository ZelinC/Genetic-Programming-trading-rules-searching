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

* step 2: configure Genetic Programming hyper-parameters in vector ''
```
# defines number of independent trading rules that the first phase will search
GP_hyperparam['number of trials']                  
GP_hyperparam['number of population']
```


* step 3: source the file
```
code
```

* step 4: wait for couple of hours

* step 5: collect and visualise the results

The log file contains the general trading performance information about candidate trading rules. One can use the log file to identify top rule index from the saved candidate trading rules. 

For a selected trading rule, we can visualise its historical performance in the past training/validation/test periods.
```
selected_rule = top5_rules[[5]]
rule_visualisation(rule, dataframe, GP_hyperparam[['training period']], GP_hyperparam[['validation period']], GP_hyperparam[['test period']])
```





# Reference
