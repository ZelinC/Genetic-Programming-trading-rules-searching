### Project background: 

This repository includes my own research and code about using genentic programming to search for profitable trading rules. 
This project is also a fulfilment for my degree [Master of Applied Econometrics](https://handbook.unimelb.edu.au/2018/courses/mc-appecon) thesis. 

### Available resources in this repository:

* [Master thesis on GP](https://github.com/ZelinC/Trading-rule-searching-Genetic-Programming-two-phase-searching-methodology/blob/master/Effective-searching-for-profitable-forex-trading-rules-via-genetic-programming-a-new-two-phase-searching-methodology.pdf)
* [Implementation instruction](https://github.com/ZelinC/Trading-rule-searching-Genetic-Programming-two-phase-searching-methodology/wiki/Implementation-Instruction)
* [Genetic programming introduction](https://github.com/ZelinC/Trading-rule-searching-Genetic-Programming-two-phase-searching-methodology/wiki)

### Program folder [Genetic-Programming-trading-rules-searching](https://github.com/ZelinC/Genetic-Programming-trading-rules-searching/blob/master/Genetic-Programming-trading-rules-searching.zip):
Click the link and view raw to download. It contains seven files. 
* `first_phase_GP_setp.R` and `second_phase_GP_setup.R` are the two files that you need to run to obtain the trading rules.
* `Double_Evolution_GP_backend.R` is the backend function support this program, do not open it unless you want to edit the source code. 
* `train2_fit.rds` and `train2_rules.rds` are my pre-searched trading rules for my research. 
* `GP_visulisation.R` helps you to visualise the performance of selected trading rules. 
* `USDJPY1440.csv` is the exchange rate data I used for my thesis, it is downloaded from MetaTrader 4. It is recommended that you download exchange rate data from the platform so it matches the input requirement. It works for different foreign exchange rates and frequencies. 
 

### Program information
* current version: 1.0
* recent update time: 18/12/2018

### Declaration
This program is for research purpose only. I do not take any responsibility that you made by using this program for real-world trading. Please do not sue me as I do not have money. 


### Reference
* Riccardo Poli, William B. Langdon, and Nicholas Freitag McPhee. 2008. A Field Guide to Genetic Programming. Lulu Enterprises, UK Ltd.
