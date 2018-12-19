#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Welcome to the "First phase GP" setup interface! 
# Coder: Zelin Chen
#
# For bug or improvement suggestions please leave comments on: 
# <https://github.com/ZelinC/Genetic-Programming-trading-rules-searching/issues>

# version: 1.0 
# update date: 12/12/2018

# Declaration: "this program is for research purpose, I do not take any 
# responsbility for the loss that you made by using this program. Do not sue me 
# because I do not have money."
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#----data and package loading-----------------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
library('TTR')
library('plyr')
library('zoo')
library('xts')
library('rlist')
library('plotly')
library('ggplot2')
library('rstudioapi')
library('tidyverse')
#library('timetk')

current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path ))
print( getwd() )
source('Double_Evolution_GP_backend.R')




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#----Configuration of hyper-parameters--------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Configuration of hyper-parameters
GP_hyperparam <- vector(mode="list", length=18)
names(GP_hyperparam) <- c( "data file", "number of trials", "number of population", "number of evolution",
                          "elite ratio", "mutation rate", "principal size", "transaction cost",
                          "tree_max_depth", "fitness type", 
                          "training period", "validation period", "test period", 
                          "rule file name", "fitness file name", "log file name",
                          "seed", "fitness_report_all")

# change the following settings:

GP_hyperparam['data file name']       = 'D:/all_about_code/financial_data/EURUSD1440.csv'

GP_hyperparam['number of trials']     = 5

GP_hyperparam['number of population'] = 30

GP_hyperparam['number of evolution']  = 20

GP_hyperparam['elite ratio']          = 0.1

GP_hyperparam['mutation rate']        = 0.05

GP_hyperparam['principal size']       = 100 

GP_hyperparam['transaction cost']     = 0.0001 

GP_hyperparam['tree max depth']       = 10

GP_hyperparam['fitness type']         = 'returns'    # or 'correctness', 'sharpe', 'returns'

GP_hyperparam['training period']      = '2012/2017'

GP_hyperparam['validation period']    = '2018/2018'

GP_hyperparam['test period']          = NULL

GP_hyperparam['rule file name']       = 'rules_1st_2.rds' # make names into numeric order, e.g. 'rules_1st_1', rules_2st_2', ect.

GP_hyperparam['fitness file name']    = 'rules_fit_1st_2.rds'

GP_hyperparam['log file name']        = 'GP log file'

GP_hyperparam['seed']                 = 2

GP_hyperparam['fitness_report_all']   = FALSE




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#----GP runing--------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
data_loaded = data_loading(GP_hyperparam[['data file name']])
dataframe   = data_loaded[[1]]
log_return  = data_loaded[[2]]
# first stage searching 
double_evolution_GP_1st_phase(dataframe, GP_hyperparam)




