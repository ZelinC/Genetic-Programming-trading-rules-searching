#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Welcome to the "Second phase GP" setup interface! 
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
# libraries and source data
library('TTR')
library('plyr')
library('zoo')
library('xts')
library('rlist')
library('plotly')
library('ggplot2')
library('rstudioapi')

current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path ))
print( getwd() )
source('Double_Evolution_GP_backend.R')
source('source_preparation.R')


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#----Configuration of hyper-parameters--------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Configuration of hyper-parameters
GP_hyperparam <- vector(mode="list", length=17)
names(GP_hyperparam) <- c("number of trials", "number of population", "number of evolution",
                          "elite ratio", "mutation rate", "principal size", "transaction cost",
                          "tree_max_depth", "fitness type", 
                          "training period", "validation period", "test period", 
                          "rule file name", "fitness file name", "log file name",
                          "seed", "fitness_report_all")

GP_hyperparam['number of trials']     = 5

GP_hyperparam['number of population'] = 50

GP_hyperparam['number of evolution']  = 10 ##

GP_hyperparam['elite ratio']          = 0.1

GP_hyperparam['mutation rate']        = 0.05

GP_hyperparam['principal size']       = 100 

GP_hyperparam['transaction cost']     = 0.0001

GP_hyperparam['tree max depth']       = 10

GP_hyperparam['fitness type']         = 'returns'    # or 'correctness', 'sharpe'

GP_hyperparam['training period']      = '1990/2014'

GP_hyperparam['validation period']    = '2015/2018'

GP_hyperparam['test period']          = NULL

GP_hyperparam['rule file name']       = 'rules_2nd.rds'

GP_hyperparam['fitness file name']    = 'rules_fitness_2nd.rds'

GP_hyperparam['log file name']        = 'GP log file'

GP_hyperparam['seed']                 = 2018

GP_hyperparam['fitness_report_all']   = FALSE

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#----gather first phase trading rules--------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# define number of parallel files. 
num_files = 2

train_rules = c()
train_fit   = c()


for (i in 1:num_files) {
  
  file_index = i
  train_rules_file = paste0("rules_1st_", file_index, ".rds")
  train_fit_file   = paste0("rules_fitness_1st_", file_index, ".rds")
  
  train_rules_load = readRDS(train_rules_file)
  train_fit_load   = readRDS(train_fit_file)
  
  train_rules = c(train_rules_load, train_rules)
  train_fit   = c(train_fit_load, train_fit)
  
}

# use the following if did not use parallel searching
# readRDS(GP_hyperparam[['rule file name']])
# readRDS(GP_hyperparam[['fitness file name']])

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#----GP runing--------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
data_loading(GP_hyperparam[['data file name']])
# second stage searching
double_evolution_GP_2nd_phase(dataframe, GP_hyperparam, 
                              train_rules = train_rules, 
                              train_fit = train_fit)




