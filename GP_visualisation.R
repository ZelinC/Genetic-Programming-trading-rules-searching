#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Welcome to the visualisation interface! 
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

data_loading('USDJPY1440.csv')

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#----plotting--------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# load the searched rules
rules = readRDS('type save trading rules file name here')
rules_fitness = readRDS('type save trading rules fitness file name here')

# either pick a rule 
arule = rules[[2]]

# or select top rules
toprules = top_rule_selector(rules, rules_fitness, number = 10) # type a int for top selection


# rule performance plot
rule_visualisation(arule, dataframe, GP_hyperparam, plot_range = 'train_valid')

rule_visualisation(toprules[[1]], dataframe, GP_hyperparam, plot_range = 'train_valid')


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#----tree structure printing--------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# rule structure 
rule_printer(arule, full=TRUE)
rule_printer(toprules[[1]], full=TRUE)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#----trading rule implementation--------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# implement single trading rule
investment_decision(dataframe , toprules[5], '2010/2018')

# implement resembled trading rules 
# (i.e. combine a group of trading rules, let the mode tells you what you shoud do)
# this method is currently under development

