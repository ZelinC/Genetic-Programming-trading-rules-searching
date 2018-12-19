#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# GP backend code (functions support the GP algorithms)
# Coder: Zelin Chen
#
# For bug or improvement suggestions please leave comments on: 
# <https://github.com/ZelinC/Genetic-Programming-trading-rules-searching/issues>

# version: 1.0 
# update date: 12/12/2018

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#----financial data preparation--------------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
data_loading <- function(filename) {
  
  dataframe = read.csv(filename, header = F)
  
  colnames(dataframe)  = c('date','time','Open','High','Low','Close','Volume') 
  
  dataframe$datetime =  as.POSIXct(paste(dataframe$date, dataframe$time), format="%Y.%m.%d %H:%M")
  
  dataframe = dataframe[c('datetime','High','Low','Close')]
  
  dataframe = as.xts(dataframe[, -1], order.by = dataframe$datetime)
  #colnames(dataframe) = 'price.close'
  
  log_return = diff(log(dataframe$Close), lag=1)
  
  
  return(list(dataframe, log_return))
  
}



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#----GP setup (loop over trials)--------------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
double_evolution_GP_1st_phase <- function(dataframe, GP_hyperparam) {

  set.seed(GP_hyperparam[['seed']])
  
  # define saving space for trading rules and their fitness
  train_rules = c()
  train_fit = c()
  
  # loop over trial, each trial will return one rule that is the best in the trial population after the evolution
  for (t in 1:GP_hyperparam[['number of trials']]) {

    begin_print = paste(c("Trial ", t,":"), collapse="")
    
    cat(paste0("\n------------------------",begin_print,"--------------------------------------------"))

    
    trail.starttime = Sys.time()
    #***************************************************************************************
    results <- GP_trial(dataframe, GP_hyperparam)
    
    train_rules = c(train_rules, list(results[[1]]))
    train_fit =  c(train_fit, results[[2]])
    #***************************************************************************************
    trail.endtime <- Sys.time()
    
    end_print = paste(c(begin_print, paste(c("Max fitness: ", train_fit[[t]]), collapse=""),
                        paste(c("Time consumption: ", round(trail.endtime-trail.starttime,digits=2), " mins"), collapse="")), collapse = ", ")
    cat(end_print, '\n')
    
  }
  
  saveRDS(train_rules, file=GP_hyperparam[['rule file name']])
  saveRDS(train_fit,   file=GP_hyperparam[['fitness file name']])
  

}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#----2nd phase evolution--------------------------------------------------------------

# evolve those indepndent locally optimised trading rules from the 1st phase
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

double_evolution_GP_2nd_phase <- function(dataframe, GP_hyperparam, train_rules, train_fit) {
  
  period = GP_hyperparam[['training period']]
  log_return <- diff(log(dataframe$Close), lag=1)
  
  new_population = c()
  for (i in 1:GP_hyperparam[['number of evolution']]) {
    
    start.time  = Sys.time()
    new_population = train2_elite(population = train_rules, 
                                  population_fitness = train_fit, 
                                  elite_ratio = GP_hyperparam[['elite ratio']], 
                                  mutation_rate = GP_hyperparam[['mutation rate']])
    population_fitness = fitness(dataframe, new_population, period, log_return)
    end.time    = Sys.time()
    
    cat("\nCurrent generation period: ", i, 
        ", Max principal: ", max(population_fitness),
        ", 1000 rules Average performance: ", mean(population_fitness),
        "Time consumption: ", end.time-start.time)
    
    write(paste('*Current generation period:', i,
                ", 1000 rules Average performance: ", mean(population_fitness), 
                ", top 20 rules Average performance: ", mean(tail(sort(population_fitness),50)),
                ", top 10 rules Average performance: ", mean(tail(sort(population_fitness),20)),
                ", top 5 rules Average performance: ", mean(tail(sort(population_fitness),5)),
                ", best rule performance: ", max(population_fitness),
                sep=""), 
          file="benchmark_log.txt", append=TRUE) 
    
    population = new_population
  }
  
  saveRDS(new_population,     file=GP_hyperparam[['rule file name']])
  saveRDS(population_fitness, file=GP_hyperparam[['fitness file name']])
  
}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#----Genetic Programming for each independent trial-------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# input: data, population size, iteration, crossover rate, mutation rate, (control of rules)
# output: one selected rule

GP_trial <- function(dataframe, GP_hyperparam) {
 
  # calculate rate of return each day if in the market. (in %) 
  # ********can treat this as input data.*************************************edit!!!
  log_return <- diff(log(dataframe$Close), lag=1)
  
  # 1. generate an initial population:
  start.time <- Sys.time()
  population = initial_population(GP_hyperparam[['number of population']], max_depth=GP_hyperparam[['tree max depth']])
  pop_fit    = fitness(dataframe, 
                       population, 
                       period = GP_hyperparam[['training period']], 
                       log_return, 
                       principal = GP_hyperparam[['principal size']]
                       )
  max_return = max(pop_fit)
  end.time = Sys.time()
  cat("\nCurrent generation period: ", 1, 
      ", Max fitness: ", max_return[1],
      "Time consumption: ", end.time-start.time,
      "\n")
  
  # 2. loop:
  ##  2.1. get fitness, and associated crossover selection prob
  ##  2.2. create new population through cross and mutation
  # stop until iteration achieved
  for (g in 2:GP_hyperparam[['number of evolution']]) {
    # record time
    start.time <- Sys.time()
    
    population = evolution(population, pop_fit, 
                           elite         = GP_hyperparam[['elite ratio']], 
                           mutation_rate = GP_hyperparam[['mutation rate']], 
                           max_depth     = GP_hyperparam[['tree max depth']])
    pop_fit    = fitness(dataframe, population, 
                         period = GP_hyperparam[['training period']], 
                         log_return, 
                         principal = GP_hyperparam[['principal size']])
    
    # record evolution progress: improvement of max returns
    max_return = c(max_return, max(pop_fit))
    
    end.time = Sys.time()
    cat("Current generation period: ", g, 
        ", Max fitness: ", max_return[g], 
        "Time consumption: ", end.time-start.time,
        "\n")
  }
  
  # 3. report the best rule based on its fitness.  
  ## * which.max stops at first occruance of maximum. only report 1. 
  best_rule = list(population[[ which.max(pop_fit)]])
  best_return = max(pop_fit)
  
  
  # plot evolution 
  # plot(c(1:iteration), max_return, type='l')
  
  return(c(best_rule, best_return))
}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#----Generate a new population of trading rules-----------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
initial_population <- function(population_size, max_depth) {
  
  i = 0
  initial_population = c()
  while (i<population_size) {
    # depth to identify levels, but it should start from 0
    initial_population = c(initial_population, list(list(rule_generator(max_depth = max_depth, long_short='long'), 
                                                         rule_generator(max_depth = max_depth, long_short='short')
    ))
    )
    i = i+1
  }
  
  return(initial_population)
  
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#----Generate a new rule for the new population------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
rule_generator <- function(output = 'b', depth = 0, max_depth, long_short) {
  
  depth = depth + 1
  
  # list of nodes 
  # functions that output booleans
  boolean.output.fn.list = c('and.fn','or.fn','xor','larger.fn','smaller.fn',
                             'MACD.fn','RSI.fn', 'stoch.fn','BBands.fn', 'KST.fn',
                             'DonchianChannel.fn', 'True.fn','False.fn')
  
  # functions that output real numbers (terminal function list)
  realnum.output.fn.list = c('runMax', 'runMin', 'SMA', 'EMA', 'Close','High','Low')
  
  # starting root node function (input=b, output=b)
  start.node.list = c('and.fn','or.fn','xor')
  
  # functions that output booleans - and/or
  shorten.boolean.output.fn.list = setdiff(boolean.output.fn.list, start.node.list)
  
  # function does not require inputs
  no.input.fn.list = c('True.fn','False.fn','Close','High','Low')
  
  # price inputs
  price.list = c('Close','High','Low')
  
  # parameter range
  param.range = c(1:200)
  
  # creation of the node
  if (depth == 1) {
    # level 1 node from start.node.list
    # starting node
    node = sample(start.node.list, 1)
    rule = list(node) } 
  else {
    
    if (output == 'b') {
      # other level nodes creation: if level>max level, stop drawing 'and','or','xor'
      if (depth <= max_depth) {
        node = sample(boolean.output.fn.list, 1)
        rule = list(node) } 
      else {
        # finish with functions that take only numeric values.
        node = sample(shorten.boolean.output.fn.list, 1)
        rule = list(node) } } 
    else {
      # output == 'r'
      node = sample(realnum.output.fn.list, 1)
      rule = list(node)
    } }
  
  # createion of the leaves
  if (node %in% start.node.list) {
    
    # and/or needs boolean input, so leaves should output booleans 
    rule = list.append(rule, list(rule_generator(output = 'b', depth, max_depth= max_depth, long_short), 
                                  rule_generator(output = 'b', depth, max_depth= max_depth, long_short)))
    
  } else if (node %in% c('larger.fn','smaller.fn')) {
    
    # and/or needs boolean input, so leaves should output booleans 
    rule = list.append(rule, list(rule_generator(output = 'r', depth, max_depth= max_depth, long_short), 
                                  rule_generator(output = 'r', depth, max_depth= max_depth, long_short)))
    
  } else if (node %in% realnum.output.fn.list & node %ni% price.list) {
    
    #rule = list.append(rule, sample(param.range,1))
    rule = list.append(rule, list(sample(price.list,1), sample(param.range,1)))
    
  } else if (node=='MACD.fn') {
    
    rule = list.append(rule, c(12,26,9))
    
  } else if (node=='RSI.fn') {
    
    if (long_short == 'long') {
      
      #rule = list.append(rule, c(sample(c(20:30),1),sample(c(70:80),1)))
      rule = list.append(rule, sample(c(20:40),1))
    } else {
      # short
      rule = list.append(rule, sample(c(60:80),1))
    }
    
  } else if (node %in% c('SMI.fn', 'stoch.fn','BBands.fn', 'KST.fn','DonchianChannel.fn')) {
    
    rule = list.append(rule, 0)
    
  } else if (node %in% no.input.fn.list) {
    
    rule = list.append(rule, 0)
    #do nothing. 
    
  } 

  return(rule)
} 

'%ni%' <- Negate('%in%')

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#----Trading rule printer---------------------------------------------------------------------
#
# print the tree-structured trading rule in a hierarchical representation
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
rule_printer <- function(rule, level=1, full=FALSE) {
  
  # full: print both buy and sell
  if (full == TRUE) {
    
    cat('buy rule:\n')
    rule_printer(rule[[1]],level=1)
    cat('sell rule:\n')
    rule_printer(rule[[2]],level=1)
    
  } else {
    
    node = rule[[1]]
    leaves = rule[[2]]
    
    if (node %in% c('and.fn','or.fn','xor','larger.fn','smaller.fn')) {
      
      cat(  paste(strrep(' ',(level-1)*3), '(',level,') ',node,'\n', sep = "")  )
      
      rule_printer(leaves[[1]], level=level+1)
      rule_printer(leaves[[2]], level=level+1) 
      
    } else if (node %in% c('MACD.fn','RSI.fn')) {
      
      cat(  paste(strrep(' ',(level-1)*3), '(',level,') ',node, sep = "")  )
      cat('(')
      cat(leaves)
      cat(')\n')
      
    } else if (node %in% c('runMax','runMin','SMA','EMA')) {
      
      cat(  paste(strrep(' ',(level-1)*3), '(',level,') ',node, sep = "")  )
      cat('(')
      cat(unlist(leaves))
      cat(')\n')
      
    } else if (node %in% c('True.fn','False.fn','SMI.fn',
                           'stoch.fn','BBands.fn', 'KST.fn','DonchianChannel.fn')) {
      
      cat(  paste(strrep(' ',(level-1)*3), '(',level,') ',rule[[1]],'\n', sep = "")  )
      
    } else if (node %in% c('Close','High','Low')) {
      
      cat(  paste(strrep(' ',(level-1)*3), '(',level,') ',rule[[1]],'\n', sep = "")  )
      
    }
    
  }
}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#----Trading rule reader---------------------------------------------------------------------
#
# implement the rule and obtain the long/short investment decision
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rule_decision <-  function(dataframe, rule, long_short) {
  
  # as.character(substitute(root.fn))
  if (rule[[1]] %in% c('and.fn','or.fn','xor','larger.fn','smaller.fn')) {
    
    # input subrules into rule_decision 
    leaf1 = rule_decision(dataframe, rule[[2]][[1]], long_short)
    leaf2 = rule_decision(dataframe, rule[[2]][[2]], long_short)
    
    # make the mathemtical operator
    root.fn = eval(parse(text=rule[[1]]))
    results = root.fn(leaf1, leaf2)
    
  } else if (rule[[1]] %in% c('runMax', 'runMin', 'SMA', 'EMA')) { 
    
    leaf.param = as.numeric(unlist(rule[[2]])[[2]])
    leaf.series= unlist(rule[[2]])[[1]] 
    root.fn    = eval(parse(text=rule[[1]])) 
    results    = root.fn(dataframe[,leaf.series], n= leaf.param) 
    
  } else if (rule[[1]] == 'MACD.fn' ) {
    
    leaf.param = unlist(rule[[2]])
    root.fn    = eval(parse(text=rule[[1]]))
    results    = root.fn(dataframe[,'Close'], 
                         nFast = as.numeric(leaf.param[[1]]), 
                         nSlow = as.numeric(leaf.param[[2]]), 
                         nSig = as.numeric(leaf.param[[3]]), 
                         long_short) 
    
  } else if (rule[[1]] == 'RSI.fn') {
    
    leaf.param = as.numeric(unlist(rule[[2]]))
    root.fn    = eval(parse(text=rule[[1]]))
    results    = root.fn(dataframe[,'Close'], n = 14, criteria = leaf.param, long_short)
    
  } else if (rule[[1]] %in% c('High','Low','Close')) {
    
    
    results    = dataframe[,rule[[1]]]
    
    
  } else if (rule[[1]] %in% c('True.fn','False.fn')) {
    
    root.fn    = eval(parse(text=rule[[1]]))
    results    = root.fn(dataframe[,'Close'])
    
  } else if (rule[[1]] %in% c('stoch.fn','BBands.fn', 'KST.fn','DonchianChannel.fn')) {
    
    root.fn    = eval(parse(text=rule[[1]]))
    results    = root.fn(dataframe, long_short)
    
  }
  
  return(results)
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#----TTR reader-------------------------------------------------------------------------------
#
# transform teachnical trading rules into long/short investment decision
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

and.fn <- function(arg1, arg2) {
  return(arg1 & arg2)
}

or.fn <- function(arg1, arg2) {
  return(arg1|arg2)
}

larger.fn <- function(arg1, arg2) {
  return(arg1 > arg2)
}

smaller.fn <- function(arg1, arg2) {
  return(arg1 < arg2)
}

RSI.fn <- function(dataframe, n=14, criteria, long_short) {
  
  rsi.series = RSI(dataframe$Close, n)
  
  if (long_short=='long') {
    # oversold --> buy
    return(rsi.series<criteria)
  } else {
    # overbought --> sell
    return(rsi.series>criteria)
  }
  
}

MACD.fn <- function(dataframe, nFast = 12, nSlow = 26, nSig = 9, long_short) {
  
  macd.line <- MACD(dataframe, nFast, nSlow, nSig)$macd 
  signal.line <- MACD(dataframe, nFast, nSlow, nSig)$signal
  
  if (long_short=='long') {
    # buy signal
    return(macd.line>signal.line) 
  } else {
    # sell signal
    return(signal.line>macd.line)
  }
}

BBands.fn <- function(dataframe, long_short) {
  
  BB_channel = BBands(dataframe)
  
  if (long_short=='long') {
    # buy signal
    return(dataframe$Close < BB_channel$dn) 
  } else {
    # sell signal
    return(dataframe$Close > BB_channel$up)
  }
}

stoch.fn <- function(dataframe, long_short) {
  
  stoch.index = stoch(dataframe)
  
  if (long_short=='long') {
    # buy signal
    return(stoch.index$slowD < 20) 
  } else {
    # sell signal
    return(stoch.index$slowD > 80)
  }  
}


KST.fn <- function(dataframe, long_short) {
  
  kst.index = KST(dataframe$Close)
  
  
  if (long_short=='long') {
    # buy signal
    return(kst.index$kst > kst.index$signal) 
  } else {
    # sell signal
    return(kst.index$kst < kst.index$signal)
  }
}

DonchianChannel.fn <- function(dataframe, long_short) {
  
  DC_channel = DonchianChannel(dataframe[,c('High','Low')])
  
  if (long_short=='long') {
    # buy signal
    return(dataframe$Close > DC_channel$high) 
  } else {
    # sell signal
    return(dataframe$Close < DC_channel$low)
  }
  
}

True.fn <- function(dataframe) {
  return(dataframe!=TRUE)
}

False.fn <- function(dataframe) {
  return(dataframe==TRUE)
}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#----trading rule fitness evaluation----------------------------------------------------------
#
# evaluate the fitness of each rule in the population over the training or validation period
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fitness <- function(dataframe, population, period, log_return, 
                    fitness_type     = GP_hyperparam[['fitness type']], 
                    principal        = GP_hyperparam[['principal size']], 
                    transaction_cost = GP_hyperparam[['transaction cost']],
                    report_all       = GP_hyperparam[['fitness_report_all']]) {
  
  size = length(population)
  
  population_fitness = c()
  
  for (i in 1:size) {
    
    rule = population[[i]]
    population_fitness = c(population_fitness, 
                           single_rule_fitness(dataframe, rule, period, log_return, 
                                               fitness_type = GP_hyperparam[['fitness type']], 
                                               principal = principal, 
                                               transaction_cost = transaction_cost,
                                               report_all = report_all
                                               ))
  }
  
  return(population_fitness)
}


single_rule_fitness <- function(dataframe, rule, period, log_return, 
                                fitness_type     = GP_hyperparam[['fitness type']], 
                                principal        = GP_hyperparam[['principal size']], 
                                transaction_cost = GP_hyperparam[['transaction cost']],
                                report_all       = GP_hyperparam[['fitness_report_all']]) {
  
  # separate period into start/end
  # include one extra past year for period for calculating of moving averages at the begining of the period
  split_period = unlist(strsplit(period, "/"))
  start_period = split_period[[1]]
  end_period   = split_period[[2]]
  train_period.start = toString(as.numeric(start_period)-1)
  train_period = paste0(train_period.start,'/',end_period,sep='')
  
  # obtain decisions
  decision.buy = rule_decision(dataframe[train_period], rule[[1]], long_short='long')
  decision.sell = rule_decision(dataframe[train_period], rule[[2]], long_short='short')
  decision = decision.buy[period] - decision.sell[period]
  
  # manipulate the decision into long/short position in the market
  # decision=0  implies contradiction of long/short indicators, stay in previous market. 
  # decision=1  implies long position
  # decision=-1 implies short position
  
  previous = 0
  transaction = decision
  for (t in 1:length(decision)) {
    
    if (decision[[t]] == 0 ) {
      if (previous == 1) {
        decision[[t]] = 1
      } 
      else {
        # previous == -1
        decision[[t]] = -1
      }
    }
    
    if (decision[[t]] != previous) {
      # transaction happens
      transaction[[t]] = 1
      # update previous position status
      previous = decision[[t]]
    } else {
      transaction[[t]] = 0
    }
    
  }
  
  # adjust investment decision to matach the correct prediction day.
  decision[-seq(1)] = decision[seq(length(decision)-1)]
  transaction[-seq(1)] = transaction[seq(length(transaction)-1)]
  decision[1] = NA 
  transaction[1] = NA
  log_return = log_return[period]
 
   
  if (fitness_type == 'returns') {
    ## 1. accumulated rate of return as fitness
    
    # toal returns each trading day. 1+r
    returns = decision * log_return + 1 - transaction * transaction_cost
    
    # accumulated total return in the market
    report_fitness = principal * prod(c(returns[-1])) 
    
  }
  
  else if (fitness_type == 'correctness') {
    ## 2. percentage of correct prediction of next day market movement as fitness
    market_movement = log_return
    market_movement[market_movement>0] = 1
    market_movement[market_movement<0] = 0
    time_horizon = length(market_movement)
    
    decision[decision==1]  = 1
    decision[decision==-1] = 0
    
    predict_correctness = (time_horizon - sum(abs(market_movement[-1] - decision[-1])))/time_horizon
    report_fitness = predict_correctness
  }
  
  ## 3. Sharpe ratio as fitness
  else if (fitness_type == 'sharpe') {
    
    # toal returns each trading day. 1+r
    returns = decision * log_return + 1 - transaction * transaction_cost
    strategy_returns = returns[-1] - 1
    volatility = sd(c(strategy_returns))
    report_fitness = mean(c(strategy_returns))/volatility #+ 1 for probability selection, avoid negative results.
  }
  
  if (report_all == TRUE) {
    
    # realised volatility for the whole period
    realised.volatility = sum(c(strategy_returns)**2)
    report_all = c(single_rule_fitness(dataframe, rule, period, log_return, report_all = FALSE, fitness_type = 'returns'),
                   single_rule_fitness(dataframe, rule, period, log_return, report_all = FALSE, fitness_type = 'correctness'),
                   single_rule_fitness(dataframe, rule, period, log_return, report_all = FALSE, fitness_type = 'sharpe'),  
                   sum(transaction[-1]), volatility, realised.volatility)
    names(report_all) = c("returns", "correctness", "sharpe","num_transactions","volatility",'rv')
    
    return(report_all) 
  } else {
    return(report_fitness)
  }
}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#----tracking trading rule performance overtime ----------------------------------------------
#
# track the decision making and %return of each trading rule
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
single_strategy_returns <- function(dataframe, rule, period, log_return, 
                                    principal        = GP_hyperparam[['principal size']], 
                                    transaction_cost = GP_hyperparam[['transaction cost']]){
  
  
  # separate period into start/end
  # include one extra past year for period for calculating of moving averages at the begining of the period
  split_period = unlist(strsplit(period, "/"))
  start_period = split_period[[1]]
  end_period   = split_period[[2]]
  train_period.start = toString(as.numeric(start_period)-1)
  train_period = paste0(train_period.start,'/',end_period,sep='')
  
  # obtain decisions
  decision.buy = rule_decision(dataframe[train_period], rule[[1]], long_short='long')
  decision.sell = rule_decision(dataframe[train_period], rule[[2]], long_short='short')
  decision = decision.buy[period] - decision.sell[period]
  
  # manipulate the decision into long/short position in the market
  # decision=0  implies contradiction of long/short indicators, stay in previous market. 
  # decision=1  implies long position
  # decision=-1 implies short position
  
  previous = 0
  transaction = decision
  for (t in 1:length(decision)) {
    
    if (decision[[t]] == 0 ) {
      if (previous == 1) {
        decision[[t]] = 1
      } 
      else {
        # previous == -1
        decision[[t]] = -1
      }
    }
    
    if (decision[[t]] != previous) {
      # transaction happens
      transaction[[t]] = 1
      # update previous position status
      previous = decision[[t]]
    } else {
      transaction[[t]] = 0
    }
    
  }
  
  ## implement rule to calculate its rate of return. 
  # the decision indicates returns for the next trading day,
  # so we shift indicators up to match the correct day
  decision[-seq(1)] = decision[seq(length(decision)-1)]
  transaction[-seq(1)] = transaction[seq(length(transaction)-1)]
  ## truncate data by 1 to make fitness comparison consistent in time frame. 
  decision[1] = NA 
  transaction[1] = NA
  
  ## truncate data to make two series both have non NA
  ## due to NA for decisions under some series with large MA parameters.
  log_return = log_return[period]
  
  # toal returns each trading day. r
  log_return = decision * log_return - transaction * transaction_cost
  
  return(log_return)
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#----Ensemble of multiple trading rules -----------------------------------------------------
#
# Make trading decision based on a group of trading rules, and take the mode decision.
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
single_rule_decision <- function(dataframe, rule, period){
  
  # separate period into start/end
  split_period = unlist(strsplit(period, "/"))
  start_period = split_period[[1]]
  end_period   = split_period[[2]]
  train_period.start = toString(as.numeric(start_period)-1)
  train_period = paste0(train_period.start,'/',end_period,sep='')
  
  # obtain decisions
  decision.buy = rule_decision(dataframe[train_period], rule[[1]], long_short='long')
  decision.sell = rule_decision(dataframe[train_period], rule[[2]], long_short='short')
  
  # combining two decisions, and select periods ([period] to truncate early periods)
  decision = decision.buy[period] - decision.sell[period]
  
  previous = 0
  transaction = decision
  for (t in 1:length(decision)) {
    
    if (decision[[t]] == 0 ) {
      if (previous == 1) {
        decision[[t]] = 1
      } 
      else {
        # previous == -1
        decision[[t]] = -1
      }
    }
    
    if (decision[[t]] != previous) {
      # transaction happens
      transaction[[t]] = 1
      # update previous position status
      previous = decision[[t]]
    } else {
      transaction[[t]] = 0
    }
    
  }
  
  
  return(decision)
}


investment_decision <- function(dataframe, rules, period) {
  
  if (length(rules) == 1) {
    
    decisions = single_rule_decision(dataframe = dataframe,  rules[[1]], period)
    
    
  } else {
    cat('ensemble of investment decision via multiple trading rules is in development...')
    cat('please try again and insert only one trading rule.')
    #decisions = multiple_rule_decision(dataframe = currency_data,  rules, period)
    break
  }
  
  next_day_decision = decisions[[length(decisions)]]
  
  
  cat('\n############################################################\n')
  cat(paste0('the investment decision made on date ', index(dataframe)[length(index(dataframe))],' for the next trading day is: \n'))
  if (next_day_decision == 1) {
    cat('* go long!\n') 
  }  else {
    cat('* go short!\n')
  }
  cat('############################################################\n')
  
}

return_vol <- function(currency_data, strategy, period, log_return, 
                       principal        = GP_hyperparam[['principal size']], 
                       transaction_cost = GP_hyperparam[['transaction cost']]) {
  
  # returns for each period
  log_return = single_strategy_returns(currency_data, strategy, period, log_return, principal, transaction_cost)
  
  # realised volatility for the whole period
  realised.volatility = sum(c(log_return[-1])**2)
  
  # general volatility for the whole period
  volatility = sd(c(log_return[-1]))
  
  return(c(volatility, realised.volatility))
}


strategy_combiner <- function(dataframe, rules, period, log_return, 
                              principal        = GP_hyperparam[['principal size']], 
                              transaction_cost = GP_hyperparam[['transaction cost']]) {
  
  num_rules = length(rules)
  individual_decision = single_strategy_decision(dataframe,rules[[1]], period, log_return)
  
  for (i in 2:num_rules) {
    
    strategy = rules[[i]]
    individual_decision = cbind(individual_decision, 
                                single_strategy_decision(dataframe,strategy, period, log_return))
    
    
  }
  
  individual_decision$decisionSum = rowSums(individual_decision, na.rm = TRUE, dims=1)
  
  long_index <- individual_decision$decisionSum > 0
  short_index <- individual_decision$decisionSum < 0
  individual_decision$decision[long_index]  <-  1
  individual_decision$decision[short_index] <- -1
  
  individual_decision$transaction = individual_decision$decision
  
  previous = 0
  for (t in 1:length(individual_decision$decision)) {
    
    if (individual_decision$decision[[t]] != previous) {
      # transaction happens
      individual_decision$transaction[[t]] = 1
      # update previous position status
      previous = individual_decision$decision[[t]]
    } else {
      individual_decision$transaction[[t]] = 0
    }
    
  }
  
  ## truncate data to make two series both have non NA
  ## due to NA for decisions under some series with large MA parameters.
  log_return = log_return[period]
  
  # toal returns each trading day. 1+r
  log_return = individual_decision$decision * log_return + 1 - individual_decision$transaction * transaction_cost
  
  # assume default 1m principal: end of day total value. # -1 skip the first NA value
  principal = principal * prod(c(log_return[-1])) 
  
  return(principal)
}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#----Population evolution functions -----------------------------------------------------
#
# apply crossover, mutation and elitelism to evolve the population
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

evolution <- function(population, population_fitness, 
                      elite, mutation_rate, max_depth) {
  
  # get number of elite to select
  num_elite = as.integer(length(population)*elite)
  
  new_population = c()
  
  # select elites 
  new_population = c(new_population, top_rule_selector(population, population_fitness, num_elite))
  
  # crossover_mutation
  ## elites will still partipate in crossover and mutation operation
  ## crossover only return (length(pop) - num_elite) numbers of rules. 
  new_population = c(new_population, 
                     crossover_mutation(population, population_fitness, 
                                        num_elite, mutation_rate=mutation_rate, max_depth=max_depth))
  
  return(new_population)
}

crossover_mutation <- function(population, population_fitness, num_elite, mutation_rate=mutation_rate, max_depth = max_depth) {
  
  # creating prior space and values
  population_size = length(population)
  new_population  = c()
  node.fn.list = c('and.fn', 'or.fn','larger.fn','smaller.fn')
  and.or.fn = c('and.fn', 'or.fn')
  terminal.fn.list = c('runMax', 'runMin', 'SMA')
  
  # creating roulette wheel probability for selection.: prob = F/sum(F)
  sum_fitness = sum(population_fitness)
  select_prob   = unlist(population_fitness)/sum_fitness
  
  for (i in 1:(population_size-num_elite)) {
    
    # 1. randomly select two parent rules based on population_fitness. 
    parents = sample(population, 2, prob = select_prob)
    parent1 = parents[[1]]
    # [Mutation] parent 2 has a probability to be replaced by a new rule.
    if (as.logical(rbinom(1,size=1,prob=mutation_rate))==TRUE) {
      parent2 = list(rule_generator(max_depth = max_depth, long_short='long'), rule_generator(max_depth = max_depth, long_short='short'))
    } else {
      parent2 = parents[[2]]
    }
    
    # call crossover operator for both buy and sell strategy
    buy.rule  = crossover_operator(parent1[[1]],parent2[[1]])
    sell.rule = crossover_operator(parent1[[2]],parent2[[2]])
    new.rule  = list(buy.rule, sell.rule)
    
    # save rule into new population
    new_population = c(new_population, list(new.rule))
  }
  
  return(new_population)
}

# Single rule crossover
crossover_operator <- function(parent1, parent2) {
  
  parent1[[2]][[sample(c(1,2),1)]] = parent2[[2]][[sample(c(1,2),1)]]
  
  return(parent1)
}


# collect all subrules under terminal.fn.list
subrule_collector <- function(parent2) {
  
  and.or.fn = c('and.fn', 'or.fn')
  rules = list()
  
  subrule1 = parent2[[2]][[1]]
  if (subrule1[[1]] %in% and.or.fn) {
    rules = c(rules, subrule_collector(subrule1))
  }
  else {
    rules = c(rules, list(subrule1[[2]][[1]]), list(subrule1[[2]][[2]]))
  }
  
  subrule2 = parent2[[2]][[2]]
  if (subrule2[[1]] %in% and.or.fn) {
    rules = c(rules, subrule_collector(subrule2))
  }
  else {
    rules = c(rules, list(subrule2[[2]][[1]]), list(subrule2[[2]][[2]]))
  }
  
  return(rules)
}


# Top rule selection: elitism
top_rule_selector <- function(population, fitness, number){
  
  top_fit = tail(sort(unlist(fitness)), number)
  position = c()
  selected_rules = list()
  
  # identify their position in the fitness vector
  for (i in 1:number) {
    
    position = append(position, min(which(unlist(fitness)==top_fit[i]))) # min make sure to get the position of first element found
    selected_rules = append(selected_rules, list(population[[ position[i] ]]))
  }
  
  return(selected_rules)
}

# second phase evolution: elitism
train2_elite <- function(population, population_fitness, 
                         elite_ratio = GP_hyperparam[['elite ratio']], 
                         max_depth   = GP_hyperparam['tree max depth'],
                         mutation_rate = GP_hyperparam[['mutation rate']]) {
  
  # get number of elite to select
  num_elite = as.integer(length(population)*elite_ratio)
  
  new_population = c()
  
  # select elites 
  new_population = c(new_population, elite(population, population_fitness, num_elite, duplication = FALSE))
  
  # crossover buy and sell rules
  new_population = c(new_population, 
                     train2_crossover(population, population_fitness, 
                                      num_elite, 
                                      max_depth = max_depth,
                                      mutation_rate=mutation_rate))
  return(new_population)
}

# second phase evolution: crossover and mutation 
train2_crossover <- function(population, population_fitness, num_elite, max_depth, mutation_rate) {
  
  new_population  = c()
  population_size = length(population)
  
  # creating roulette wheel probability for selection.: prob = F/sum(F)
  sum_principal = sum(population_fitness)
  select_prob   = unlist(population_fitness)/sum_principal
  
  for (i in 1:(population_size-num_elite)) {
    
    # 1. randomly select two parent rules based on population_fitness. 
    parents = sample(population, 2, prob = select_prob)
    
    sample_order = sample(c(1,2),2)
    
    # call crossover operator for both buy and sell strategy
    buy.rule  = parents[[ sample_order[[1]] ]][[1]]
    sell.rule = parents[[ sample_order[[2]] ]][[2]]
    
    if (as.logical(rbinom(1,size=1,prob=mutation_rate))==TRUE) {
      buy.rule = rule_generator(max_depth = max_depth, long_short='long')
    } else {
      buy.rule  = parents[[ sample_order[[1]] ]][[1]]
    }
    
    if (as.logical(rbinom(1,size=1,prob=mutation_rate))==TRUE) {
      sell.rule = rule_generator(max_depth = max_depth, long_short='short')
    } else {
      sell.rule  = parents[[ sample_order[[2]] ]][[2]]
    }
    
    new.rule  = list(buy.rule, sell.rule)
    
    # save rule into new population
    new_population = c(new_population, list(new.rule))
  }
  
  return(new_population)
}

elite <- function(population, fitness, number = 10, duplication = TRUE) {
  
  top_fit = tail(sort(count(fitness)$x), number)
  
  position = c()
  elites = list()
  
  # identify their position in the fitness vector
  for (n in 1:number) {
    
    position = append(position, min(which(unlist(fitness)==top_fit[n]))) # min make sure to get the position of first element found
    elites = append(elites, list(population[[ position[n] ]]))
  }
  
  return(elites)
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#----Trading rule performance visualisation -------------------------------------------------
#
# plot the movement of the market price, market position and accumulative return
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rule_visualisation <- function(rule, dataframe, GP_hyperparam, plot_range) {
  
  # define plotting period 
  if (plot_range == "train_valid") {
    
    start_period_training     = sub('/.*$','',GP_hyperparam[['training period']])
    start_period_validation   = sub('^.*/','',GP_hyperparam[['training period']])
    end_period_validation     = sub('^.*/','',GP_hyperparam[['validation period']])
   
    period = paste0(start_period_training,'/',end_period_validation) 
  }
  
  # obtain decisions
  decision.buy = rule_decision(dataframe, rule[[1]], long_short='long')
  decision.sell = rule_decision(dataframe, rule[[2]], long_short='short')
  
  # combining two decisions, and select periods
  decision = decision.buy[period] - decision.sell[period]
  
  #  1: buy in
  # -1: sell    --> 0: as no short sell
  #  0: stay    --> 1/0: given buy/sell decision
  # manipulate 0:stay to corresponding to its previous action. 0-->1/-1 to indicate long/short position
  previous = 0
  transaction = decision
  for (t in 1:length(decision)) {
    
    if (decision[[t]] == 0 ) {
      if (previous == 1) {
        decision[[t]] = 1
      } 
      else {
        # previous == -1
        decision[[t]] = -1
      }
    }
    
    if (decision[[t]] != previous) {
      # transaction happens
      transaction[[t]] = 1
      # update previous position status
      previous = decision[[t]]
    } else {
      transaction[[t]] = 0
    }
    
  }
  
  ## implement rule to calculate its rate of return. 
  # the decision indicates returns for the next trading day,
  # so we shift indicators up to match the correct day
  decision[-seq(1)] = decision[seq(length(decision)-1)]
  transaction[-seq(1)] = transaction[seq(length(transaction)-1)]
  ## truncate data by 1 to make fitness comparison consistent in time frame. 
  decision[1] = NA 
  transaction[1] = NA
  
  ## truncate data to make two series both have non NA
  ## due to NA for decisions under some series with large MA parameters.
  log_return <- diff(log(dataframe$Close), lag=1) 
  log_return = log_return[period]
  log_return = log_return[period]
  log_return = decision * log_return + 1 - transaction * GP_hyperparam[['transaction cost']]
  log_return = na.omit(log_return)
  
  
  price = dataframe$Close[period]
  
  #plot(Visits ~ Date, dm, xaxt = "n", type = "l")
  
  #plot(c(price,decision))
  df = data.frame(date=index(price), coredata(price))
  decision = data.frame(date=index(decision), coredata(decision))
  
  df['decision'] = decision[-1]
  
  # df['decision'] = df['decision']+2
  df = na.omit(df)
  df$decision[df$decision==-1] <- 'short USD'
  df$decision[df$decision==1] <- 'long USD'
  
  
  df[,'log_return'] = NA
  
  # toal returns each trading day. 1+r
  for (i in 1:nrow(df)) {
    df[,'log_return'][[i]] = prod(c(log_return[1:i]))
  }
  
  df[,'one'] = 'total return'
  #--------------------------------------------------------------------------
  # plots
  x <- list(
    title = "Date",
    titlefont = 'f',
    showgrid = FALSE,
    showline = TRUE
  )
  y <- list(
    title = "USD/JPY",
    titlefont = 'f',
    showline = TRUE
  )
  
  vline <- function(x = 0, color = "black") {
    list(
      type = "line", 
      y0 = min(df$Close)*0.9, 
      y1 = max(df$Close), 
      yref = "y",
      x0 = x, 
      x1 = x, 
      xref = "x",
      line = list(color = color),
      opacity = 0.2
    )
  }
  
  ay <- list(
    tickfont = list(color = "grey"),
    overlaying = "y",
    side = "right",
    title = "total return",
    showgrid = F,
    showline = TRUE
    #gridcolor = "grey"
  )
  
  
  # annotation location 
  train_period_len = length( dataframe$Close[GP_hyperparam[['training period']]] )
  period_len  = length(dataframe$Close[period])
  
  
  p = plot_ly(df, x = ~date, y= ~Close, type = "scatter", color=~decision, 
          mode= 'marker', marker=list( size=4 , opacity=0.6),colors = c('green','red','black')) %>% 
    
    add_trace(y = ~log_return, mode = 'lines', color = ~one, type = 'scatter', marker=list( size=2),
                              name = "total return", yaxis = "y2",opacity=0.5) %>%
      
    add_annotations(text = 'Training',  
                    x = df$date[train_period_len/2], 
                    y = min(df$Close)*1.1, 
                    showarrow=FALSE,
                    font = list(size = 100))   %>%
      
    add_annotations(text = 'Validation',  
                    x = df$date[(period_len-train_period_len)/2 + train_period_len], 
                    y = min(df$Close)*1.1, 
                    showarrow=FALSE,
                    font = list(size = 100))   %>%
      
    layout(shapes = vline(df$date[train_period_len]),
           title="Performance of a Selected Strategy", 
           xaxis = x, yaxis = y, yaxis2 = ay)  

  p

  return(p)
}

