dataframe = read.csv("USDJPY1440.csv", header = F)
colnames(dataframe)  = c('date','time','Open','High','Low','Close','Volume') 

dataframe$datetime =  as.POSIXct(paste(dataframe$date, dataframe$time), format="%Y.%m.%d %H:%M")

dataframe = dataframe[c('datetime','High','Low','Close')]

dataframe <- as.xts(dataframe[, -1], order.by = dataframe$datetime)
#colnames(dataframe) = 'price.close'

log_return <- diff(log(dataframe$Close), lag=1)

log_return = log_return[-1]


