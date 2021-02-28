source("function_0_loadLibrary.R")
# input:
# Only uncalibrated pcr-globwb model is used 
calibrMod <- 'uncalibrated'      
trainPeriod <- 1981:1990
testPeriod <- 1991:2000
# In the RF models, we used the state variables and lagged met variables.
benchmark <- F    # whether to use only driving variables as predictors in the random forests 
state_lagged <- T  # whether to include lagged driving variables in the random forests where driving variables and state variables are predictors
source('function_2_RF_0_setUpDirectory.R')
outputDir

optParam <- matrix(NA, nrow=length(station), ncol=5)   # there are 5 columns returned by determineParam function.
#------------1. Determine optimal parameter----------
source('function_2_RF_2_determineParameter.R')    
for(station_i in seq_along(station)){
  print(station[station_i])
  source('function_1_readData_new.R')
  optParam[station_i,] <- determineParam(station_i) %>% as.numeric()
  
  if(station_i==length(station)){
    optParam <- optParam %>% as.data.frame()
    names(optParam) <- determineParam(station_i) %>% names()
    row.names(optParam) <- station
  }
}
optParam

#-------2. optimal_ranger----------
source('function_2_RF_3_optimalRF_quantile.R')
for(station_i in seq_along(station)){
  source('function_1_readData_new.R')
  optimalRF(optParam, station_i)
}


