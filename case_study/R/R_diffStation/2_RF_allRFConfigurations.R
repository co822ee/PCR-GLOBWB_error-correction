# input:
calibrMod <- 'uncalibrated'      # calibrated / uncalibrated    
                               # whether to implement for the calibrated/uncalibrated PCR-GLOBWB model 
trainPeriod <- 1981:1990
testPeriod <- 1991:2000
benchmark <- F    # whether to include state variables as predictors in the random forests 
source('function_0_loadLibrary.R')
source('function_2_RF_0_setUpDirectory.R')

optParam <- matrix(NA, nrow=length(station), ncol=5)   # there are 5 columns returned by determineParam function.

#--------------RF---------------
# self-note: this script comes from the 2_RF_excludeChannelStorage.R (20200511).
#-----------1. Tune parameter---------------
for(station_i in seq_along(station)){
    source(paste0('function_1_readData_excludeChannelStorage', R_B_end))
    print(station[station_i])
    source('function_2_RF_1_tuneParameter.R')
}

#------------2. Determine optimal parameter----------
# call function determineParam(): 
# Determine the optimal parameter based on the min OOB RMSE 
source('function_2_RF_2_determineParameter.R')    
for(station_i in seq_along(station)){
    print(station[station_i])
    optParam[station_i,] <- determineParam(station_i) %>% as.numeric()
    
    if(station_i==length(station)){
        optParam <- optParam %>% as.data.frame()
        names(optParam) <- determineParam(station_i) %>% names()
        row.names(optParam) <- station
    }
}
optParam

#-------3. optimal_ranger----------
source('function_2_RF_3_optimalRF.R')
for(station_i in seq_along(station)){
    source(paste0('function_1_readData_excludeChannelStorage', R_B_end))
    
    result <- optimalRF(optParam, station_i)
    
    if(station_i==1){
        rf.eval <- result[[1]]
        rf.eval_r <- result[[3]]
        vi <- data.frame(names=names(result[[2]]),
                         x=result[[2]] %>% as.numeric())
    }else{
        rf.eval <- result[[1]] %>% rbind(rf.eval, .)
        rf.eval_r <- result[[3]] %>% rbind(rf.eval_r, .)
        vi <- inner_join(vi, data.frame(names=names(result[[2]]),
                                        x=result[[2]] %>% as.numeric()),
                         by='names')
    }
    if(station_i==length(station)){
        names(vi)[2:ncol(vi)] <- stationInfo$station %>% as.character()
        write.csv(vi, paste0(outputFolder, 'result_', calibrMod,
                             '/variable_importance.csv'), 
                  row.names = F)
    }
}
# optParam
write.csv(rf.eval, 
          paste0(outputFolder, 'result_', calibrMod,
                 '/rf_eval.csv'), row.names = F)
write.csv(rf.eval_r, 
          paste0(outputFolder, 'result_', calibrMod,
                 '/rf_eval_r.csv'), row.names = F)

