# input:
calibrMod <- 'calibrated'      # calibrated    uncalibrated
# station_i <- 1                         # station id
trainPeriod <- 1981:1990
testPeriod <- 1991:2000
# plotTitle <- stationInfo$plotName[station_i]
repeatCV <- 3
repeatedCV <- F # whether repeated two-fold cv
benchmark <- F  # benchmark model or not


source('function_0_loadLibrary.R')
source('function_2_RF_0_setUpDirectory.R')

optParam <- matrix(NA, nrow=length(station), ncol=7)   # there are 7 columns returned by determineParam function.

#--------------RF---------------
# self-note: this script comes from the 2_RF_excludeChannelStorage.R (20200511).
#-----------1. Tune parameter---------------
for(station_i in seq_along(station)){
    source(paste0('function_1_readData_excludeChannelStorage', R_B_end))
    print(station[station_i])

    
    source(paste0('function_2_RF_1_tuneParameter', R_end))
}

#------------2. Determine optimal parameter----------
# call function determineParam(): 
# Determine the optimal parameter based on either the min OOB RMSE 
# or the min cv errors:
source(paste0('function_2_RF_2_determineParameter', R_end))      # R_end is intended to differentiate the benchmark model.
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
source(paste0('function_2_RF_3_optimalRF', R_end))
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
        if(repeatedCV){
            write.csv(vi, paste0(outputFolder, 'result_', calibrMod,
                                 '/repeatedcv/variable_importance.csv'), 
                      row.names = F)
        }else write.csv(vi, paste0(outputFolder, 'result_', calibrMod,
                                   '/variable_importance.csv'), 
                        row.names = F)
    }
}
# optParam
if(repeatedCV){
    write.csv(rf.eval, 
              paste0(outputFolder, 'result_', calibrMod,
                     '/repeatedcv/rf_eval.csv'), row.names = F)
    write.csv(rf.eval_r, 
              paste0(outputFolder, 'result_', calibrMod,
                     '/repeatedcv/rf_eval_r.csv'), row.names = F)
}else{
    write.csv(rf.eval, 
              paste0(outputFolder, 'result_', calibrMod,
                     '/rf_eval.csv'), row.names = F)
    write.csv(rf.eval_r, 
              paste0(outputFolder, 'result_', calibrMod,
                     '/rf_eval_r.csv'), row.names = F)
}

