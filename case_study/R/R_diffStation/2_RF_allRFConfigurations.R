# input:
calibrMod <- 'uncalibrated'      # calibrated / uncalibrated    
                               # whether to implement for the calibrated/uncalibrated PCR-GLOBWB model 
trainPeriod <- 1981:1990
testPeriod <- 1991:2000
benchmark <- F    # whether to use only historical and current driving variables as predictors in the random forests 
state_lagged <- T  # whether to include lagged driving variables in the random forests where driving variables and state variables are predictors
source('function_0_loadLibrary.R')
source('function_2_RF_0_setUpDirectory.R')

optParam <- matrix(NA, nrow=length(station), ncol=5)   # there are 5 columns returned by determineParam function.

#--------------RF---------------
#-----------1. Tune parameter---------------
# for(station_i in seq_along(station)){
#     source('function_1_readData_new.R')
#     print(station[station_i])
#     source('function_2_RF_1_tuneParameter.R')   
# }
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
    source('function_1_readData_new.R')
    result <- optimalRF(optParam, station_i)
    
    if(station_i==1){
        rf.eval <- result[[1]]
        # rf.eval_r <- result[[3]]
        vi_df <- data.frame(names=names(result[[2]]),
                         x=result[[2]] %>% as.numeric())
    }else{
        rf.eval <- result[[1]] %>% rbind(rf.eval, .)
        # rf.eval_r <- result[[3]] %>% rbind(rf.eval_r, .)
        vi_df <- inner_join(vi_df, data.frame(names=names(result[[2]]),
                                        x=result[[2]] %>% as.numeric()),
                         by='names')
    }
}
names(vi_df)[2:ncol(vi_df)] <- stationInfo$station %>% as.character()
if(benchmark){
    write.csv(vi_df, paste0(outputDir, '/bm_variable_importance.csv'), row.names = F)
    write.csv(rf.eval, paste0(outputDir, '/bm_rf_eval.csv'), row.names = F)
    # write.csv(rf.eval_r, paste0(outputDir, '/bm_rf_eval_r.csv'), row.names = F)
}else{
    if(state_lagged){
        write.csv(vi_df, paste0(outputDir, '/variable_importance.csv'), row.names = F)
        write.csv(rf.eval, paste0(outputDir, '/rf_eval.csv'), row.names = F)
        # write.csv(rf.eval_r, paste0(outputDir, '/rf_eval_r.csv'), row.names = F)
    }else{
        write.csv(vi_df, paste0(outputDir, '/nolag_variable_importance.csv'), row.names = F)
        write.csv(rf.eval, paste0(outputDir, '/nolag_rf_eval.csv'), row.names = F)
        # write.csv(rf.eval_r, paste0(outputDir, '/nolag_rf_eval_r.csv'), row.names = F)
    }
}


