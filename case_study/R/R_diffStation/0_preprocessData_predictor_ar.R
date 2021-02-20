# preprocess state variable and input variables
source('function_0_loadLibrary.R')
fileList <- list.files("../data/preprocess/calibrated/", pattern = 'state')
datetime <- read.csv(paste0('../data/preprocess/calibrated/q_basel.csv'))$datetime %>% 
    as.Date()
lags <- 1:10
drive_names <- c('et','p','t')
lags_grid <- expand.grid(drive=drive_names, lag_t=lags)

for(i in 1:length(fileList)){
    stationName <- substr(fileList[i], 34, nchar(fileList[i])-4)
    input <- read.csv(paste0('../data/preprocess/inputVariables/upstream_avg_input_var_1981_2000_',
                             stationName,'.csv'))[,-1]
    pred <- cbind(datetime=datetime, input) #,state
    lags_l <- lapply(1:nrow(lags_grid), function(row_i){
        lag(pred[as.character(lags_grid$drive[row_i])], lags_grid$lag_t[row_i])
    })
    
    lags_df <- Reduce(cbind, lags_l)
    names(lags_df) <- paste0(names(lags_df), "_", lags_grid$lag_t)
    pred <- cbind(pred, lags_df)
    write.csv(pred, 
              paste0('../data/preprocess/inputVariables/met_lag10_',stationName, '.csv'), 
              row.names = F)
}

