
# c1 <- makeCluster(3)
# doParallel::registerDoParallel(c1)
tune_param <- function(station_i){
  calibrMod <- 'uncalibrated'      # calibrated / uncalibrated
  # whether to implement for the calibrated/uncalibrated PCR-GLOBWB model
  trainPeriod <- 1981:1990
  testPeriod <- 1991:2000
  benchmark <- T    # whether to use only driving variables as predictors in the random forests
  state_lagged <- F
  source('function_0_loadLibrary.R')
  source('function_2_RF_0_setUpDirectory.R')
  source('function_1_readData_new.R')
  
  print(station[station_i])
  source('function_2_RF_1_tuneParameter.R')
}
# parLapply(c1, seq_along(station), tune_param)
parallel::stopCluster(c1)
mclapply(seq_along(station), tune_param, mc.cores = 3)