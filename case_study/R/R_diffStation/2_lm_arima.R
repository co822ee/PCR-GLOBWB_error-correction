source("function_0_loadLibrary.R")
library(nlme)
# input:
calibrMod <- 'calibrated'      # calibrated / uncalibrated    
# whether to implement for the calibrated/uncalibrated PCR-GLOBWB model 
trainPeriod <- 1981:1990
testPeriod <- 1991:2000
# For our linear regression (combined with temporal autocorrelation information), the 
# benchmark and state_lagged variables should be set in both FALSE 
benchmark <- F    # whether to use only driving variables as predictors in the random forests 
state_lagged <- F  # whether to include lagged driving variables in the random forests where driving variables and state variables are predictors
source('function_0_loadLibrary.R')
source('function_2_RF_0_setUpDirectory.R')
outputDir
ar_p <- c(3,3,3)
result.eval <- vector('list', length=length(station))

for(station_i in seq_along(station)){
    source('function_1_readData_lmar.R')
    print(station[station_i])
    df_train %>% head
    # use the time covariate in 10-year period
    df_train$t_ar <- 1:nrow(df_train)
    df_test$t_ar <- 1:nrow(df_test)
    df_train <- df_train[1:3600,]
    gls_m1 <- gls(as.formula(paste0("res~", paste(x_varname, collapse = "+"))),
                 data=df_train, correlation = corARMA(p=ar_p[station_i], q=0, form=~t_ar))  #t (time index for the data)
    # gls_m1day <- gls(as.formula(paste0("res~", paste(x_varname, collapse = "+"))),
    #               data=df_train, correlation = corARMA(p=ar_p[station_i], q=0, form=~t_ar|day))  #t (time index for the data) or grouping factor (day of the year))
    # Using day of the year as a grouping factor does not make sense, because here we want to use the information from the temporal autocorrelation, rather than the correlation between the same day/date from every year.
    
    test_pred <- predict(gls_m1, df_test) %>% as.vector()
    train_pred <- predict(gls_m1, df_train) %>% as.vector()
    # test_pred <- predict(gls_m1day, df_test) %>% as.vector()
    # train_pred <- predict(gls_m1day, df_train) %>% as.vector()
    lme.result <- all_df %>% 
      mutate(mod_res=c(train_pred, test_pred)) %>% 
      mutate(pcr_corrected=pcr+mod_res)
    
    # lme.result <- all_df %>%    #This setting is not reasonable because it d
    #   mutate(mod_res=predict(gls_m1, all_df %>% mutate(t=1:nrow(all_df))) ) %>% 
    #   mutate(pcr_corrected=pcr+mod_res)
    
    lme.eval <-  lme.result %>%
      group_by(datatype) %>%
      summarise(
        RMSE=(((res)^2) %>% mean(na.rm=T) %>% sqrt),
        RMSE_corrected=(((mod_res)^2) %>% mean(na.rm=T) %>% sqrt),
        MAE=res %>% abs %>% mean(na.rm=T),
        MAE_corrected=mod_res %>% abs %>% mean(na.rm=T),
        KGE=KGE(sim = pcr, obs = obs,
                s = c(1,1,1), na.rm = TRUE, method = "2009"),
        KGE_corrected=KGE(sim = pcr_corrected, obs = obs,
                          s = c(1,1,1), na.rm = TRUE, method = "2009"),
        NSE = NSE(sim = pcr, obs = obs, 
                  na.rm = T),
        NSE_corrected = NSE(sim = pcr_corrected, obs = obs, 
                            na.rm = T),
        nRMSE=(((res)^2) %>% mean(na.rm=T) %>% sqrt)/mean(obs),
        nRMSE_corrected=(((mod_res)^2) %>% mean(na.rm=T) %>% sqrt)/mean(obs),
        nMAE=(res %>% abs %>% mean(na.rm=T))/mean(obs),
        nMAE_corrected=(mod_res %>% abs %>% mean(na.rm=T))/mean(obs),
        Rsquared=(lm(pcr~obs) %>% summary)$adj.r.squared,
        Rsquared_corrected=(lm(pcr_corrected~obs) %>% summary)$adj.r.squared) %>% 
      mutate(station=stationInfo$station[station_i]) %>% 
      mutate(plotTitle=plotTitle)
    print(paste0('output csv file:', outputDir, '/lmarima_result_',
                 station[station_i], '.csv'))
    write.csv(lme.result, paste0(outputDir, '/lmarima_result_',
                                station[station_i], '.csv'), row.names = F)
    result.eval[[station_i]] <- lme.eval
}
for(station_i in seq_along(station)){
  write.csv(result.eval[[station_i]], paste0(outputDir, '/lmarima_eval_', station[station_i], '.csv'),
            row.names = F)
}


