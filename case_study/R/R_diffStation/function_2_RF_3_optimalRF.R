optimalRF <- function(optParam, station_i){
    
    optimal_ranger <- ranger(
        formula         = res ~ ., 
        data            = df_train, 
        num.trees       = optParam[station_i,]$ntrees,
        mtry            = optParam[station_i,]$mtry,
        min.node.size   = 5,
        seed = 123,
        importance = 'impurity'          # 'permutation'
    )
    rf.result <- all_df %>% 
        mutate(mod_res=predict(optimal_ranger, all_df) %>% predictions()) %>% 
        mutate(pcr_corrected=pcr+mod_res)
    
    rf.eval <- rf.result %>%
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
    
    # rf.eval_r <- rf.result %>% 
    #     group_by(datatype) %>% 
    #     summarise(KGE=KGE(sim = pcr, obs = obs,
    #                       s = c(1,1,1), na.rm = TRUE, method = "2009"),
    #               KGE_corrected=KGE(sim = pcr_corrected, obs = obs,
    #                                 s = c(1,1,1), na.rm = TRUE, method = "2009"),
    #               NSE = NSE(sim = pcr, obs = obs, 
    #                                   na.rm = T),
    #               NSE_corrected = NSE(sim = pcr_corrected, obs = obs, 
    #                         na.rm = T),
    #               nRMSE=(((res)^2) %>% mean(na.rm=T) %>% sqrt)/mean(obs),
    #               nRMSE_corrected=(((mod_res)^2) %>% mean(na.rm=T) %>% sqrt)/mean(obs),
    #               nMAE=(res %>% abs %>% mean(na.rm=T))/mean(obs),
    #               nMAE_corrected=(mod_res %>% abs %>% mean(na.rm=T))/mean(obs),
    #               Rsquared=(lm(pcr~obs) %>% summary)$adj.r.squared,
    #               Rsquared_corrected=(lm(pcr_corrected~obs) %>% summary)$adj.r.squared) %>% 
    #     mutate(station=stationInfo$station[station_i]) %>% 
    #     mutate(plotTitle=plotTitle)
    if(benchmark){
        print(paste0('output csv file:', outputDir, '/bm_rf_result_',
                     station[station_i], '.csv'))
        write.csv(rf.result, paste0(outputDir, '/bm_rf_result_',
                                    station[station_i], '.csv'), row.names = F)
    }else{
        if(state_lagged){
            print(paste0('output csv file:', outputDir, '/rf_result_',
                         station[station_i], '.csv'))
            write.csv(rf.result, paste0(outputDir, '/rf_result_',
                                        station[station_i], '.csv'), row.names = F)
        }else{
            print(paste0('output csv file:', outputDir, '/nolag_rf_result_',
                         station[station_i], '.csv'))
            write.csv(rf.result, paste0(outputDir, '/nolag_rf_result_',
                                        station[station_i], '.csv'), row.names = F)
        }
    }
   
    list(rf.eval, optimal_ranger$variable.importance)
}