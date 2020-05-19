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
    rf.result <- all %>% 
        mutate(mod_res=predict(optimal_ranger, all) %>% predictions()) %>% 
        mutate(pcr_corrected=pcr+mod_res)
    
    rf.eval <- rf.result %>%
        group_by(datatype) %>%
        summarise(KGE=KGE(sim = pcr, obs = obs,
                          s = c(1,1,1), na.rm = TRUE, method = "2009"),
                  KGE_corrected=KGE(sim = pcr_corrected, obs = obs,
                                    s = c(1,1,1), na.rm = TRUE, method = "2009"),
                  RMSE=(((res)^2) %>% mean(na.rm=T) %>% sqrt),
                  RMSE_corrected=(((mod_res)^2) %>% mean(na.rm=T) %>% sqrt)) %>% 
        mutate(station=stationInfo$station[station_i]) %>% 
        mutate(plotTitle=plotTitle)
    
   
    print(paste0('output csv file: ', outputFolder,'result_', calibrMod, 
                 '/repeatedcv/rf_result_',
                 station[station_i], '.csv'))
    write.csv(rf.result, paste0(outputFolder,'result_', calibrMod, 
                                '/repeatedcv/rf_result_',
                                station[station_i], '.csv'), row.names = F)
    list(rf.eval, optimal_ranger$variable.importance)
}