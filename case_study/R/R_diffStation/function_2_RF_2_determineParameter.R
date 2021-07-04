#------------------------------------------------------------------------
#----------Find the parameter values that leads to min RMSE-------------
#-(either from OOB or cross-validation, depending on their ranges of the RMSE values)--
#------------------------------------------------------------------------

determineParam <- function(station_i){

    if(benchmark){
        print(paste0('Read csv file: ', outputDir, '/bm_hyper_grid_', 
                     station[station_i], '.csv'))
        hyper_grid <- read.csv(paste0(outputDir, '/bm_hyper_grid_',  
                                      station[station_i], '.csv'), header = T)
    }else{
        if(state_lagged){
            print(paste0('Read csv file: ', outputDir, '/hyper_grid_', 
                         station[station_i], '.csv'))
            hyper_grid <- read.csv(paste0(outputDir, '/hyper_grid_',  
                                          station[station_i], '.csv'), header = T)
        }else{
            print(paste0('Read csv file: ', outputDir, '/nolag_hyper_grid_', 
                         station[station_i], '.csv'))
            hyper_grid <- read.csv(paste0(outputDir, '/nolag_hyper_grid_',  
                                          station[station_i], '.csv'), header = T)
        }
    }
    
    a <- (hyper_grid %>%
              dplyr::arrange(OOB_RMSE))
    ar <- (hyper_grid$OOB_RMSE*stationInfo$area[station_i]/0.0864) %>% range() %>% round(digits =6)
    print(paste0('value range of OOB_RMSE: ', paste0(ar, collapse = ',')))
    hyper_grid[which.min(hyper_grid$OOB_RMSE),] %>% print()
}
