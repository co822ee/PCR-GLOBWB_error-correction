#------------------------------------------------------------------------
#----------Find the parameter values that leads to min RMSE-------------
#-(either from OOB or cross-validation, depending on their ranges of the RMSE values)--
#------------------------------------------------------------------------

determineParam <- function(station_i){
    print(paste0('Read csv file: ', outputFolder,'result_', calibrMod,
                 '/hyper_grid_', 
                 station[station_i], '.csv'))
    hyper_grid <- read.csv(paste0(outputFolder,'result_', calibrMod,
                                  '/hyper_grid_',  
                                  station[station_i], '.csv'), header = T)
    a <- (hyper_grid %>%
              dplyr::arrange(OOB_RMSE))
    ar <- hyper_grid$OOB_RMSE %>% range() %>% round(digits =6)
    print(paste0('value range of OOB_RMSE: ', paste0(ar, collapse = ',')))
    hyper_grid[which.min(hyper_grid$OOB_RMSE),] %>% print()
}
