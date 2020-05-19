#------------------------------------------------------------------------
#----------Find the parameter values that leads to min RMSE-------------
#-(either from OOB or cross-validation, depending on their ranges of the RMSE values)--
#------------------------------------------------------------------------

determineParam <- function(station_i){
    hyper_grid <- read.csv(paste0(outputFolder,'result_', calibrMod,
                                  '/repeatedcv/hyper_grid_10cv_',   
                                  station[station_i], '.csv'), header = T)
    a <- (hyper_grid %>%
              dplyr::arrange(OOB_RMSE))
    b <- (hyper_grid %>%
              dplyr::arrange(two.cv_RMSE))
    
    ar <- hyper_grid$OOB_RMSE %>% range() %>% round(digits = 1)
    br <- hyper_grid$two.cv_RMSE %>% range() %>% round(digits = 1)
    
    print(paste0('value range of OOB_RMSE: ', paste0(ar, collapse = ',')))
    print(paste0('value range of two.cv_RMSE: ', paste0(br, collapse = ',')))
    
    
    if((ar[2]-ar[1])<(br[2]-br[1])){
        print(hyper_grid[which.min(hyper_grid$two.cv_RMSE),])
        hyper_grid[which.min(hyper_grid$two.cv_RMSE),]
    }else{
        print(hyper_grid[which.min(hyper_grid$OOB_RMSE),])
        hyper_grid[which.min(hyper_grid$OOB_RMSE),]}
}
