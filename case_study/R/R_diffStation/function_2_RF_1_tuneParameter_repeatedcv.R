hyper_grid <- expand.grid(
    mtry = seq(3, length(feature), by=1),
    ntrees = seq(100,1000, by=200),
    OOB_RMSE = 0,
    OOB_R2 = 0,
    two.cv_RMSE = 0,
    two.cv_R2 = 0,
    test_RMSE = 0
)
set.seed(123)      #make sure each hyper_grid element has the same yr selected for cross-validation
yr <- sample(10, repeatCV,replace=F)
for(i in 1:nrow(hyper_grid)){
    two.cv_RMSE <- c(NA)
    two.cv_R2 <- c(NA)
    for(j in 1:repeatCV){
        if(j==1){
            model <- ranger(
                formula = res~.,
                data = df_train,
                # num.trees = 500,
                num.trees = hyper_grid$ntrees[i],
                mtry = hyper_grid$mtry[i],
                seed = 123                        #keep the bootstrapping samples the same
            )
        }
        
        df_train.cv <- df_train[which(all$yr!=(1980+yr[j])),]
        df_train.cv <- df_train.cv[!is.na(df_train.cv)] %>% 
            matrix(ncol=length(feature)+1, byrow=F) %>% 
            as.data.frame()
        names(df_train.cv) <- names(df_train)
        
        cv_model <- ranger(
            formula = res~.,
            data = df_train.cv,
            # num.trees = 500,
            num.trees = hyper_grid$ntrees[i],
            mtry = hyper_grid$mtry[i],
            seed = 123                        #keep the bootstrapping samples the same
        )
        two.cv_RMSE[j] <- (df_train[which(all$yr==(1980+yr[j])),]$res-(predict(model, df_train[which(all$yr==(1980+yr[j])),]) %>% 
                                                                           predictions()))^2 %>%
            mean() %>% sqrt()
        two.cv_R2[j] <- (cor(df_train[which(all$yr==(1980+yr[j])),]$res, predict(model, df_train[which(all$yr==(1980+yr[j])),]) %>% 
                                 predictions()))^2
    }
    
    

    # add OOB error to grid
    hyper_grid$OOB_RMSE[i] <- sqrt(model$prediction.error)   #prediction.error is the mean squared error.
    hyper_grid$OOB_R2[i] <- model$r.squared
    # add cross-validation error to grid
    hyper_grid$two.cv_RMSE[i] <- mean(two.cv_RMSE)
    hyper_grid$two.cv_R2[i] <- mean(two.cv_R2)
    # add test RMSE
    hyper_grid$test_RMSE[i] <- (df_test$res-(predict(model, df_test) %>% predictions()))^2 %>%
        mean() %>% sqrt()
}

print(paste0('output csv file: ', outputFolder,'result_', calibrMod, 
             '/repeatedcv/hyper_grid_10cv_',
             station[station_i], '.csv'))

write.csv(hyper_grid, paste0(outputFolder,'result_', calibrMod, 
                             '/repeatedcv/hyper_grid_10cv_',
                             station[station_i], '.csv'), row.names = F)

