hyper_grid <- expand.grid(
    mtry = seq(3, length(feature), by=1),
    ntrees = seq(100,1000, by=200),
    OOB_RMSE = 0,
    OOB_R2 = 0,
    two.cv_RMSE = 0,
    two.cv_R2 = 0,
    test_RMSE = 0
)

for(i in 1:nrow(hyper_grid)){
    model <- ranger(
        formula = res~.,
        data = df_train,
        # num.trees = 500,
        num.trees = hyper_grid$ntrees[i],
        mtry = hyper_grid$mtry[i],
        seed = 123                        #keep the bootstrapping samples the same
    )
    two.cv_model <- holdoutRF(
        formula = res~.,
        data = df_train,
        # num.trees = 500,
        num.trees = hyper_grid$ntrees[i],
        mtry = hyper_grid$mtry[i],
        seed = 123                        #keep the bootstrapping samples the same
    )

    # add OOB error to grid
    hyper_grid$OOB_RMSE[i] <- sqrt(model$prediction.error)   #prediction.error is the mean squared error.
    hyper_grid$OOB_R2[i] <- model$r.squared
    # add cross-validation error to grid
    hyper_grid$two.cv_RMSE[i] <- sqrt(two.cv_model$rf2$prediction.error)
    hyper_grid$two.cv_R2[i] <- two.cv_model$rf2$r.squared
    # add test RMSE
    hyper_grid$test_RMSE[i] <- (df_test$res-(predict(model, df_test) %>% predictions()))^2 %>%
        mean() %>% sqrt()
}

print(paste0('output csv file: ', outputFolder,'result_', calibrMod, '/hyper_grid_',
             station[station_i], '.csv'))

write.csv(hyper_grid, paste0(outputFolder,'result_', calibrMod, '/hyper_grid_',
                             station[station_i], '.csv'), row.names = F)

