hyper_grid <- expand.grid(
    mtry = seq(3, length(x_varname), by=2),
    ntrees = seq(100,1000, by=200),
    OOB_RMSE = 0,
    OOB_R2 = 0,
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
    
    # add OOB error to grid
    hyper_grid$OOB_RMSE[i] <- sqrt(model$prediction.error)   #prediction.error is the mean squared error.
    hyper_grid$OOB_R2[i] <- model$r.squared
    # add test RMSE
    hyper_grid$test_RMSE[i] <- (df_test$res-(predict(model, df_test) %>% predictions()))^2 %>%
        mean() %>% sqrt()
}

if(benchmark){
    print(paste0('output csv file: ', outputDir, '/bm_hyper_grid_',
                 station[station_i], '.csv'))
    write.csv(hyper_grid, paste0(outputDir, '/bm_hyper_grid_',
                                 station[station_i], '.csv'), row.names = F)
}else{
    if(state_lagged){
        print(paste0('output csv file: ', outputDir, '/hyper_grid_',
                     station[station_i], '.csv'))
        write.csv(hyper_grid, paste0(outputDir, '/hyper_grid_',
                                     station[station_i], '.csv'), row.names = F)
    }else{
        print(paste0('output csv file: ', outputDir, '/nolag_hyper_grid_',
                     station[station_i], '.csv'))
        write.csv(hyper_grid, paste0(outputDir, '/nolag_hyper_grid_',
                                     station[station_i], '.csv'), row.names = F)
    }
}

