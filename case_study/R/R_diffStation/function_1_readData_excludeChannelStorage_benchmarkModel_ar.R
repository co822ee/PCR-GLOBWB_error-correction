source('self-explanatory/function_1_readData_excludeChannelStorage_ar.R')
#calibrated and uncalibrated are the same for driving forces
pred <- read.csv(paste0('../data/preprocess_ar/', calibrMod, 
                                 '/pred_',station[station_i], '.csv'))
pred <- pred[,-c(1,5:22)]
feature <- names(pred)
df_train <- df_train %>% select(all_of(feature), 'res')
df_test <- df_test %>% select(all_of(feature), 'res')
# df_train <- df_train[-(1:10),]
