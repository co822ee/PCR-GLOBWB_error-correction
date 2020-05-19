source('function_1_readData_excludeChannelStorage.R')

feature <- read.csv(paste0('../data/preprocess/inputVariables/upstream_avg_input_var_1981_2000_',
                           station[station_i],'.csv'))[,-1] %>% names
df_train <- df_train %>% select(all_of(feature), 'res')
df_test <- df_test %>% select(all_of(feature), 'res')
pred <- read.csv(paste0('../data/preprocess/inputVariables/upstream_avg_input_var_1981_2000_',
                        station[station_i],'.csv'))[,-1]