# print(station[station_i])
plotTitle <- stationInfo$plotName[station_i]
upstreamArea <- stationInfo$area[station_i]   #km2

ymd <- read.csv('../data/date.csv', header = T) %>% 
    mutate(datetime=as.Date(datetime))
q <- read.csv(paste0('../data/preprocess/', calibrMod, '/q_', station[station_i], '.csv')) %>% 
    mutate(datetime=as.Date(datetime)) 
q <- ((q[,-1])/upstreamArea*0.0864) %>%           # transform discharge from cms to m/day
    cbind(datetime=q$datetime, .)
    
# Read in predictor variables
if(benchmark){
    pred <- read.csv(paste0('../data/preprocess/inputVariables/met_lag10_', 
                            tolower(station[station_i]), '.csv')) %>% 
        mutate(datetime=as.Date(datetime)) %>% 
        inner_join(., ymd, by='datetime') %>% 
        mutate(datatype=ifelse(yr%in%(trainPeriod), 'train', 'test')) %>% 
        select(-yr,-m)   
}else{
    if(state_lagged){
        pred1 <- read.csv(paste0('../data/preprocess/', calibrMod, '/pred_', station[station_i], '.csv'))
        pred2 <- read.csv(paste0('../data/preprocess/inputVariables/met_lag10_', 
                                 tolower(station[station_i]), '.csv'))
        pred_all <- inner_join(pred1, pred2 %>% select(-et,-t,-p), by=c('datetime'))
        pred <- pred_all %>% 
            mutate(datetime=as.Date(datetime)) %>% 
            inner_join(., ymd, by='datetime') %>% 
            mutate(datatype=ifelse(yr%in%(trainPeriod), 'train', 'test')) %>% 
            select(-yr,-m,-channelStorage)   
    }else{
        pred <- read.csv(paste0('../data/preprocess/', calibrMod, '/pred_', station[station_i], '.csv')) %>% 
            mutate(datetime=as.Date(datetime)) %>% 
            inner_join(., ymd, by='datetime') %>% 
            mutate(datatype=ifelse(yr%in%(trainPeriod), 'train', 'test')) %>% 
            select(-yr,-m,-channelStorage)   
    }
}

# Normalized the predictors
pred <- sapply(pred %>% 
                   select(-datetime) %>% 
                   filter(datatype=='train') %>% 
                   select(-datatype), 
               function(i) (i-mean(i, na.rm=T))/sd(i, na.rm=T)) %>% 
    rbind(., sapply(pred %>% 
                        select(-datetime) %>% 
                        filter(datatype=='test') %>% 
                        select(-datatype), 
                    function(i) (i-mean(i, na.rm=T))/sd(i, na.rm=T))
          ) %>% 
    as.data.frame() %>% 
    cbind(pred %>% select(datetime),.)   

all_df <- 
    inner_join(q, pred, by="datetime") %>% 
    inner_join(., ymd %>% select(-d), by="datetime") %>% 
    mutate(datatype=ifelse(yr%in%(trainPeriod), 'train', 'test')) %>% 
    mutate(day=ymd$d)

timeVar <- c('datetime','m','yr','day')   #'d'
qVar <- c('res','obs','pcr')
x_varname <- setdiff(names(all_df), c(timeVar, qVar, 'datatype'))  
#-----------training and testing data for the model-------------
df_train <- all_df %>% filter(datatype=='train') %>% select(all_of(x_varname), 'res')
if(benchmark){
    all_df <- all_df[-(1:10), ]
    df_train <- df_train[-(1:10), ]  # remove the first 10 rows because of missing values
}else{
    if(state_lagged){
        all_df <- all_df[-(1:10), ]
        df_train <- df_train[-(1:10), ]  # remove the first 10 rows because of missing values
    }
}
df_test <- all_df %>% filter(datatype=='test') %>% select(all_of(x_varname), 'res')
print(x_varname)