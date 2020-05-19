# print(station[station_i])
plotTitle <- stationInfo$plotName[station_i]
upstreamArea <- stationInfo$area[station_i]   #km2

ymd <- read.csv('../data/date.csv', header = T) %>% 
    mutate(datetime=as.Date(datetime))
q <- read.csv(paste0('../data/preprocess/', calibrMod, '/q_', station[station_i], '.csv')) %>% 
    mutate(datetime=as.Date(datetime))
q <- ((q[,-1])/upstreamArea*0.0864) %>%           # transform discharge from cms to m/day
    cbind(datetime=q$datetime, .)


pred <- read.csv(paste0('../data/preprocess/', calibrMod, '/pred_', station[station_i], '.csv')) %>% 
    mutate(datetime=as.Date(datetime)) %>% 
    inner_join(., ymd, by='datetime') %>% 
    mutate(datatype=ifelse(yr%in%(trainPeriod), 'train', 'test')) %>% 
    select(-yr,-m)

    #%>% select(-channelStorage)
# Days in year is not normalized and also channelStorage is included.
# 
pred <- sapply(pred %>% 
                   select(-datetime) %>% 
                   filter(datatype=='train') %>% 
                   select(-datatype), 
               function(i) (i-mean(i))/sd(i)) %>% 
    rbind(., sapply(pred %>% 
                        select(-datetime) %>% 
                        filter(datatype=='test') %>% 
                        select(-datatype), 
                    function(i) (i-mean(i))/sd(i))
          ) %>% 
    as.data.frame() %>% 
    cbind(pred %>% select(datetime),.)    #normalized the predictors

# ymd <- q$datetime %>% 
#     as.character() %>%
#     strsplit(., split = '-') %>% 
#     lapply(., as.numeric) %>% 
#     do.call(rbind, .) %>% as.data.frame() %>% rename('yr'='V1','m'='V2','d'='V3')


all <- 
    inner_join(q, pred, by="datetime") %>% 
    inner_join(., ymd %>% select(-d), by="datetime") %>% 
    mutate(datatype=ifelse(yr%in%(trainPeriod), 'train', 'test')) %>% 
    mutate(day=ymd$yr)

timeVar <- c('datetime','m','yr','day')   #'d'
qVar <- c('res','obs','pcr')
feature <- setdiff(names(all), c(timeVar, qVar, 'datatype'))   #channelStorage

#-----------training and testing data for the model-------------
df_train <- all %>% filter(datatype=='train') %>% select(all_of(feature), 'res')
df_test <- all %>% filter(datatype=='test') %>% select(all_of(feature), 'res')
