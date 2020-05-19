# calibrMod <- 'calibrated'      # calibrated    uncalibrated
station_i <- 1                         # station id
trainPeriod <- 1981:1990
testPeriod <- 1991:2000
source('function_0_loadLibrary.R')
stationInfo <- read.csv('../data/rawData/stationLatLon.csv')
station <- stationInfo$station %>% tolower

q <- read.csv(paste0('../data/preprocess/', calibrMod, '/q_', station[station_i], '.csv')) %>% 
    mutate(datetime=as.Date(datetime))

date_list <- q$datetime %>% as.character() %>% 
    strsplit(., split = '-')
date_list <- lapply(date_list, as.numeric)
date <- date_list %>% do.call(rbind, .)
date <- as.data.frame(date)           # this can be used already!
names(date) <- c('yr','m','d')
str(date)
for(i in 1:nrow(date)){
    temp <- date$yr[i]
    temp2 <- date$yr[i+1]
    if(i==1){
        count=1
    }
    if(temp!=temp2&i!=nrow(date)){
        date$d[i] <- count
        count=1
    }else{
        date$d[i] <- count
        count=count+1
    }
}
date <- date %>% cbind(datetime=q$datetime, .)
write.csv(date, '../data/date.csv', row.names = F)
