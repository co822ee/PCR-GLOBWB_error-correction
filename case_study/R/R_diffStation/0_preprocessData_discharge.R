# This script is for cleaning the pcr discharge predictions extracted from the netCDF.
library(dplyr)
# calibrMod <- 'uncalibrated'      #calibrated
filePath <- paste0('../data/preprocess/', calibrMod,'/')
fileList <- list.files(filePath, pattern = 'pcr')
startDate <- '1981-01-01'
endDate <- '2000-12-31'


for(i in 1:length(fileList)){
    pcr <- read.csv(paste0(filePath, fileList[i]))[,-1]
    stationName <- substr(fileList[i], 5, nchar(fileList[i])-4)
    pcr_new <- pcr[which(pcr$datetime==startDate):which(pcr$datetime==endDate),] %>% 
        mutate(datetime=as.Date(datetime)) %>% 
        rename('pcr'='discharge')
    if(stationName=='borgharen'){
        obs <- read.csv(paste0('../data/rawData/Obs-Discharge/Discharge_', 
                               stationName, '.csv')) %>% mutate(datetime=as.Date(datetime, '%m/%d/%Y'))
    }else{
        obs <- read.csv(paste0('../data/rawData/Obs-Discharge/Discharge_', 
                               stationName, '.csv'))
        
    }
    
    obs_new <- obs[which(obs$datetime==startDate):which(obs$datetime==endDate),] %>% 
        mutate(datetime=as.Date(datetime)) %>% 
        rename('obs'='discharge')
    
    q <- inner_join(obs_new, pcr_new, by='datetime') %>% 
        mutate(res=obs-pcr)
    
    print(stationName)
    write.csv(q, paste0('../data/preprocess/', calibrMod,'/q_', stationName, '.csv'),
              row.names = F)
}
