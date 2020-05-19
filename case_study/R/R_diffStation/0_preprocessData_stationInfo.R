source('function_0_loadLibrary.R')
library(Hmisc)

stationInfo <- read.csv('../data/rawData/stationLatLon.csv')

stationInfo <- stationInfo %>% mutate(station=station %>% as.character() %>% tolower() %>% capitalize,
                                      river=river %>% as.character() %>% tolower() %>% capitalize) %>% 
    within(., plotName <- paste(station, river, sep=' (')) %>% 
    mutate(plotName=paste0(plotName, ')'))

write.csv(stationInfo, '../data/rawData/stationLatLon.csv', row.names = F)
