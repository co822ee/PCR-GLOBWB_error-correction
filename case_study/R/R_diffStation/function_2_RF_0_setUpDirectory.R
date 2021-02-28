# Read the station information
stationInfo <- read.csv('../data/rawData/stationLatLon.csv') %>% 
    mutate(plotName=plotName %>% as.character())
station <- list.files('../data/preprocess/calibrated/','pcr_') %>% 
    sapply(., function(x) substr(x, 5, nchar(x)-4)) %>% as.character()
stationInfo <- stationInfo[(stationInfo$station %>% tolower)%in%station,] %>% 
    mutate(station=factor(station, levels = c('Basel', 'Cochem', 'Lobith')))
station <- stationInfo$station
# Create the outputFolder
if(!dir.exists('../data/analysis')){
    dir.create('../data/analysis')
}
if(!dir.exists('../graph')){
    dir.create('../graph')
}
outputFolder <- '../data/analysis/'
outputGraphFolder <- '../graph/'
# Create subfolders for calibrated and uncalibrated results
if(!dir.exists(paste0(outputFolder, 'result_', calibrMod, '/'))){
    paste0(outputFolder, 'result_', calibrMod) %>% dir.create()
}
if(!dir.exists(paste0(outputGraphFolder, calibrMod))){
    paste0(outputGraphFolder, calibrMod) %>% dir.create()
}
outputDir <- paste0(outputFolder,'result_', calibrMod)
outputGraphDir <- paste0(outputGraphFolder, calibrMod)

