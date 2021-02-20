# Read the station information
if(calibrMod=='calibrated'){
    stationInfo <- read.csv('../data/rawData/stationLatLon.csv') %>% 
        mutate(plotName=plotName %>% as.character())
    station <- list.files('../data/preprocess/calibrated/','pcr_') %>% 
        sapply(., function(x) substr(x, 5, nchar(x)-4)) %>% as.character()
    stationInfo <- stationInfo[(stationInfo$station %>% tolower)%in%station,] %>% 
        mutate(station=factor(station, levels = c('Basel', 'Cochem', 'Lobith')))
    station <- stationInfo$station
    # stationOrder <- c(1,3,2)
}else{
    # stationInfo <- read.csv('../data/rawData/stationLatLon.csv') %>% 
    #     mutate(plotName=plotName %>% as.character())
    # station <- stationInfo$station %>% tolower
    # station <- factor(station, levels = c('Basel','Maxau','Lobith','Cochem','Wuerzburg'))
    # stationOrder <- c(1,4,3,2,5)
    stationInfo <- read.csv('../data/rawData/stationLatLon.csv') %>% 
        mutate(plotName=plotName %>% as.character())
    station <- list.files('../data/preprocess/calibrated/','pcr_') %>% 
        sapply(., function(x) substr(x, 5, nchar(x)-4)) %>% as.character()
    stationInfo <- stationInfo[(stationInfo$station %>% tolower)%in%station,] %>% 
        mutate(station=factor(station, levels = c('Basel', 'Cochem', 'Lobith')))
    station <- stationInfo$station
    # stationOrder <- c(1,3,2)
}

if(benchmark){
    outputFolder <- '../data/analysis/benchmark_ar/'   
    outputGraphFolder <- '../graph/benchmark_ar/'
    R_B_end <- '_benchmarkModel.R'
}else{
    outputFolder <- '../data/analysis_ar/'
    outputGraphFolder <- '../graph_ar/'
    R_B_end <- '.R'
}

# Create the outputFolder
if(!dir.exists(outputFolder)){
    dir.create(outputFolder)
}
if(!dir.exists(outputGraphFolder)){
    dir.create(outputGraphFolder)
}
# Create subfolders for calibrated and uncalibrated results
if(!dir.exists(paste0(outputFolder, 'result_', calibrMod, '/'))){
    paste0(outputFolder, 'result_', calibrMod) %>% dir.create()
}
if(!dir.exists(paste0(outputGraphFolder, calibrMod))){
    paste0(outputGraphFolder, calibrMod) %>% dir.create()
}

