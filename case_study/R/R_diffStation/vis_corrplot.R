library(corrplot)
calibrMod <- 'uncalibrated'      # calibrated    uncalibrated

source('function_0_loadLibrary.R')
dir <- paste0('../data/analysis/', 'result_', calibrMod,'/')

files <- sapply(dir, list.files, pattern='rf_result')

stationInfo <- read.csv('../data/rawData/stationLatLon.csv')

csvFiles <- paste0(dir, files)
df_list <- lapply(csvFiles, read.csv)
i <- 3
df_1 <- df_list[[i]]
corrplot(cor(df_1 %>% select(-datetime, -d, -m, -yr, -datatype, -day)), 
         type = "lower", method = "pie", tl.cex = 0.9, 
         title=paste0(c("(a) ", "(b) ", "(c) ")[i], stationInfo[i,]$station),
         mar=c(0,0,2,0))

