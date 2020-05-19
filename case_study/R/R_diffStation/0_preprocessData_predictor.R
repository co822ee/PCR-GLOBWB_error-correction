# preprocess state variable and input variables
source('function_0_loadLibrary.R')
# calibrMod <- 'uncalibrated'      #calibrated
filePath <- paste0('../data/preprocess/', calibrMod,'/')
fileList <- list.files(filePath, pattern = 'state')
datetime <- read.csv(paste0('../data/preprocess/', calibrMod,'/q_basel.csv'))$datetime %>% 
    as.Date()

for(i in 1:length(fileList)){
    state <- read.csv(paste0(filePath, fileList[i]))[,-1]
    stationName <- substr(fileList[i], 34, nchar(fileList[i])-4)
    input <- read.csv(paste0('../data/preprocess/inputVariables/upstream_avg_input_var_1981_2000_',
                             stationName,'.csv'))[,-1]
    pred <- cbind(datetime=datetime, input, state)
    write.csv(pred, 
              paste0('../data/preprocess/', calibrMod, '/pred_',stationName, '.csv'), 
              row.names = F)
}






# file <- '../data/upstream_average_state_variables_1981_2000.csv'
# state <- read.csv(file)[,-1]
# datetime <- read.csv('../data/predictor.csv')$datetime
# datetime <- as.Date(datetime)
# str(state)
# names(state)
# state <- cbind(data.frame(datetime=datetime), state)
# 
# write.csv(state, '../data/state_variables.csv', row.names = F)

# #------------------------------------
# # Preprocess the pcr state variables
# library(ncdf4)
# path <- '../data/rawData/stateVariable/'
# ncName <- 'directRunoff_dailyTot_output_1981_2000_upstream'
# ncfName <- paste0(path,ncName,'.nc')
# # open a netCDF file
# ncin <- nc_open(ncfName)
# print(ncin)
# str(ncin)
# names(ncin)
# ncin$ndims
# ncin$nvars
# ncin[["dim"]][["record"]][["len"]]
# ncin[["dim"]][["record"]][["vals"]]
# ncin[["dim"]][["lat"]][["vals"]]
