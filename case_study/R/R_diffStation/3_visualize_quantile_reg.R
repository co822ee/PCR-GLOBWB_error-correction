source("function_0_loadLibrary.R")
library(hydroTSM)
calibrMod <- 'uncalibrated'
source('function_2_RF_0_setUpDirectory.R')
in_dir <- paste0("../data/analysis/result_", calibrMod)
files <- list.files(in_dir, "rf_quantile_result")
pred_quant <- lapply(paste0(in_dir, "/", files), read.csv)
# plot time series or flow duration curve
plot_fdc <- function(station_i){
  print(station[station_i] %>% as.character)
  station_name <- (files[station_i] %>% strsplit(., '_', 3))[[1]][4] %>% sub('.csv','',.)
  # plotTitle <- stationInfo$plotName[which((stationInfo$station %>% tolower())==(station %>% tolower()))]
  plotTitle <- paste0(c("(a) ", "(b) ", "(c) ")[station_i],  station_name)
  upstreamArea <- stationInfo$area[which((stationInfo$station %>% tolower())==(station %>% tolower()))]
  convRatio <- upstreamArea/0.0864 # from m/d to cms
  
  # To make negative values as zero
  if(any(pred_quant[[station_i]]$pcr_corrected05<0)) print(paste0("There are ", sum(pred_quant[[station_i]]$pcr_corrected05<0), " negative values in pcr_corrected05"))
  pred_quant[[station_i]] <- mutate(pred_quant[[station_i]], 
                                    pcr_corrected05=ifelse(pcr_corrected05<0,
                                                           0, pcr_corrected05))
  if(any(pred_quant[[station_i]]$pcr_corrected50<0)) print(paste0("There are ", sum(pred_quant[[station_i]]$pcr_corrected50<0), " negative values in pcr_corrected50"))
  pred_quant[[station_i]] <- mutate(pred_quant[[station_i]], 
                                    pcr_corrected50=ifelse(pcr_corrected50<0,
                                                           0, pcr_corrected50))
  
  test_df <- pred_quant[[station_i]] %>% filter(datatype=="test")
  fdc_df <- test_df %>% select(obs, pcr, pcr_corrected05, pcr_corrected50, pcr_corrected95)
  print(paste0('Output ', outputGraphDir, '/fdc_', station_name, '.tiff'))
  tiff(filename = paste0(outputGraphDir, '/fdc_', station_name, '.tiff'), 
       width=4, height=4, units='in', res=300)
  fdc(fdc_df, plot=T, leg.pos='bottomleft', main = plotTitle, 
      xlab='Exceedance probability', ylab='Q [m/d]')
  dev.off()
  
  fdc_df <- test_df %>% select(obs, pcr, pcr_corrected05, pcr_corrected50, pcr_corrected95)
  fdc_df <- fdc_df*convRatio
  print(paste0('Output ', outputGraphDir, '/fdc_', station_name, '_cms.tiff'))
  tiff(filename = paste0(outputGraphDir, '/fdc_', station_name, '_cms.tiff'), 
       width=4, height=4, units='in', res=300)
  p1 <- fdc(fdc_df, plot=T, leg.pos='bottomleft', main = plotTitle,
      xlab='Exceedance probability', ylab="Q [m^3/s]")
  dev.off()
  
}
lapply(seq_along(station), plot_fdc)
