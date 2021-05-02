source("function_0_loadLibrary.R")
library(hydroTSM)
calibrMod <- 'calibrated'
source('function_2_RF_0_setUpDirectory.R')
in_dir <- paste0("../data/analysis/result_", calibrMod)
files <- list.files(in_dir, "nolag_rf_quantile_")
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
  # Only data from the validation period is used
  test_df <- pred_quant[[station_i]] %>% filter(datatype=="test")
  fdc_df <- test_df %>% select(obs, pcr, pcr_corrected05, pcr_corrected50, pcr_corrected95)
  
  ecdf_df <- ddply(fdc_df %>%  gather(., key='Q',value='discharge',
                                      c('obs','pcr','pcr_corrected05','pcr_corrected50',
                                        'pcr_corrected95')), .(Q), summarize,
                                      discharge = unique(discharge),
                                      ecdf = ecdf(discharge)(unique(discharge)))
  bound90_df <- ecdf_df %>% spread( key='Q',value='ecdf', c('obs','pcr','pcr_corrected05','pcr_corrected50',
                                                  'pcr_corrected95'))
  p1 <- ggplot()+
    geom_line(data=ecdf_df %>% filter(Q%in%c("pcr_corrected05", "pcr_corrected95")),
                aes(x=discharge, y=ecdf, col=Q), lwd=1.2)+
    geom_line(data=ecdf_df %>% filter(Q%in%c("obs","pcr","pcr_corrected50")),
              aes(x=discharge, y=ecdf, col=Q))+
    
    
    scale_x_continuous(sec.axis = sec_axis(~.*convRatio[station_i], name=expression((m^{3}/s))))+
    labs(title=paste0(plotTitle, ': train period (1981-1990)'),
         subtitle = calibrMod,
         x='flow depth (m/d)',
         y='CDF')+
    coord_flip()+
    theme_linedraw()+
    theme(
      axis.text.y = element_text(size = 12),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      axis.text.x = element_text(size = 12),
      strip.text.x = element_text(size = 12, color = 'black'),
      strip.background = element_rect(colour = "transparent", fill = "white"),
      strip.text.y = element_text(size = 12, color = 'black'),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      title = element_text(size = 13),
      plot.subtitle = element_text(size = 12),
      legend.text = element_text(size = 12),
      legend.title = element_blank())+ 
    scale_colour_manual(values=c('black', 'cornflowerblue','darkolivegreen','red','darkgreen'))      #'#F0E442'
  return(p1)
}
pAll <- lapply(seq_along(station), plot_fdc)
tiff(paste0('../graph/RFresult_all_ar/timeseries_', calibrMod,
            '/cdf_quantile.tiff'), res = 300, units = 'in',
     width=8, height=10)
do.call(grid.arrange, pAll)
dev.off()
