calibrMod <- 'uncalibrated'      # calibrated    uncalibrated

source('function_0_loadLibrary.R')
dir <- c(
    # paste0('../data/analysis/benchmark_ar//',  'result_', calibrMod,'/'), 
         paste0('../data/analysis/', 'result_', calibrMod,'/'))

files <- lapply(dir, list.files, pattern="bm_rf_result")
files[2] <- lapply(dir, list.files, pattern="nolag_rf_result")
# files[[2]] <- files[[1]][!grepl("bm_rf_result", files[[1]])]
# files[1] <- lapply(dir, list.files, pattern='bm_rf_result')

stationInfo <- read.csv('../data/rawData/stationLatLon.csv')

csvFiles <- lapply(seq_along(files), function(i) paste0(dir, files[[i]]))

if(!dir.exists(paste0('../graph/RFresult_all_ar/timeseries_', calibrMod))){
    dir.create(paste0('../graph/RFresult_all_ar/timeseries_', calibrMod))
}
readData <- function(i){
    station <- (files[[1]][i] %>% strsplit(., '_', 3))[[1]][4] %>% sub('.csv','',.)
    plotTitle <- stationInfo$plotName[which((stationInfo$station %>% tolower())==(station %>% tolower()))]
    upstreamArea <- stationInfo$area[which((stationInfo$station %>% tolower())==(station %>% tolower()))]
    convRatio <- upstreamArea/0.0864
    
    RFd <- read.csv(csvFiles[[1]][i], header = T)
    RFds <- read.csv(csvFiles[[2]][i], header = T)
    print(paste0(calibrMod, ': ', plotTitle))
    
    RFd_corct <- RFd %>% select('pcr_corrected','datetime') %>% #mod_res
        rename("RFd-lagged"=pcr_corrected) 
    RFds_corct <- RFds %>% select('pcr_corrected','datetime') %>% #mod_res
        rename(RFd_s=pcr_corrected)
    
    combine_values <- inner_join(RFd %>% select(-c('mod_res','pcr_corrected')), 
                          RFd_corct, by='datetime') %>% inner_join(., RFds_corct, by='datetime') %>% 
        mutate(datetime=as.Date(datetime))
    
    RFd_res <- RFd %>% select('mod_res','datetime') %>% #mod_res
        rename("RFd-lagged"=mod_res) 
    RFds_res <- RFds %>% select('mod_res','datetime') %>% #mod_res
        rename(RFd_s=mod_res)
    
    combine_values_res <- inner_join(RFd %>% select(-c('mod_res','pcr_corrected')), 
                              RFd_res, by='datetime') %>% inner_join(., RFds_res, by='datetime') %>% 
        mutate(datetime=as.Date(datetime))
    combine_values_res_real <- combine_values %>% rename(a="RFd-lagged") %>% 
        mutate("RFd-lagged"=obs-a, RFd_s=obs-RFd_s) %>% select(-a)
    list(combine_values, combine_values_res, combine_values_res_real, 
         plotTitle, convRatio)
}
#!-------------CDF (RF only)---------------
p2_list <- vector("list", length=3)
for(i in seq_along(csvFiles[[1]])){
    t <- readData(i)
    combine_values <- t[[1]]
    combine_values_res_real <- t[[3]]
    plotTitle <- t[[4]]
    convRatio <- t[[5]]
    
    ts_q <- combine_values %>% gather(., key='Q',value='discharge',
                                      c('obs','RFd-lagged','RFd_s')) %>% 
        mutate(datetime=as.Date(datetime)) %>% 
        mutate(Q=factor(Q, levels = c('obs','RFd-lagged','RFd_s')))
    ts_q <- ts_q %>% mutate(Q=case_when(Q=='RFd_s' ~ ifelse(calibrMod=='calibrated',
                                                           'RFd_s','RFd_s'),
                                        Q=='obs'~'obs',
                                        Q=='RFd-lagged'~ifelse(calibrMod=='calibrated',
                                                        'RFd-lagged','RFd-lagged'))) %>%     #Q=='RFd' ~ ifelse(calibrMod=='calibrated', 'PCRcalibr-RFd','PCRun-RFd')
        mutate(Q=factor(Q, levels = c('obs','RFd-lagged','RFd_s')))
    p1 <- ggplot(data=ts_q %>% filter(datatype=='train'),
                 aes(discharge, col=Q))+
        stat_ecdf()+
        scale_x_continuous(sec.axis = sec_axis(~.*convRatio, name=expression((m^{3}/s))))+
        # facet_wrap(yr~., scale='free_x')+
        # scale_x_date(date_labels = '%m', date_breaks = '1 month')+
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
        scale_colour_manual(values=c('black', 'cornflowerblue','indianred1'))      #'#F0E442'
        
    
    p2 <- p1%+%(ts_q %>% filter(datatype=='test'))+
        labs(title=paste0(plotTitle, ': validation period (1991-2000)'))
    
    
    print(paste0('../graph/RFresult_all_ar/timeseries_', calibrMod,
                 '/cdf_', plotTitle, '_train.tiff'))
    p1 %>% print
    ggsave(paste0('../graph/RFresult_all_ar/timeseries_', calibrMod,
                  '/cdf_', plotTitle, '_train.tiff'), dpi=300,
           width=6, height=5)
    
    print(paste0('../graph/RFresult_all_ar/timeseries_', calibrMod,
                 '/cdf_', plotTitle, '_test.tiff'))
    p2 %>% print
    ggsave(paste0('../graph/RFresult_all_ar/timeseries_', calibrMod,
                  '/cdf_', plotTitle, '_test.tiff'), dpi=300,
           width=6, height=5)
    p2_list[[i]] <- p2
    
}
tiff(paste0('../graph/RFresult_all_ar/timeseries_', calibrMod,
            '/cdf_all_test.tiff'), res = 300, units = 'in',
     width=8, height=10)
do.call(grid.arrange, p2_list)
dev.off()



