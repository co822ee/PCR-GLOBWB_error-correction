calibrMod <- 'uncalibrated'      # calibrated    uncalibrated
# station_i <- 2                         # station id
trainPeriod <- 1981:1990
testPeriod <- 1991:2000
source('function_0_loadLibrary.R')

stationInfo <- read.csv('../data/rawData/stationLatLon.csv')
station <- stationInfo$station %>% tolower


for(station_i in 1:length(station)){
    plotTitle <- stationInfo$plotName[station_i]
    print(plotTitle)
    
    rf.result <- read.csv(paste0('../data/analysis/result_', calibrMod, '/rf_result_',
                                 station[station_i], '.csv'), header = T) %>% 
        mutate(datetime=as.Date(datetime)) %>% 
        rename(res_mod=mod_res)
    
    #--------time series of streamflow (hydrograph)------------
    ts_q <- rf.result %>% gather(., key='Q',value='discharge',
                                 c('obs','pcr','pcr_corrected'))
    
    p1 <- ggplot(data=ts_q %>% filter(datatype=='train'), 
                 aes(x=datetime, y=discharge, col=Q))+
        geom_line()+
        facet_wrap(yr~., scale='free')+
        scale_x_date(date_labels = '%b', date_breaks = '1 month')+
        geom_vline(
            xintercept=as.numeric((rf.result %>% 
                                       filter(grepl('-05-01', datetime)))$datetime), lty=2)+
        geom_vline(
            xintercept=as.numeric((rf.result %>% 
                                       filter(grepl('-10-01', datetime)))$datetime), lty=2)+
        labs(title=paste0(plotTitle, ': train period (1981-1990)'), 
             y='discharge (cms)')+
        theme(axis.text.x = element_text(size=7))+
        scale_colour_manual(values=c('black','red','blue'))
    
    p2 <- p1%+%(ts_q %>% filter(datatype=='test'))+
        labs(title=paste0(plotTitle, ': test period (1991-2000)'))
    p1 %>% print
    ggsave(paste0('../graph/timeseries_', calibrMod,
                  '/discharge_', plotTitle, '_train.tiff'), dpi=300, 
           width=12, height=6)
    
    p2 %>% print
    ggsave(paste0('../graph/timeseries_', calibrMod,
                  '/discharge_', plotTitle, '_test.tiff'), dpi=300, 
           width=12, height=6)
    
    #--------time series of residuals------------
    ts_res <- rf.result %>% gather(., key='residuals',value='value',
                                   c('res','res_mod'))
    
    p1 <- ggplot(data=ts_res %>% filter(datatype=='train'), 
                 aes(x=datetime, y=value, col=residuals))+
        geom_line()+
        facet_wrap(yr~., scale='free')+
        scale_x_date(date_labels = '%b', date_breaks = '1 month')+
        geom_vline(
            xintercept=as.numeric((rf.result %>% 
                                       filter(grepl('-05-01', datetime)))$datetime), lty=2)+
        geom_vline(
            xintercept=as.numeric((rf.result %>% 
                                       filter(grepl('-10-01', datetime)))$datetime), lty=2)+
        labs(title=paste0(plotTitle, ': train period (1981-1990)'), 
             y='residuals (cms)')+
        theme(axis.text.x = element_text(size=7))+
        scale_colour_manual(values=c('black','blue'))
    
    p2 <- p1%+%(ts_res %>% filter(datatype=='test'))+
        labs(title=paste0(plotTitle, ': test period (1991-2000)'))
    p1 %>% print
    ggsave(paste0('../graph/timeseries_', calibrMod,
                  '/res_', plotTitle, '_train.tiff'), dpi=300, 
           width=12, height=6)
    p2 %>% print
    ggsave(paste0('../graph/timeseries_', calibrMod,
                  '/res_', plotTitle, '_test.tiff'), dpi=300, 
           width=12, height=6)
    
    #--------time series of streamflow (combined)------------
    ts_q <- rf.result %>% gather(., key='Q',value='discharge',
                                 c('obs','pcr','pcr_corrected',
                                   'res','res_mod'))
    
    p1 <- ggplot(data=ts_q %>% filter(datatype=='train'), 
                 aes(x=datetime, y=discharge, col=Q))+
        geom_line()+
        facet_wrap(yr~., scale='free')+
        scale_x_date(date_labels = '%b', date_breaks = '1 month')+
        geom_vline(
            xintercept=as.numeric((rf.result %>% 
                                       filter(grepl('-05-01', datetime)))$datetime), lty=2)+
        geom_vline(
            xintercept=as.numeric((rf.result %>% 
                                       filter(grepl('-10-01', datetime)))$datetime), lty=2)+
        labs(title=paste0(plotTitle, ': train period (1981-1990)'), 
             y='discharge (cms)')+
        theme(axis.text.x = element_text(size=7))+
        scale_colour_manual(values=c('black','red','blue',
                                     'orange','orchid3'))
    
    p2 <- p1%+%(ts_q %>% filter(datatype=='test'))+
        labs(title=paste0(plotTitle, ': test period (1991-2000)'))
    p1 %>% print
    ggsave(paste0('../graph/timeseries_', calibrMod,
                  '/combined_', plotTitle, '_train.tiff'), dpi=300, 
           width=12, height=6)
    p2 %>% print
    ggsave(paste0('../graph/timeseries_', calibrMod,
                  '/combined_', plotTitle, '_test.tiff'), dpi=300, 
           width=12, height=6)
}

#-------------------