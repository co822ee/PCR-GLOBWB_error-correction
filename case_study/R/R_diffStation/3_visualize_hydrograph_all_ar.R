calibrMod <- 'calibrated'      # calibrated    uncalibrated

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
#!-------------hydrograph (RF only)---------------
for(i in seq_along(csvFiles[[1]])){
    t <- readData(i)
    combine_values <- t[[1]]
    combine_values_res_real <- t[[3]]
    plotTitle <- t[[4]]
    convRatio <- t[[5]]
    
    #--------time series of streamflow (hydrograph)------------
    ts_q <- combine_values %>% gather(., key='Q',value='discharge',
                                      c('obs','RFd-lagged','RFd_s')) %>% 
        mutate(datetime=as.Date(datetime)) %>% 
        mutate(Q=factor(Q, levels = c('obs','RFd-lagged','RFd_s')))
    ts_q <- ts_q %>% mutate(Q=case_when(Q=='RFd_s' ~ ifelse(calibrMod=='calibrated',
                                                           'RFd_s','RFds'),
                                        Q=='obs'~'obs',
                                        Q=='RFd-lagged'~ifelse(calibrMod=='calibrated',
                                                        'RFd-lagged','RFd-lagged'))) %>%     #Q=='RFd' ~ ifelse(calibrMod=='calibrated', 'PCRcalibr-RFd','PCRun-RFd')
        mutate(Q=factor(Q, levels = c('obs','RFd-lagged','RFd_s')))
    p1 <- ggplot(data=ts_q %>% filter(datatype=='train'),
                 aes(x=datetime, y=discharge, col=Q))+
        geom_line(lwd=0.65, alpha=0.8)+
        facet_wrap(yr~., scale='free_x')+
        scale_x_date(date_labels = '%m', date_breaks = '1 month')+
        labs(title=paste0(plotTitle, ': train period (1981-1990)'),
             subtitle = calibrMod,
             y='flow depth (m/d)',
             x='month')+
        theme_linedraw()+
        theme(
            axis.text.y = element_text(size = 7),
            axis.title.x = element_text(size = 10),
            axis.title.y = element_text(size = 10),
            axis.text.x = element_text(size = 7),
            strip.text.x = element_text(size = 10, color = 'black'),
            strip.background = element_rect(colour = "transparent", fill = "white"),
            strip.text.y = element_text(size = 10, color = 'black'),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            title = element_text(size = 15),
            plot.subtitle = element_text(size = 12),
            legend.text = element_text(size = 12),
            legend.title = element_blank())+
        scale_colour_manual(values=c('black', 'cornflowerblue','indianred1'))+      #'#F0E442'
        scale_y_continuous(sec.axis = sec_axis(~.*convRatio, name=expression((m^{3}/s))))

    p2 <- p1%+%(ts_q %>% filter(datatype=='test'))+
        labs(title=paste0(plotTitle, ': validation period (1991-2000)'))


    print(paste0('../graph/RFresult_all_ar/timeseries_', calibrMod,
                 '/discharge_', plotTitle, '_train.tiff'))
    p1 %>% print
    ggsave(paste0('../graph/RFresult_all_ar/timeseries_', calibrMod,
                  '/discharge_', plotTitle, '_train.tiff'), dpi=300,
           width=12, height=6)

    print(paste0('../graph/RFresult_all_ar/timeseries_', calibrMod,
                 '/discharge_', plotTitle, '_test.tiff'))
    p2 %>% print
    ggsave(paste0('../graph/RFresult_all_ar/timeseries_', calibrMod,
                  '/discharge_', plotTitle, '_test.tiff'), dpi=300,
           width=12, height=6)
    
    #-----------time series of residuals----------
    ts_res <- combine_values_res_real %>% gather(., key='Q',value='discharge',
                                                 c('res','RFd-lagged','RFd_s')) %>%
        mutate(datetime=as.Date(datetime)) %>%
        mutate(Q=factor(Q, levels = c('res','RFd-lagged','RFd_s')))
    ts_res <- ts_res %>% mutate(Q=case_when(Q=='RFd_s' ~ ifelse(calibrMod=='calibrated',
                                                               'RFd_s','RFd_s'),
                                            Q=='RFd-lagged' ~ ifelse(calibrMod=='calibrated',
                                                               'RFd-lagged','RFd-lagged'),
                                            Q=='res'~ifelse(calibrMod=='calibrated',
                                                            'PCRcalibr','PCRun')))       #Q=='RFd' ~ ifelse(calibrMod=='calibrated', 'PCRcalibr-RFd','PCRun-RFd')
    
    p1%+%(ts_res %>% filter(datatype=='train'))+
        labs(title=paste0(plotTitle, ': train period (1981-1990)'),
             y='residual (m/d)')+
        geom_abline(intercept = 0, slope = 0, color='grey',lty=2)+
        scale_colour_manual(values=c('black','cornflowerblue','indianred1'))+
        scale_y_continuous(sec.axis = sec_axis(~.*convRatio, name=expression((m^{3}/s))))
    ggsave(paste0('../graph/RFresult_all_ar/timeseries_', calibrMod,
                  '/res_', plotTitle, '_train.tiff'), dpi=300,
           width=12, height=6)
    
    p1%+%(ts_res %>% filter(datatype=='test'))+
        labs(title=paste0(plotTitle, ': validation period (1991-2000)'),
             y='residual (m/d)')+
        scale_colour_manual(values=c('black','cornflowerblue','indianred1'))+
        geom_abline(intercept = 0, slope = 0, color='grey',lty=2)+
        scale_y_continuous(sec.axis = sec_axis(~.*convRatio, name=expression((m^{3}/s))))
    ggsave(paste0('../graph/RFresult_all_ar/timeseries_', calibrMod,
                  '/res_', plotTitle, '_test.tiff'), dpi=300,
           width=12, height=6)
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


#!------ partial time series--------
for(i in seq_along(csvFiles[[1]])){
    t <- readData(i)
    combine_values <- t[[1]]
    combine_values_res_real <- t[[3]]
    plotTitle <- t[[4]]
    convRatio <- t[[5]]
    
    ts_q <- combine_values %>% gather(., key='Q',value='discharge',
                               c('obs','pcr','RFd_s')) %>% 
        mutate(datetime=as.Date(datetime)) %>% 
        mutate(Q=factor(Q, levels = c('obs','pcr','RFd_s')))
    ts_q <- ts_q %>% mutate(Q=case_when(Q=='RFd_s' ~ ifelse(calibrMod=='calibrated',
                                                           'calibratedPCR RFd_s','uncalibratedPCR RFd_s'),
                                        Q=='obs'~'obs',
                                        Q=='pcr'~ifelse(calibrMod=='calibrated',
                                                        'calibratedPCR','uncalibratedPCR')))
    if(calibrMod=='calibrated'){
        ts_q$Q <- factor(ts_q$Q, levels = c('obs','calibratedPCR','calibratedPCR RFd_s'))
    }else{
        factor(ts_q$Q, levels = c('obs','uncalibratedPCR','uncalibratedPCR RFd_s'))
    }
    
    #------partial time series of discharge------
    test_p1 <- ggplot(data=ts_q %>% filter(datatype=='test', yr%in%(1991:1992)),
                      aes(x=datetime, y=discharge, col=Q))+
        geom_line(lwd=0.8)+
        scale_x_date(date_labels = '%Y-%m', 
                     date_breaks = '3 month',
                     expand = c(0,0))+   #, date_breaks = '3 month'
        
        geom_vline(
            xintercept=as.numeric((combine_values %>%
                                       filter(grepl('-05-01', datetime)))$datetime), 
            lty=2, color='grey')+
        geom_vline(
            xintercept=as.numeric((combine_values %>%
                                       filter(grepl('-10-01', datetime)))$datetime), 
            lty=2, color='grey')+
        labs(title=paste0(plotTitle), 
             subtitle = paste0(calibrMod,' (validation period)'),
             y='flow depth (m/d)',
             x='month')+
        theme_linedraw()+
        theme(
            axis.text.y = element_text(size = 14),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 14),
            axis.text.x = element_text(size = 14),
            strip.text.x = element_text(size = 10, color = 'black'),
            strip.background = element_rect(colour = "transparent", fill = "white"),
            strip.text.y = element_text(size = 10, color = 'black'),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            title = element_text(size = 15),
            plot.subtitle = element_text(size = 14),
            plot.margin = margin(0.5,0.2,0.1,0.1,"cm"),
            legend.background = element_rect(fill='transparent',color='transparent'),
            legend.justification=c(1,0), legend.position=c(1,0.99),
            legend.margin = margin(r=0.1, unit="cm"),
            legend.key = element_rect(colour = 'transparent', fill = 'transparent'),
            legend.title=element_blank(),
            legend.text = element_text(size = 14))+
        lims(y=range(ts_q %>% filter(yr%in%(1991:1994)) %>% select(discharge)))+
        scale_y_continuous(sec.axis = sec_axis(~.*convRatio, name=expression((m^{3}/s))))+
        scale_colour_manual(values=c('black', 'cornflowerblue','indianred1'))
    
    testp2 <- test_p1%+%(ts_q %>% filter(datatype=='test', yr%in%(1993:1994)))+
        theme(title = element_blank(),
              legend.position = 'none',
              plot.margin = margin(0.1,0.2,0.1,0.1,"cm"))+
        lims(y=range(ts_q %>% filter(yr%in%(1991:1994)) %>% select(discharge)))+
        scale_y_continuous(sec.axis = sec_axis(~.*convRatio, name=expression((m^{3}/s))))

    trainp1 <- test_p1%+%(ts_q %>% filter(datatype=='train', yr%in%(1987:1988)))+
        theme(plot.margin = margin(0.5,0.2,0.1,0.1,"cm"))+
        labs(subtitle = paste0(calibrMod,' (train period)'))+
        lims(y=range(ts_q %>% filter(yr%in%(1987:1990)) %>% select(discharge)))+
        scale_y_continuous(sec.axis = sec_axis(~.*convRatio, name=expression((m^{3}/s))))

    
    trainp2 <- trainp1%+%(ts_q %>% filter(datatype=='train', yr%in%(1989:1990)))+
        theme(title = element_blank(),
              legend.position = 'none',
              plot.margin = margin(0.1,0.2,0.1,0.1,"cm"))+
        lims(y=range(ts_q %>% filter(yr%in%(1987:1990)) %>% select(discharge)))+
        scale_y_continuous(sec.axis = sec_axis(~.*convRatio, name=expression((m^{3}/s))))


    
    #--------partial time series of residuals------------
    ts_res <- combine_values_res_real %>% gather(., key='Q',value='discharge',
                                          c('res','RFd_s')) %>%
        mutate(datetime=as.Date(datetime)) %>%
        mutate(Q=factor(Q, levels = c('res','RFd_s')))
    
    testres_p1 <- test_p1%+%(ts_res %>% filter(datatype=='test', yr%in%(1991:1992)))+
        theme(plot.margin = margin(0.5,0.2,0.1,0.1,"cm"))+
        geom_abline(intercept = 0, slope = 0, color='grey',lty=2)+
        lims(y=range(ts_res %>% filter(yr%in%(1991:1994)) %>% select(discharge)))+
        labs(y='residual (m/d)')+
        scale_colour_manual(values=c('cornflowerblue','indianred1'))+
        scale_y_continuous(sec.axis = sec_axis(~.*convRatio, name=expression((m^{3}/s))))
    
    testres_p2 <- testres_p1%+%(ts_res %>% filter(datatype=='test', yr%in%(1993:1994)))+
        theme(title = element_blank(),
              legend.position = 'none',
              plot.margin = margin(0.1,0.2,0.1,0.1,"cm"))+
        geom_abline(intercept = 0, slope = 0, color='grey',lty=2)+
        lims(y=range(ts_res %>% filter(yr%in%(1991:1994)) %>% select(discharge)))+
        scale_colour_manual(values=c('cornflowerblue','indianred1'))+
        scale_y_continuous(sec.axis = sec_axis(~.*convRatio, name=expression((m^{3}/s))))

    
    trainres_p1 <- test_p1%+%(ts_res %>% filter(datatype=='train', yr%in%(1987:1988)))+
        theme(plot.margin = margin(0.5,0.2,0.1,0.1,"cm"))+
        labs(subtitle = paste0(calibrMod,' (train period)'),
             y='residual (m/d)')+
        lims(y=range(ts_res %>% filter(yr%in%(1987:1990)) %>% select(discharge)))+
        scale_colour_manual(values=c('cornflowerblue','indianred1'))+
        scale_y_continuous(sec.axis = sec_axis(~.*convRatio, name=expression((m^{3}/s))))

    
    trainres_p2 <- trainres_p1%+%(ts_res %>% filter(datatype=='train', yr%in%(1989:1990)))+
        theme(title = element_blank(),
              plot.subtitle = element_blank(),
              legend.position = 'none',
              plot.margin = margin(0.1,0.2,0.1,0.1,"cm"))+
        lims(y=range(ts_res %>% filter(yr%in%(1987:1990)) %>% select(discharge)))+
        scale_colour_manual(values=c('cornflowerblue','indianred1'))+
        scale_y_continuous(sec.axis = sec_axis(~.*convRatio, name=expression((m^{3}/s))))

    
    #-----putting discharge and res together--------
    tiff(paste0('../graph/RFresult_all_ar/timeseries_', calibrMod,
                '/F_QandRes_', plotTitle, '_test.tiff'), res = 300, units = 'in',
         width=12, height=10)
    grid.arrange(test_p1+labs(
                              subtitle = '(a) flow depth (1991-1992)'), 
                 testres_p1+labs(subtitle = '(b) residuals (1991-1992)')+
                     theme(title=element_blank(), 
                                  legend.position = 'none'),
                 testp2+labs(title = '',
                             subtitle = '(c) flow depth (1993-1994)'), 
                 testres_p2+labs(title = '',
                                 subtitle = '(d) residuals (1993-1994)'), ncol=1)     #1991-1992
    dev.off()
    
    
}



#!---------scatterplot of predictions vs obs----------
temp <- vector('list',length(csvFiles[[1]]))
# only validation period and show all the locations in one graph
for(i in seq_along(csvFiles[[1]])){
    t <- readData(i)
    combine_values <- t[[1]]
    plotTitle <- t[[4]]

    
    ts_res <- combine_values %>% gather(., key='prediction',value='value',
                                 c('pcr','RFds','RFd')) %>% 
        mutate(datetime=as.Date(datetime)) %>% 
        mutate(prediction=factor(prediction, levels = c('pcr','RFd','RFds')))
    temp[[i]] <- ts_res
    temp[[i]]$plotTitle <- plotTitle

}

test <- do.call(rbind, temp)
r <- c(c(range(test$value)[1], range(test$obs)[1]) %>% min,
       c(range(test$value)[2], range(test$obs)[2]) %>% max)
pcrCode <- ifelse(calibrMod=='uncalibrated', 'PCRun', 'PCRcalibr')
plotDF <- test %>% 
    filter(datatype=='test') %>% 
    mutate(prediction=case_when(prediction=='pcr'~pcrCode,
                                prediction=='RFd'~paste0(pcrCode, '-', prediction),
                                prediction=='RFds'~paste0(pcrCode, '-', prediction)))
    
ggplot(plotDF,
       aes(x=obs, y=value))+
    geom_point(alpha=0.3, color='black')+
    facet_grid(prediction~plotTitle)+
    stat_poly_eq(formula = y~x,
                 aes(label = paste(..rr.label..)),
                 parse = TRUE, size=4, hjust = 0, vjust = 0.85) +
    geom_abline(intercept=0, slope=1, lty=2, lwd=0.8,
                color='dark grey')+
    lims(x=r, y=r)+
    theme_linedraw()+
    theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(size = 13),
        # axis.text.y = element_blank(),
        axis.title = element_text(size = 13),
        axis.text.x = element_text(size = 13),
        strip.text.x = element_text(size = 13, color = 'black'),
        strip.background = element_rect(colour = "black", fill = "transparent"),
        strip.text.y = element_text(size = 13, color = 'black'),
        panel.spacing=unit(1, "lines"),
        title = element_text(size = 15),
    )+
    labs(x='observed streamflow (m/d)', y='simulated streamflow (m/d)',
         subtitle = calibrMod)
ggsave(paste0('../graph/RFresult_all_ar/', 
              'scatterplot_predVSobs_',calibrMod, '_all.tiff'), dpi=300, 
       width=9, height=7)


