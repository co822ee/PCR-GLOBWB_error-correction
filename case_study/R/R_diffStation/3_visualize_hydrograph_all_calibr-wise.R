# station_i <- 2                         # station id

source('function_0_loadLibrary.R')
dir <- c(paste0('../data/analysis/',  'result_calibrated/'), 
         paste0('../data/analysis/', 'result_uncalibrated/'))

calibrL <- list(T,F)

files <- lapply(dir, list.files, pattern='rf_result')
stationInfo <- read.csv('../data/rawData/stationLatLon.csv')

csvFiles <- lapply(seq_along(files), function(i) paste0(dir[i], files[[i]]))
if(!dir.exists(paste0('../graph/RFresult_all/timeseries_calibr-wise'))){
    dir.create(paste0('../graph/RFresult_all/timeseries_calibr-wise'))
}
readData <- function(i){
    station <- (files[[1]][i] %>% strsplit(., '_', 3))[[1]][3] %>% sub('.csv','',.)
    plotTitle <- stationInfo$plotName[which((stationInfo$station %>% tolower())==(station %>% tolower()))]
    upstreamArea <- stationInfo$area[which((stationInfo$station %>% tolower())==(station %>% tolower()))]
    convRatio <- upstreamArea/0.0864
    
    PCRcalibr <- read.csv(csvFiles[[1]][i], header = T)
    PCRun <- read.csv(csvFiles[[2]][i], header = T)
    print(paste0(plotTitle))
    
    PCRcalibr_p <- PCRcalibr %>% select('pcr','datetime') %>% #mod_res
        rename(PCRcalibr=pcr) 
    PCRun_p <- PCRun %>% select('pcr','datetime') %>% #mod_res
        rename(PCRun=pcr)
    
    combine <- inner_join(PCRcalibr %>% select(-c('mod_res','pcr_corrected','pcr')), 
                          PCRcalibr_p, by='datetime') %>% inner_join(., PCRun_p, by='datetime') %>% 
        mutate(datetime=as.Date(datetime))
    
    PCRcalibr_res <- PCRcalibr %>% select('res','datetime') %>% #mod_res
        rename(PCRcalibr=res) 
    PCRun_res <- PCRun %>% select('res','datetime') %>% #mod_res
        rename(PCRun=res)
    
    combine_res <- inner_join(PCRcalibr %>% select(-c('mod_res','pcr_corrected','pcr')), 
                              PCRcalibr_res, by='datetime') %>% inner_join(., PCRun_res, by='datetime') %>% 
        mutate(datetime=as.Date(datetime))
    
    list(combine, combine_res, plotTitle, convRatio)
}
#--------1 time series (all)------------
for(i in seq_along(csvFiles[[1]])){
    t <- readData(i)
    combine <- t[[1]]
    combine_res <- t[[2]]
    plotTitle <- t[[3]]
    convRatio <- t[[4]]
    #--------time series of streamflow (hydrograph)------------
    ts_q <- combine %>% gather(., key='Q',value='discharge',
                               c('obs','PCRcalibr','PCRun')) %>% 
        mutate(datetime=as.Date(datetime)) %>% 
        mutate(Q=factor(Q, levels = c('obs','PCRun','PCRcalibr')))
    
    p1 <- ggplot(data=ts_q %>% filter(datatype=='train'),
                 aes(x=datetime, y=discharge, col=Q))+
        geom_line(lwd=0.8)+
        facet_wrap(yr~., scale='free_x')+
        scale_x_date(date_labels = '%m', date_breaks = '1 month')+
        geom_vline(
            xintercept=as.numeric((combine %>%
                                       filter(grepl('-05-01', datetime)))$datetime), 
            lty=2, color='grey')+
        geom_vline(
            xintercept=as.numeric((combine %>%
                                       filter(grepl('-10-01', datetime)))$datetime), 
            lty=2, color='grey')+
        labs(title=paste0(plotTitle, ': train period (1981-1990)'),
             # subtitle = calibrMod,
             y='flow depth (m/d)',
             x='month')+
        theme_linedraw()+
        theme(
            axis.text.y = element_text(size = 7),
            # axis.text.y = element_blank(),
            axis.title.x = element_text(size = 10),
            axis.title.y = element_text(size = 10),
            axis.text.x = element_text(size = 7),
            strip.text.x = element_text(size = 10, color = 'black'),
            strip.background = element_rect(colour = "transparent", fill = "white"),
            strip.text.y = element_text(size = 10, color = 'black'),
            # panel.margin=unit(.05, "lines"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            # strip.background = element_blank(),
            # strip.text = element_blank(),
            title = element_text(size = 15),
            plot.subtitle = element_text(size = 12),
            legend.text = element_text(size = 12),
            legend.title =element_blank())+
        scale_colour_manual(values=c('black','#F0E442','#D55E00'))+
        scale_y_continuous(sec.axis = sec_axis(~.*convRatio, name=expression((m^{3}/s))))

    p2 <- p1%+%(ts_q %>% filter(datatype=='test'))+
        labs(title=paste0(plotTitle, ': test period (1991-2000)'))

    # p1%+%(ts_q %>% filter(datatype=='test', yr%in%(1997:2000)))

    print(paste0('../graph/RFresult_all/timeseries_calibr-wise',
                 '/discharge_', plotTitle, '_train.tiff'))
    p1 %>% print
    ggsave(paste0('../graph/RFresult_all/timeseries_calibr-wise',
                  '/discharge_', plotTitle, '_train.tiff'), dpi=300,
           width=12, height=6)

    print(paste0('../graph/RFresult_all/timeseries_calibr-wise',
                 '/discharge_', plotTitle, '_test.tiff'))
    p2 %>% print
    ggsave(paste0('../graph/RFresult_all/timeseries_calibr-wise',
                  '/discharge_', plotTitle, '_test.tiff'), dpi=300,
           width=12, height=6)
    #-----time series of residuals--------
    ts_res <- combine_res %>% gather(., key='Q',value='discharge',
                                          c('PCRcalibr','PCRun')) %>%
        mutate(datetime=as.Date(datetime)) %>%
        mutate(Q=factor(Q, levels = c('PCRun','PCRcalibr')))
    
    p1%+%(ts_res %>% filter(datatype=='train'))+
        labs(title=paste0(plotTitle, ': train period (1981-1990)'),
             y='residual (m/d)')+
        scale_colour_manual(values=c('#F0E442','#D55E00'))+
        geom_abline(intercept = 0, slope = 0, color='grey',lty=2)
    ggsave(paste0('../graph/RFresult_all/timeseries_calibr-wise',
                  '/res_', plotTitle, '_train.tiff'), dpi=300,
           width=12, height=6)
    
    p1%+%(ts_res %>% filter(datatype=='test'))+
        labs(title=paste0(plotTitle, ': validation period (1991-2000)'),
             y='residual (m/d)')+
        scale_colour_manual(values=c('#F0E442','#D55E00'))+
        geom_abline(intercept = 0, slope = 0, color='grey',lty=2)
    ggsave(paste0('../graph/RFresult_all/timeseries_calibr-wise',
                  '/res_', plotTitle, '_test.tiff'), dpi=300,
           width=12, height=6)
    
}
#---------2 partial time series----------
for(i in seq_along(csvFiles[[1]])){
    
    t <- readData(i)
    combine <- t[[1]]
    combine_res <- t[[2]]
    plotTitle <- t[[3]]
    
    #--------time series of streamflow (hydrograph)------------
    ts_q <- combine %>% gather(., key='Q',value='discharge',
                               c('obs','PCRcalibr','PCRun')) %>% 
        mutate(datetime=as.Date(datetime)) %>% 
        mutate(Q=factor(Q, levels = c('obs','PCRun','PCRcalibr')))
    
    r <- range(ts_q %>% filter(datatype=='test', yr%in%(1991:1994)) %>% 
                   select(discharge))
    
    #------partial time series of discharge------
    test_p1 <- ggplot(data=ts_q %>% filter(datatype=='test', yr%in%(1991:1992)),
                      aes(x=datetime, y=discharge, col=Q))+
        geom_line(lwd=0.8)+
        # facet_wrap(yr~., scale='free_x')+
        scale_x_date(date_labels = '%Y-%m', 
                     date_breaks = '3 month',
                     # breaks = c('1991-01-01','1991-05-01','1991-10-01',
                     #            '1992-05-01','1992-10-01','1992-12-31') %>% as.Date(),
                     expand = c(0,0))+   #, date_breaks = '3 month'
        
        geom_vline(
            xintercept=as.numeric((combine %>%
                                       filter(grepl('-05-01', datetime)))$datetime), 
            lty=2, color='grey')+
        geom_vline(
            xintercept=as.numeric((combine %>%
                                       filter(grepl('-10-01', datetime)))$datetime), 
            lty=2, color='grey')+
        labs(title=paste0(plotTitle), 
             subtitle = paste0('(test period)'),
             y='discharge (m/d)',
             x='month')+
        theme_linedraw()+
        theme(
            axis.text.y = element_text(size = 12),
            # axis.text.y = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 12),
            axis.text.x = element_text(size = 12),
            strip.text.x = element_text(size = 10, color = 'black'),
            strip.background = element_rect(colour = "transparent", fill = "white"),
            strip.text.y = element_text(size = 10, color = 'black'),
            # panel.margin=unit(.05, "lines"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            # strip.background = element_blank(),
            # strip.text = element_blank(),
            title = element_text(size = 15),
            plot.subtitle = element_text(size = 12),
            plot.margin = margin(0.5,1.2,0.1,0.1,"cm"),
            legend.background = element_rect(fill='transparent',color='transparent'),
            legend.justification=c(1,0), legend.position=c(1,0.99),
            legend.margin = margin(r=0.2, unit="cm"),
            legend.key = element_rect(colour = 'transparent', fill = 'transparent'),
            legend.title=element_blank(),
            legend.text = element_text(size = 12))+
        scale_colour_manual(values=c('black','#F0E442','#D55E00'))+
        lims(y=r)
        # lims(y=c(0,0.015))
    
    testp2 <- test_p1%+%(ts_q %>% filter(datatype=='test', yr%in%(1993:1994)))+
        # scale_x_date(date_labels = '%Y-%m-%d', 
        #              breaks = c('1993-01-01','1993-05-01','1993-10-01',
        #                         '1994-05-01','1994-10-01','1994-12-31') %>% as.Date(),
        #              expand = c(0,0))+
        theme(title = element_blank(),
              plot.subtitle = element_blank(),
              legend.position = 'none',
              plot.margin = margin(0.1,1.2,0.1,0.1,"cm"))
        # lims(y=c(0,0.015))
    
    tiff(paste0('../graph/RFresult_all/timeseries_calibr-wise/',
                '/F_discharge_', plotTitle, '_test.tiff'), res = 300, units = 'in',
         width=12, height=6)
    grid.arrange(test_p1, testp2)
    dev.off()
    
    
    r <- range(ts_q %>% filter(datatype=='train', yr%in%(1987:1990)) %>% 
                   select(discharge))
    
    trainp1 <- test_p1%+%(ts_q %>% filter(datatype=='train', yr%in%(1987:1988)))+
        # scale_x_date(date_labels = '%Y-%m-%d', 
        #              breaks = c('1987-01-01','1987-05-01','1987-10-01',
        #                         '1988-05-01','1988-10-01','1988-12-31') %>% as.Date(),
        #              expand = c(0,0))+
        theme(plot.margin = margin(0.5,1.2,0.1,0.1,"cm"))+
        labs(subtitle = paste0('(train period)'))+
        lims(y=r)
        # lims(y=c(0,0.015))
    
    trainp2 <- trainp1%+%(ts_q %>% filter(datatype=='train', yr%in%(1989:1990)))+
        # scale_x_date(date_labels = '%Y-%m-%d', 
        #              breaks = c('1989-01-01','1989-05-01','1989-10-01',
        #                         '1990-05-01','1990-10-01','1990-12-31') %>% as.Date(),
        #              expand = c(0,0))+
        theme(title = element_blank(),
              plot.subtitle = element_blank(),
              legend.position = 'none',
              plot.margin = margin(0.1,1.2,0.1,0.1,"cm"))+
        lims(y=r)
        # lims(y=c(0,0.015))
    
    tiff(paste0('../graph/RFresult_all/timeseries_calibr-wise',
                '/F_discharge_', plotTitle, '_train.tiff'), res = 300, units = 'in',
         width=12, height=6)
    grid.arrange(trainp1, trainp2)
    dev.off()
    
    
    #--------partial time series of residuals------------
    # ts_res <- combine_res %>% gather(., key='Q',value='discharge',
    #                                  c('PCRun','PCRcalibr')) %>%
    #     mutate(datetime=as.Date(datetime)) %>%
    #     mutate(Q=factor(Q, levels = c('PCRun','PCRcalibr')))
    # 
    # testp1 <- test_p1%+%(ts_res %>% filter(datatype=='test', yr%in%(1991:1992)))+
    #     # scale_x_date(date_labels = '%Y-%m-%d', 
    #     #              breaks = c('1991-01-01','1991-05-01','1991-10-01',
    #     #                         '1992-05-01','1992-10-01','1992-12-31') %>% as.Date(),
    #     #              expand = c(0,0))+
    #     theme(plot.margin = margin(0.5,1.2,0.1,0.1,"cm"))+
    #     scale_colour_manual(values=c('#F0E442','#D55E00'))+
    #     labs(y='residual (m/d)')+
    #     lims(y=c(-0.006,0.01))
    # 
    # testp2 <- testp1%+%(ts_res %>% filter(datatype=='test', yr%in%(1993:1994)))+
    #     # scale_x_date(date_labels = '%Y-%m-%d', 
    #     #              breaks = c('1993-01-01','1993-05-01','1993-10-01',
    #     #                         '1994-05-01','1994-10-01','1994-12-31') %>% as.Date(),
    #     #              expand = c(0,0))+
    #     theme(title = element_blank(),
    #           plot.subtitle = element_blank(),
    #           legend.position = 'none',
    #           plot.margin = margin(0.1,1.2,0.1,0.1,"cm"))+
    #     lims(y=c(-0.006,0.01))
    # 
    # tiff(paste0('../graph/RFresult_all/timeseries_calibr-wise',
    #             '/F_res_', plotTitle, '_test.tiff'), res = 300, units = 'in',
    #      width=12, height=6)
    # grid.arrange(testp1, testp2)
    # dev.off()
    # 
    # 
    # trainp1 <- test_p1%+%(ts_res %>% filter(datatype=='train', yr%in%(1987:1988)))+
    #     # scale_x_date(date_labels = '%Y-%m-%d', 
    #     #              breaks = c('1987-01-01','1987-05-01','1987-10-01',
    #     #                         '1988-05-01','1988-10-01','1988-12-31') %>% as.Date(),
    #     #              expand = c(0,0))+
    #     theme(plot.margin = margin(0.5,1.2,0.1,0.1,"cm"))+
    #     scale_colour_manual(values=c('#F0E442','#D55E00'))+
    #     labs(subtitle = paste0('(train period)'),
    #          y='residual (m/d)')+
    #     lims(y=c(-0.006,0.01))
    # 
    # trainp2 <- trainp1%+%(ts_res %>% filter(datatype=='train', yr%in%(1989:1990)))+
    #     # scale_x_date(date_labels = '%Y-%m-%d', 
    #     #              breaks = c('1989-01-01','1989-05-01','1989-10-01',
    #     #                         '1990-05-01','1990-10-01','1990-12-31') %>% as.Date(),
    #     #              expand = c(0,0),
    #     #              date_minor_breaks = "3 month")+
    #     theme(title = element_blank(),
    #           plot.subtitle = element_blank(),
    #           legend.position = 'none',
    #           plot.margin = margin(0.1,1.2,0.1,0.1,"cm"))+
    #     lims(y=c(-0.006,0.01))
    # tiff(paste0('../graph/RFresult_all/timeseries_calibr-wise',
    #             '/F_res_', plotTitle, '_train.tiff'), res = 300, units = 'in',
    #      width=12, height=6)
    # grid.arrange(trainp1, trainp2)
    # dev.off()
    
}
