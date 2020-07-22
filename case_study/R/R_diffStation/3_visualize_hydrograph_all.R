calibrMod <- 'calibrated'      # calibrated    uncalibrated
# station_i <- 2                         # station id

source('function_0_loadLibrary.R')
dir <- c(paste0('../data/analysis/benchmark/',  'result_', calibrMod,'/'), 
         paste0('../data/analysis/', 'result_', calibrMod,'/'))

files <- lapply(dir, list.files, pattern='rf_result')

stationInfo <- read.csv('../data/rawData/stationLatLon.csv')

csvFiles <- lapply(seq_along(files), function(i) paste0(dir[i], files[[i]]))
if(!dir.exists(paste0('../graph/RFresult_all/timeseries_', calibrMod))){
    dir.create(paste0('../graph/RFresult_all/timeseries_', calibrMod))
}
readData <- function(i){
    station <- (files[[1]][i] %>% strsplit(., '_', 3))[[1]][3] %>% sub('.csv','',.)
    plotTitle <- stationInfo$plotName[which((stationInfo$station %>% tolower())==(station %>% tolower()))]
    upstreamArea <- stationInfo$area[which((stationInfo$station %>% tolower())==(station %>% tolower()))]
    convRatio <- upstreamArea/0.0864
    
    RFd <- read.csv(csvFiles[[1]][i], header = T)
    RFds <- read.csv(csvFiles[[2]][i], header = T)
    print(paste0(calibrMod, ': ', plotTitle))
    
    RFd_corct <- RFd %>% select('pcr_corrected','datetime') %>% #mod_res
        rename(RFd=pcr_corrected) 
    RFds_corct <- RFds %>% select('pcr_corrected','datetime') %>% #mod_res
        rename(RFds=pcr_corrected)
    
    combine_values <- inner_join(RFd %>% select(-c('mod_res','pcr_corrected')), 
                          RFd_corct, by='datetime') %>% inner_join(., RFds_corct, by='datetime') %>% 
        mutate(datetime=as.Date(datetime))
    
    RFd_res <- RFd %>% select('mod_res','datetime') %>% #mod_res
        rename(RFd=mod_res) 
    RFds_res <- RFds %>% select('mod_res','datetime') %>% #mod_res
        rename(RFds=mod_res)
    
    combine_values_res <- inner_join(RFd %>% select(-c('mod_res','pcr_corrected')), 
                              RFd_res, by='datetime') %>% inner_join(., RFds_res, by='datetime') %>% 
        mutate(datetime=as.Date(datetime))
    
    combine_values_res_real <- combine_values %>% mutate(RFd=obs-RFd, RFds=obs-RFds)
    list(combine_values, combine_values_res, combine_values_res_real, plotTitle, 
         convRatio)
}
#!-------------hydrograph (RF only)---------------
for(i in seq_along(csvFiles[[1]])){
    t <- readData(i)
    combine_values <- t[[1]]
    # combine_values_res <- t[[2]]
    combine_values_res_real <- t[[3]]
    plotTitle <- t[[4]]
    # plotTitle <- paste0(c('(a) ','(b) ','(c) ')[i], plotTitle)
    convRatio <- t[[5]]
    
    #--------time series of streamflow (hydrograph)------------
    ts_q <- combine_values %>% gather(., key='Q',value='discharge',
                                      c('obs','RFds','pcr')) %>% 
        mutate(datetime=as.Date(datetime)) %>% 
        mutate(Q=factor(Q, levels = c('obs','pcr','RFds')))
    ts_q <- ts_q %>% mutate(Q=case_when(Q=='RFds' ~ ifelse(calibrMod=='calibrated',
                                                           'PCRcalibr-RFds','PCRun-RFds'),
                                        Q=='obs'~'obs',
                                        Q=='pcr'~ifelse(calibrMod=='calibrated',
                                                        'PCRcalibr','PCRun')))    #Q=='RFd' ~ ifelse(calibrMod=='calibrated', 'PCRcalibr-RFd','PCRun-RFd')
    p1 <- ggplot(data=ts_q %>% filter(datatype=='train'),
                 aes(x=datetime, y=discharge, col=Q))+
        geom_line(lwd=0.65, alpha=0.8)+
        facet_wrap(yr~., scale='free_x')+
        scale_x_date(date_labels = '%m', date_breaks = '1 month')+
        # geom_vline(
        #     xintercept=as.numeric((combine_values %>%
        #                                filter(grepl('-05-01', datetime)))$datetime), 
        #     lty=2, color='grey')+
        # geom_vline(
        #     xintercept=as.numeric((combine_values %>%
        #                                filter(grepl('-10-01', datetime)))$datetime), 
        #     lty=2, color='grey')+
        labs(title=paste0(plotTitle, ': train period (1981-1990)'),
             subtitle = calibrMod,
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
            legend.title = element_blank())+
        scale_colour_manual(values=c('black', 'cornflowerblue','indianred1'))+      #'#F0E442'
        scale_y_continuous(sec.axis = sec_axis(~.*convRatio, name=expression((m^{3}/s))))

    p2 <- p1%+%(ts_q %>% filter(datatype=='test'))+
        labs(title=paste0(plotTitle, ': validation period (1991-2000)'))

    # p1%+%(ts_q %>% filter(datatype=='test', yr%in%(1997:2000)))

    print(paste0('../graph/RFresult_all/timeseries_', calibrMod,
                 '/discharge_', plotTitle, '_train.tiff'))
    p1 %>% print
    ggsave(paste0('../graph/RFresult_all/timeseries_', calibrMod,
                  '/discharge_', plotTitle, '_train.tiff'), dpi=300,
           width=12, height=6)

    print(paste0('../graph/RFresult_all/timeseries_', calibrMod,
                 '/discharge_', plotTitle, '_test.tiff'))
    p2 %>% print
    ggsave(paste0('../graph/RFresult_all/timeseries_', calibrMod,
                  '/discharge_', plotTitle, '_test.tiff'), dpi=300,
           width=12, height=6)
    
    #-----------time series of residuals----------
    ts_res <- combine_values_res_real %>% gather(., key='Q',value='discharge',
                                                 c('res','RFds')) %>%
        mutate(datetime=as.Date(datetime)) %>%
        mutate(Q=factor(Q, levels = c('res','RFds')))
    ts_res <- ts_res %>% mutate(Q=case_when(Q=='RFds' ~ ifelse(calibrMod=='calibrated',
                                                               'PCRcalibr-RFds','PCRun-RFds'),
                                            Q=='res'~ifelse(calibrMod=='calibrated',
                                                            'PCRcalibr','PCRun')))       #Q=='RFd' ~ ifelse(calibrMod=='calibrated', 'PCRcalibr-RFd','PCRun-RFd')
    
    p1%+%(ts_res %>% filter(datatype=='train'))+
        labs(title=paste0(plotTitle, ': train period (1981-1990)'),
             y='residual (m/d)')+
        geom_abline(intercept = 0, slope = 0, color='grey',lty=2)+
        scale_colour_manual(values=c('cornflowerblue','indianred1'))+
        scale_y_continuous(sec.axis = sec_axis(~.*convRatio, name=expression((m^{3}/s))))
    ggsave(paste0('../graph/RFresult_all/timeseries_', calibrMod,
                  '/res_', plotTitle, '_train.tiff'), dpi=300,
           width=12, height=6)
    
    p1%+%(ts_res %>% filter(datatype=='test'))+
        labs(title=paste0(plotTitle, ': validation period (1991-2000)'),
             y='residual (m/d)')+
        scale_colour_manual(values=c('cornflowerblue','indianred1'))+
        geom_abline(intercept = 0, slope = 0, color='grey',lty=2)+
        scale_y_continuous(sec.axis = sec_axis(~.*convRatio, name=expression((m^{3}/s))))
    ggsave(paste0('../graph/RFresult_all/timeseries_', calibrMod,
                  '/res_', plotTitle, '_test.tiff'), dpi=300,
           width=12, height=6)
}

#!------ partial time series--------
for(i in seq_along(csvFiles[[1]])){
    t <- readData(i)
    combine_values <- t[[1]]
    # combine_values_res <- t[[2]]
    combine_values_res_real <- t[[3]]
    plotTitle <- t[[4]]
    # plotTitle <- paste0(c('(a) ','(b) ','(c) ')[i], plotTitle)
    convRatio <- t[[5]]
    
    ts_q <- combine_values %>% gather(., key='Q',value='discharge',
                               c('obs','RFds','pcr')) %>% 
        mutate(datetime=as.Date(datetime)) %>% 
        mutate(Q=factor(Q, levels = c('obs','pcr','RFds')))
    ts_q <- ts_q %>% mutate(Q=case_when(Q=='RFds' ~ ifelse(calibrMod=='calibrated',
                                                           'PCRcalibr-RFds','PCRun-RFds'),
                                        Q=='obs'~'obs',
                                        Q=='pcr'~ifelse(calibrMod=='calibrated',
                                                        'PCRcalibr','PCRun')))
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
            # axis.text.y = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 14),
            axis.text.x = element_text(size = 14),
            strip.text.x = element_text(size = 10, color = 'black'),
            strip.background = element_rect(colour = "transparent", fill = "white"),
            strip.text.y = element_text(size = 10, color = 'black'),
            # panel.margin=unit(.05, "lines"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            # strip.background = element_blank(),
            # strip.text = element_blank(),
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
        # lims(y=c(0,0.009))
    
    testp2 <- test_p1%+%(ts_q %>% filter(datatype=='test', yr%in%(1993:1994)))+
        # scale_x_date(date_labels = '%Y-%m-%d', 
        #              breaks = c('1993-01-01','1993-05-01','1993-10-01',
        #                         '1994-05-01','1994-10-01','1994-12-31') %>% as.Date(),
        #              expand = c(0,0))+
        theme(title = element_blank(),
              # plot.subtitle = element_blank(),
              legend.position = 'none',
              plot.margin = margin(0.1,0.2,0.1,0.1,"cm"))+
        lims(y=range(ts_q %>% filter(yr%in%(1991:1994)) %>% select(discharge)))+
        scale_y_continuous(sec.axis = sec_axis(~.*convRatio, name=expression((m^{3}/s))))
        # lims(y=c(0,0.009))
    
    # tiff(paste0('../graph/RFresult_all/timeseries_', calibrMod,
    #             '/F_discharge_', plotTitle, '_test.tiff'), res = 300, units = 'in',
    #      width=12, height=6)
    # grid.arrange(test_p1, testp2)
    # dev.off()
    
    trainp1 <- test_p1%+%(ts_q %>% filter(datatype=='train', yr%in%(1987:1988)))+
        # scale_x_date(date_labels = '%Y-%m-%d', 
        #              breaks = c('1987-01-01','1987-05-01','1987-10-01',
        #                         '1988-05-01','1988-10-01','1988-12-31') %>% as.Date(),
        #              expand = c(0,0))+
        theme(plot.margin = margin(0.5,0.2,0.1,0.1,"cm"))+
        labs(subtitle = paste0(calibrMod,' (train period)'))+
        lims(y=range(ts_q %>% filter(yr%in%(1987:1990)) %>% select(discharge)))+
        scale_y_continuous(sec.axis = sec_axis(~.*convRatio, name=expression((m^{3}/s))))
        # lims(y=c(0,0.009))
    
    trainp2 <- trainp1%+%(ts_q %>% filter(datatype=='train', yr%in%(1989:1990)))+
        # scale_x_date(date_labels = '%Y-%m-%d', 
        #              breaks = c('1989-01-01','1989-05-01','1989-10-01',
        #                         '1990-05-01','1990-10-01','1990-12-31') %>% as.Date(),
        #              expand = c(0,0))+
        theme(title = element_blank(),
              # plot.subtitle = element_blank(),
              legend.position = 'none',
              plot.margin = margin(0.1,0.2,0.1,0.1,"cm"))+
        lims(y=range(ts_q %>% filter(yr%in%(1987:1990)) %>% select(discharge)))+
        scale_y_continuous(sec.axis = sec_axis(~.*convRatio, name=expression((m^{3}/s))))
        # lims(y=c(0,0.009))
    
    # tiff(paste0('../graph/RFresult_all/timeseries_', calibrMod,
    #             '/F_discharge_', plotTitle, '_train.tiff'), res = 300, units = 'in',
    #      width=12, height=6)
    # grid.arrange(trainp1, trainp2)
    # dev.off()
    
    
    #--------partial time series of residuals------------
    ts_res <- combine_values_res_real %>% gather(., key='Q',value='discharge',
                                          c('res','RFds')) %>%
        mutate(datetime=as.Date(datetime)) %>%
        mutate(Q=factor(Q, levels = c('res','RFds')))
    
    testres_p1 <- test_p1%+%(ts_res %>% filter(datatype=='test', yr%in%(1991:1992)))+
        # scale_x_date(date_labels = '%Y-%m-%d', 
        #              breaks = c('1991-01-01','1991-05-01','1991-10-01',
        #                         '1992-05-01','1992-10-01','1992-12-31') %>% as.Date(),
        #              expand = c(0,0))+
        theme(plot.margin = margin(0.5,0.2,0.1,0.1,"cm"))+
        geom_abline(intercept = 0, slope = 0, color='grey',lty=2)+
        lims(y=range(ts_res %>% filter(yr%in%(1991:1994)) %>% select(discharge)))+
        # lims(y=c(-0.006,0.01))+
        labs(y='residual (m/d)')+
        scale_colour_manual(values=c('cornflowerblue','indianred1'))+
        scale_y_continuous(sec.axis = sec_axis(~.*convRatio, name=expression((m^{3}/s))))
    
    testres_p2 <- testres_p1%+%(ts_res %>% filter(datatype=='test', yr%in%(1993:1994)))+
        # scale_x_date(date_labels = '%Y-%m-%d', 
        #              breaks = c('1993-01-01','1993-05-01','1993-10-01',
        #                         '1994-05-01','1994-10-01','1994-12-31') %>% as.Date(),
        #              expand = c(0,0))+
        theme(title = element_blank(),
              # plot.subtitle = element_blank(),
              legend.position = 'none',
              plot.margin = margin(0.1,0.2,0.1,0.1,"cm"))+
        geom_abline(intercept = 0, slope = 0, color='grey',lty=2)+
        lims(y=range(ts_res %>% filter(yr%in%(1991:1994)) %>% select(discharge)))+
        scale_colour_manual(values=c('cornflowerblue','indianred1'))+
        scale_y_continuous(sec.axis = sec_axis(~.*convRatio, name=expression((m^{3}/s))))
        # lims(y=c(-0.006,0.01))
    
    # tiff(paste0('../graph/RFresult_all/timeseries_', calibrMod,
    #             '/F_res_', plotTitle, '_test.tiff'), res = 300, units = 'in',
    #      width=12, height=6)
    # grid.arrange(testres_p1, testres_p2)
    # dev.off()
    
    
    trainres_p1 <- test_p1%+%(ts_res %>% filter(datatype=='train', yr%in%(1987:1988)))+
        # scale_x_date(date_labels = '%Y-%m-%d', 
        #              breaks = c('1987-01-01','1987-05-01','1987-10-01',
        #                         '1988-05-01','1988-10-01','1988-12-31') %>% as.Date(),
        #              expand = c(0,0))+
        theme(plot.margin = margin(0.5,0.2,0.1,0.1,"cm"))+
        labs(subtitle = paste0(calibrMod,' (train period)'),
             y='residual (m/d)')+
        lims(y=range(ts_res %>% filter(yr%in%(1987:1990)) %>% select(discharge)))+
        scale_colour_manual(values=c('cornflowerblue','indianred1'))+
        scale_y_continuous(sec.axis = sec_axis(~.*convRatio, name=expression((m^{3}/s))))
        # lims(y=c(-0.006,0.01))
    
    trainres_p2 <- trainres_p1%+%(ts_res %>% filter(datatype=='train', yr%in%(1989:1990)))+
        # scale_x_date(date_labels = '%Y-%m-%d', 
        #              breaks = c('1989-01-01','1989-05-01','1989-10-01',
        #                         '1990-05-01','1990-10-01','1990-12-31') %>% as.Date(),
        #              expand = c(0,0),
        #              date_minor_breaks = "3 month")+
        theme(title = element_blank(),
              plot.subtitle = element_blank(),
              legend.position = 'none',
              plot.margin = margin(0.1,0.2,0.1,0.1,"cm"))+
        lims(y=range(ts_res %>% filter(yr%in%(1987:1990)) %>% select(discharge)))+
        scale_colour_manual(values=c('cornflowerblue','indianred1'))+
        scale_y_continuous(sec.axis = sec_axis(~.*convRatio, name=expression((m^{3}/s))))
        # lims(y=c(-0.006,0.01))
    # tiff(paste0('../graph/RFresult_all/timeseries_', calibrMod,
    #             '/F_res_', plotTitle, '_train.tiff'), res = 300, units = 'in',
    #      width=12, height=6)
    # grid.arrange(trainres_p1, trainres_p2)
    # dev.off()
    
    
    #-----putting discharge and res together--------
    tiff(paste0('../graph/RFresult_all/timeseries_', calibrMod,
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



#scatterplot of residuals vs predictions
for(i in seq_along(csvFiles[[1]])){
    t <- readData(i)
    # RF_res <- t[[1]]
    # combine_values_res <- t[[2]]
    plotTitle <- t[[3]]
    # combine_values <- t[[4]]
    combine_values_res_real <- t[[5]]
    
    ts_res <- combine_values_res_real %>% gather(., key='residuals',value='value',
                                     c('res','RF','RFbm')) %>% 
        mutate(datetime=as.Date(datetime)) %>% 
        mutate(residuals=factor(residuals, levels = c('res','RFbm','RF')))
    
    p1 <- ggplot(ts_res,
                 aes(x=pcr, y=value))+
        geom_point()+
        facet_grid(residuals~datatype)+
        # geom_smooth(method = "lm", se=FALSE, formula = y~x) +
        # stat_poly_eq(formula = y~x, 
        #              aes(label = paste(..eq.label..)), 
        #              parse = TRUE, size=4, hjust = 0, vjust = .5) +     
        # stat_poly_eq(formula = y~x, 
        #              aes(label = paste(..rr.label..)), 
        #              parse = TRUE, size=4, hjust = 0, vjust = 1.5) +   
        # xlim(c(0,0.0045))+ylim(c(-0.0025,0.004))+
        geom_abline(intercept=0, slope=0)+
        theme_grey(base_size = 13)+
        labs(title = plotTitle,
             subtitle = calibrMod, x='original PCR-GLOBWB predictions (m/d)', y='residuals (m/d)')
    print(paste0('../graph/RFresult_all/', calibrMod,
                 '_scatterplot_resVSpred_', plotTitle, '.tiff'))
    p1 %>% print()
    ggsave(paste0('../graph/RFresult_all/', 
                  'scatterplot_resVSpred_',calibrMod,'_', plotTitle, '.tiff'), dpi=300, 
           width=6, height=5.5)
    
    
}


#---------scatterplot of residuals vs obs-----------
for(i in seq_along(csvFiles[[1]])){
    t <- readData(i)
    # RF_res <- t[[1]]
    # combine_values_res <- t[[2]]
    plotTitle <- t[[4]]
    plotTitle <- paste0(c('(a) ','(b) ','(c) ')[i], plotTitle)
    combine_values_res_real <- t[[3]]
    
    ts_res <- combine_values_res_real %>% gather(., key='residuals',value='value',
                                     c('res','RFds','RFd')) %>% 
        mutate(datetime=as.Date(datetime)) %>% 
        mutate(residuals=factor(residuals, levels = c('res','RFd','RFds')))
    
    p1 <- ggplot(ts_res,
                 aes(x=obs, y=value))+
        geom_point()+
        facet_grid(residuals~datatype)+
        # geom_smooth(method = "lm", se=FALSE, formula = y~x) +
        # stat_poly_eq(formula = y~x, 
        #              aes(label = paste(..eq.label..)), 
        #              parse = TRUE, size=4, hjust = 0, vjust = .5) +     
        # stat_poly_eq(formula = y~x, 
        #              aes(label = paste(..rr.label..)), 
        #              parse = TRUE, size=4, hjust = 0, vjust = 1.5) +   
        # xlim(c(0,0.0045))+ylim(c(-0.0025,0.004))+
        geom_abline(intercept=0, slope=0)+
        theme_grey(base_size = 13)+
        labs(title = plotTitle,
             subtitle = calibrMod, x='observations (m/d)', y='residuals (m/d)')
    print(paste0('../graph/RFresult_all/', calibrMod,
                 'scatterplot_resVSobs_', plotTitle, '.tiff'))
    p1 %>% print()
    ggsave(paste0('../graph/RFresult_all/', 
                  'scatterplot_resVSobs_',calibrMod,'_', plotTitle, '.tiff'), dpi=300, 
           width=6, height=5.5)
}

#!---------scatterplot of predictions vs obs----------
temp <- vector('list',length(csvFiles[[1]]))
# only validation period and show all the locations in one graph
for(i in seq_along(csvFiles[[1]])){
    t <- readData(i)
    # RF_res <- t[[1]]
    # combine_values_res <- t[[2]]
    combine_values <- t[[1]]
    plotTitle <- t[[4]]
    # plotTitle <- paste0(c('(a) ','(b) ','(c) ')[i], plotTitle)
    # combine_values_res_real <- t[[3]]
    
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
    # geom_smooth(method = "lm", se=FALSE, formula = y~x) +
    # stat_poly_eq(formula = y~x,
    #              aes(label = paste(..eq.label..)),
    #              parse = TRUE, size=4, hjust = 0, vjust = .5) +
    stat_poly_eq(formula = y~x,
                 aes(label = paste(..rr.label..)),
                 parse = TRUE, size=4, hjust = 0, vjust = 0.85) +
    # xlim(c(0,0.0045))+ylim(c(-0.0025,0.004))+
    geom_abline(intercept=0, slope=1, lty=2, lwd=0.8,
                color='dark grey')+
    lims(x=r, y=r)+
    # theme_light()+
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
        # strip.background = element_blank(),
        # strip.text = element_blank(),
        title = element_text(size = 15),
        # plot.subtitle = element_text(size = 14),
        # legend.text = element_text(size = 14),
        # legend.title = element_text(size = 14)
    )+
    labs(x='observed streamflow (m/d)', y='simulated streamflow (m/d)',
         subtitle = calibrMod)
ggsave(paste0('../graph/RFresult_all/', 
              'scatterplot_predVSobs_',calibrMod, '_all.tiff'), dpi=300, 
       width=9, height=7)


