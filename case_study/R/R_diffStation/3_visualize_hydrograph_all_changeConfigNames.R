calibrMod <- 'uncalibrated'      # calibrated    uncalibrated
# station_i <- 2                         # station id

source('function_0_loadLibrary.R')
dir <- c(paste0('../data/analysis/benchmark/',  'result_', calibrMod,'/'), 
         paste0('../data/analysis/', 'result_', calibrMod,'/'))
configKey <- list('PCRcalibr-RFds','PCRun-RFd','PCRcalibr-RFds','PCRun-RFd')

calibrL <- lapply(configKey, grepl, pattern='calibr')
bmL <- list(F,T,F,T)
files <- lapply(dir, list.files, pattern='rf_result')

stationInfo <- read.csv('../data/rawData/stationLatLon.csv')

csvFiles <- lapply(seq_along(files), function(i) paste0(dir[i], files[[i]]))
if(!dir.exists(paste0('../graph/RFresult_all/timeseries_', calibrMod))){
    dir.create(paste0('../graph/RFresult_all/timeseries_', calibrMod))
}
readData <- function(i){
    station <- (files[[1]][i] %>% strsplit(., '_', 3))[[1]][3] %>% sub('.csv','',.)
    plotTitle <- stationInfo$plotName[which((stationInfo$station %>% tolower())==(station %>% tolower()))]

    RFd <- read.csv(csvFiles[[1]][i], header = T)
    RFds <- read.csv(csvFiles[[2]][i], header = T)
    print(paste0(calibrMod, ': ', plotTitle))
    
    RFd_corct <- RFd %>% select('pcr_corrected','datetime') %>% #mod_res
        rename(RFd=pcr_corrected) 
    RFds_corct <- RFds %>% select('pcr_corrected','datetime') %>% #mod_res
        rename(RFds=pcr_corrected)
    
    combine <- inner_join(RFd %>% select(-c('mod_res','pcr_corrected')), 
                          RFd_corct, by='datetime') %>% inner_join(., RFds_corct, by='datetime') %>% 
        mutate(datetime=as.Date(datetime))
    
    RFd_res <- RFd %>% select('mod_res','datetime') %>% #mod_res
        rename(RFd=mod_res) 
    RFds_res <- RFds %>% select('mod_res','datetime') %>% #mod_res
        rename(RFds=mod_res)
    
    combine_res <- inner_join(RFd %>% select(-c('mod_res','pcr_corrected')), 
                              RFd_res, by='datetime') %>% inner_join(., RFds_res, by='datetime') %>% 
        mutate(datetime=as.Date(datetime))
    
    combine_res_real <- combine %>% mutate(RFd=obs-RFd, RFds=obs-RFds)
    list(combine, combine_res, combine_res_real, plotTitle)
}
#-------------hydrograph---------------
for(i in seq_along(csvFiles[[1]])){
    t <- readData(i)
    combine <- t[[1]]
    combine_res <- t[[2]]
    combine_res_real <- t[[3]]
    plotTitle <- t[[4]]
    
    #--------time series of streamflow (hydrograph)------------
    ts_q <- combine %>% gather(., key='Q',value='discharge',
                                 c('obs','RFds','RFd')) %>% 
        mutate(datetime=as.Date(datetime)) %>% 
        mutate(Q=factor(Q, levels = c('obs','RFd','RFds')))
    # p1 <- ggplot(data=ts_q %>% filter(datatype=='train'), 
    #              aes(x=datetime, y=discharge, col=Q))+
    #     geom_line(lwd=0.8)+
    #     facet_wrap(yr~., scale='free_x')+
    #     scale_x_date(date_labels = '%m', date_breaks = '1 month')+
    #     geom_vline(
    #         xintercept=as.numeric((combine %>% 
    #                                    filter(grepl('-05-01', datetime)))$datetime), lty=2)+
    #     geom_vline(
    #         xintercept=as.numeric((combine %>% 
    #                                    filter(grepl('-10-01', datetime)))$datetime), lty=2)+
    #     labs(title=paste0(plotTitle, ': train period (1981-1990)'), 
    #          subtitle = calibrMod,
    #          y='discharge (m/d)',
    #          x='month')+
    #     theme_linedraw()+
    #     theme(
    #         axis.text.y = element_text(size = 7),
    #         # axis.text.y = element_blank(),
    #         axis.title.x = element_text(size = 10),
    #         axis.title.y = element_text(size = 10),
    #         axis.text.x = element_text(size = 7),
    #         strip.text.x = element_text(size = 10, color = 'black'),
    #         strip.background = element_rect(colour = "transparent", fill = "white"),
    #         strip.text.y = element_text(size = 10, color = 'black'),
    #         # panel.margin=unit(.05, "lines"),
    #         panel.grid.major = element_blank(),
    #         panel.grid.minor = element_blank(),
    #         # strip.background = element_blank(),
    #         # strip.text = element_blank(),
    #         title = element_text(size = 15),
    #         plot.subtitle = element_text(size = 12),
    #         legend.text = element_text(size = 12),
    #         legend.title = element_text(size = 12))+
    #     scale_colour_manual(values=c('black','#F0E442','#D55E00'))
    # 
    # p2 <- p1%+%(ts_q %>% filter(datatype=='test'))+
    #     labs(title=paste0(plotTitle, ': test period (1991-2000)'))
    # 
    # # p1%+%(ts_q %>% filter(datatype=='test', yr%in%(1997:2000)))
    # 
    # print(paste0('../graph/RFresult_all/timeseries_', calibrMod,
    #              '/discharge_', plotTitle, '_train.tiff'))
    # p1 %>% print
    # ggsave(paste0('../graph/RFresult_all/timeseries_', calibrMod,
    #               '/discharge_', plotTitle, '_train.tiff'), dpi=300, 
    #        width=12, height=6)
    # 
    # print(paste0('../graph/RFresult_all/timeseries_', calibrMod,
    #              '/discharge_', plotTitle, '_test.tiff'))
    # p2 %>% print
    # ggsave(paste0('../graph/RFresult_all/timeseries_', calibrMod,
    #               '/discharge_', plotTitle, '_test.tiff'), dpi=300, 
    #        width=12, height=6)
    
    
    #------partial time series of discharge------
    test_p1 <- ggplot(data=ts_q %>% filter(datatype=='test', yr%in%(1991:1992)),
                      aes(x=datetime, y=discharge, col=Q))+
        geom_line(lwd=0.8)+
        # facet_wrap(yr~., scale='free_x')+
        scale_x_date(date_labels = '%Y-%m-%d', 
                     breaks = c('1991-01-01','1991-05-01','1991-10-01',
                                '1992-05-01','1992-10-01','1992-12-31') %>% as.Date(),
                     expand = c(0,0))+   #, date_breaks = '3 month'
    
        # geom_vline(
        #     xintercept=as.numeric((combine %>% 
        #                                filter(grepl('-05-01', datetime)))$datetime), lty=2)+
        # geom_vline(
        #     xintercept=as.numeric((combine %>% 
        #                                filter(grepl('-10-01', datetime)))$datetime), lty=2)+
        labs(title=paste0(plotTitle), 
             subtitle = calibrMod,
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
        lims(y=c(0,0.0125))
    
    testp2 <- test_p1%+%(ts_q %>% filter(datatype=='test', yr%in%(1993:1994)))+
        scale_x_date(date_labels = '%Y-%m-%d', 
                     breaks = c('1993-01-01','1993-05-01','1993-10-01',
                                '1994-05-01','1994-10-01','1994-12-31') %>% as.Date(),
                     expand = c(0,0))+
        theme(title = element_blank(),
              plot.subtitle = element_blank(),
              legend.position = 'none',
              plot.margin = margin(0.1,1.2,0.1,0.1,"cm"))+
        lims(y=c(0,0.0125))
    
    tiff(paste0('../graph/RFresult_all/timeseries_', calibrMod,
                '/F_discharge_', plotTitle, '_test.tiff'), res = 300, units = 'in',
         width=12, height=6)
    grid.arrange(test_p1, testp2)
    dev.off()
    
    trainp1 <- test_p1%+%(ts_q %>% filter(datatype=='train', yr%in%(1987:1988)))+
        scale_x_date(date_labels = '%Y-%m-%d', 
                     breaks = c('1987-01-01','1987-05-01','1987-10-01',
                                '1988-05-01','1988-10-01','1988-12-31') %>% as.Date(),
                     expand = c(0,0))+
        theme(plot.margin = margin(0.5,1.2,0.1,0.1,"cm"))+
        lims(y=c(0,0.0125))
    
    trainp2 <- trainp1%+%(ts_q %>% filter(datatype=='train', yr%in%(1989:1990)))+
        scale_x_date(date_labels = '%Y-%m-%d', 
                     breaks = c('1989-01-01','1989-05-01','1989-10-01',
                                '1990-05-01','1990-10-01','1990-12-31') %>% as.Date(),
                     expand = c(0,0))+
        theme(title = element_blank(),
              plot.subtitle = element_blank(),
              legend.position = 'none',
              plot.margin = margin(0.1,1.2,0.1,0.1,"cm"))+
        lims(y=c(0,0.0125))
    
    tiff(paste0('../graph/RFresult_all/timeseries_', calibrMod,
                '/F_discharge_', plotTitle, '_train.tiff'), res = 300, units = 'in',
         width=12, height=6)
    grid.arrange(trainp1, trainp2)
    dev.off()
    
    #--------time series of residuals------------
    # ts_res <- combine_res %>% gather(., key='residuals',value='value',
    #                                c('res','RF','RFbm')) %>% 
    #     mutate(datetime=as.Date(datetime)) %>% 
    #     mutate(residuals=factor(residuals, levels = c('res','RFbm','RF')))
    # 
    # p1 <- ggplot(data=ts_res %>% filter(datatype=='train'), 
    #              aes(x=datetime, y=value, col=residuals))+
    #     geom_line(alpha=0.7)+
    #     facet_wrap(yr~., scale='fixed')+
    #     scale_x_date(date_labels = '%b', date_breaks = '1 month')+
    #     geom_vline(
    #         xintercept=as.numeric((combine_res %>% 
    #                                    filter(grepl('-05-01', datetime)))$datetime), lty=2)+
    #     geom_vline(
    #         xintercept=as.numeric((combine_res %>% 
    #                                    filter(grepl('-10-01', datetime)))$datetime), lty=2)+
    #     labs(title=paste0(plotTitle, ': train period (1981-1990)'), 
    #          y='residuals (cms)')+
    #     theme_light()+
    #     theme(
    #         axis.text.y = element_text(size = 7),
    #         # axis.text.y = element_blank(),
    #         axis.title = element_text(size = 0),
    #         axis.text.x = element_text(size = 7),
    #         strip.text.x = element_text(size = 10, color = 'black'),
    #         strip.background = element_rect(colour = "transparent", fill = "white"),
    #         strip.text.y = element_text(size = 10, color = 'black'),
    #         # strip.background = element_blank(),
    #         # strip.text = element_blank(),
    #         title = element_text(size = 15),
    #         plot.subtitle = element_text(size = 12),
    #         legend.text = element_text(size = 12),
    #         legend.title = element_text(size = 12))+
    #     scale_colour_manual(values=c('black','green','blue'))
    # 
    # p2 <- p1%+%(ts_res %>% filter(datatype=='test'))+
    #     labs(title=paste0(plotTitle, ': test period (1991-2000)'))
    # 
    # print(paste0('../graph/RFresult_all/timeseries_', calibrMod,
    #              '/res_', plotTitle, '_train.tiff'))
    # p1 %>% print
    # ggsave(paste0('../graph/RFresult_all/timeseries_', calibrMod,
    #               '/res_', plotTitle, '_train.tiff'), dpi=300, 
    #        width=12, height=6)
    # 
    # print(paste0('../graph/RFresult_all/timeseries_', calibrMod,
    #              '/res_', plotTitle, '_test.tiff'))
    # p2 %>% print
    # ggsave(paste0('../graph/RFresult_all/timeseries_', calibrMod,
    #               '/res_', plotTitle, '_test.tiff'), dpi=300, 
    #        width=12, height=6)
    
}

#scatterplot of residuals vs predictions
for(i in seq_along(csvFiles[[1]])){
    t <- readData(i)
    # RF_res <- t[[1]]
    # combine_res <- t[[2]]
    plotTitle <- t[[3]]
    # combine <- t[[4]]
    combine_res_real <- t[[5]]
    
    ts_res <- combine_res_real %>% gather(., key='residuals',value='value',
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
    # combine_res <- t[[2]]
    plotTitle <- t[[3]]
    # combine <- t[[4]]
    combine_res_real <- t[[5]]
    
    ts_res <- combine_res_real %>% gather(., key='residuals',value='value',
                                     c('res','RF','RFbm')) %>% 
        mutate(datetime=as.Date(datetime)) %>% 
        mutate(residuals=factor(residuals, levels = c('res','RFbm','RF')))
    
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

#---------scatterplot of predictions vs obs----------
for(i in seq_along(csvFiles[[1]])){
    t <- readData(i)
    # RF_res <- t[[1]]
    # combine_res <- t[[2]]
    plotTitle <- t[[3]]
    combine <- t[[4]]
    # combine_res_real <- t[[5]]
    
    ts_res <- combine %>% gather(., key='prediction',value='value',
                                          c('pcr','RF','RFbm')) %>% 
        mutate(datetime=as.Date(datetime)) %>% 
        mutate(prediction=factor(prediction, levels = c('pcr','RFbm','RF')))
    
    p1 <- ggplot(ts_res,
                 aes(x=obs, y=value))+
        geom_point(alpha=0.3, color='black')+
        facet_grid(prediction~datatype)+
        geom_smooth(method = "lm", se=FALSE, formula = y~x) +
        # stat_poly_eq(formula = y~x,
        #              aes(label = paste(..eq.label..)),
        #              parse = TRUE, size=4, hjust = 0, vjust = .5) +
        stat_poly_eq(formula = y~x,
                     aes(label = paste(..rr.label..)),
                     parse = TRUE, size=4, hjust = 0, vjust = 0.85) +
        # xlim(c(0,0.0045))+ylim(c(-0.0025,0.004))+
        geom_abline(intercept=0, slope=1, lty=2, lwd=0.8,
                    color='dark grey')+
        theme_light()+
        theme(
            axis.text.y = element_text(size = 12),
            # axis.text.y = element_blank(),
            axis.title = element_text(size = 12),
            axis.text.x = element_text(size = 12),
            strip.text.x = element_text(size = 12, color = 'black'),
            strip.background = element_rect(colour = "grey", fill = "transparent"),
            strip.text.y = element_text(size = 12, color = 'black'),
            # strip.background = element_blank(),
            # strip.text = element_blank(),
            title = element_text(size = 15),
            # plot.subtitle = element_text(size = 12),
            # legend.text = element_text(size = 12),
            # legend.title = element_text(size = 12)
            )+
        labs(title = plotTitle,
             subtitle = calibrMod, x='observations (m/d)', y='residuals (m/d)')
    print(paste0('../graph/RFresult_all/', calibrMod,
                 'scatterplot_predVSobs_', plotTitle, '.tiff'))
    p1 %>% print()
    ggsave(paste0('../graph/RFresult_all/', 
                  'scatterplot_predVSobs_',calibrMod,'_', plotTitle, '.tiff'), dpi=300, 
           width=6, height=7)
}
