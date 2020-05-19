calibrMod <- 'uncalibrated'      # calibrated    uncalibrated
# station_i <- 2                         # station id

source('function_0_loadLibrary.R')
dir <- c(paste0('../data/analysis/benchmark/',  'result_', calibrMod,'/'), 
         paste0('../data/analysis/', 'result_', calibrMod,'/'))
configKey <- list('PCRcalibr-RFbm','PCRun-RFbm','PCRcalibr-RF','PCRun-RF')

calibrL <- lapply(configKey, grepl, pattern='calibr')
bmL <- lapply(configKey, grepl, pattern='bm')
files <- lapply(dir, list.files, pattern='rf_result')

stationInfo <- read.csv('../data/rawData/stationLatLon.csv')

csvFiles <- lapply(seq_along(files), function(i) paste0(dir[i], files[[i]]))
if(!dir.exists(paste0('../graph/RFresult_all/timeseries_', calibrMod))){
    dir.create(paste0('../graph/RFresult_all/timeseries_', calibrMod))
}
readData <- function(i){
    station <- (files[[1]][i] %>% strsplit(., '_', 3))[[1]][3] %>% sub('.csv','',.)
    plotTitle <- stationInfo$plotName[which((stationInfo$station %>% tolower())==(station %>% tolower()))]

    bmRF <- read.csv(csvFiles[[1]][i], header = T)
    RF <- read.csv(csvFiles[[2]][i], header = T)
    print(paste0(calibrMod, ': ', plotTitle))
    
    bmRF_corct <- bmRF %>% select('pcr_corrected','datetime') %>% #mod_res
        rename(RFbm=pcr_corrected) 
    RF_corct <- RF %>% select('pcr_corrected','datetime') %>% #mod_res
        rename(RF=pcr_corrected)
    combine <- inner_join(bmRF %>% select(-c('mod_res','pcr_corrected')), 
                          bmRF_corct, by='datetime') %>% inner_join(., RF_corct, by='datetime') %>% 
        mutate(datetime=as.Date(datetime))
    
    bmRF_res <- bmRF %>% select('mod_res','datetime') %>% #mod_res
        rename(RFbm=mod_res) 
    RF_res <- RF %>% select('mod_res','datetime') %>% #mod_res
        rename(RF=mod_res)
    combine_res <- inner_join(bmRF %>% select(-c('mod_res','pcr_corrected')), 
                              bmRF_res, by='datetime') %>% inner_join(., RF_res, by='datetime') %>% 
        mutate(datetime=as.Date(datetime))
    
    combine_res_real <- combine %>% mutate(RFbm=obs-RFbm, RF=obs-RF)
    list(RF_res, combine_res, plotTitle, combine, combine_res_real)
    
}
#hydrograph
for(i in seq_along(csvFiles[[1]])){
    t <- readData(i)
    RF_res <- t[[1]]
    combine_res <- t[[2]]
    plotTitle <- t[[3]]
    combine <- t[[4]]
    
    
    #--------time series of streamflow (hydrograph)------------
    ts_q <- combine %>% gather(., key='Q',value='discharge',
                                 c('obs','pcr','RF','RFbm')) %>% 
        mutate(datetime=as.Date(datetime)) %>% 
        mutate(Q=factor(Q, levels = c('obs','pcr','RFbm','RF')))
    p1 <- ggplot(data=ts_q %>% filter(datatype=='train'), 
                 aes(x=datetime, y=discharge, col=Q))+
        geom_line(alpha=0.7)+
        facet_wrap(yr~., scale='free')+
        scale_x_date(date_labels = '%b', date_breaks = '1 month')+
        geom_vline(
            xintercept=as.numeric((combine %>% 
                                       filter(grepl('-05-01', datetime)))$datetime), lty=2)+
        geom_vline(
            xintercept=as.numeric((combine %>% 
                                       filter(grepl('-10-01', datetime)))$datetime), lty=2)+
        labs(title=paste0(plotTitle, ': train period (1981-1990)'), 
             subtitle = calibrMod,
             y='discharge (cms)')+
        theme_light()+
        theme(
            axis.text.y = element_text(size = 7),
            # axis.text.y = element_blank(),
            axis.title = element_text(size = 0),
            axis.text.x = element_text(size = 7),
            strip.text.x = element_text(size = 10, color = 'black'),
            strip.background = element_rect(colour = "transparent", fill = "white"),
            strip.text.y = element_text(size = 10, color = 'black'),
            # strip.background = element_blank(),
            # strip.text = element_blank(),
            title = element_text(size = 15),
            plot.subtitle = element_text(size = 12),
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 12))+
        scale_colour_manual(values=c('black','red','green','blue'))
    
    p2 <- p1%+%(ts_q %>% filter(datatype=='test'))+
        labs(title=paste0(plotTitle, ': test period (1991-2000)'))
    
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
    
    #--------time series of residuals------------
    ts_res <- combine_res %>% gather(., key='residuals',value='value',
                                   c('res','RF','RFbm')) %>% 
        mutate(datetime=as.Date(datetime)) %>% 
        mutate(residuals=factor(residuals, levels = c('res','RFbm','RF')))
    
    p1 <- ggplot(data=ts_res %>% filter(datatype=='train'), 
                 aes(x=datetime, y=value, col=residuals))+
        geom_line(alpha=0.7)+
        facet_wrap(yr~., scale='free')+
        scale_x_date(date_labels = '%b', date_breaks = '1 month')+
        geom_vline(
            xintercept=as.numeric((combine_res %>% 
                                       filter(grepl('-05-01', datetime)))$datetime), lty=2)+
        geom_vline(
            xintercept=as.numeric((combine_res %>% 
                                       filter(grepl('-10-01', datetime)))$datetime), lty=2)+
        labs(title=paste0(plotTitle, ': train period (1981-1990)'), 
             y='residuals (cms)')+
        theme_light()+
        theme(
            axis.text.y = element_text(size = 7),
            # axis.text.y = element_blank(),
            axis.title = element_text(size = 0),
            axis.text.x = element_text(size = 7),
            strip.text.x = element_text(size = 10, color = 'black'),
            strip.background = element_rect(colour = "transparent", fill = "white"),
            strip.text.y = element_text(size = 10, color = 'black'),
            # strip.background = element_blank(),
            # strip.text = element_blank(),
            title = element_text(size = 15),
            plot.subtitle = element_text(size = 12),
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 12))+
        scale_colour_manual(values=c('black','green','blue'))
    
    p2 <- p1%+%(ts_res %>% filter(datatype=='test'))+
        labs(title=paste0(plotTitle, ': test period (1991-2000)'))
    
    print(paste0('../graph/RFresult_all/timeseries_', calibrMod,
                 '/res_', plotTitle, '_train.tiff'))
    p1 %>% print
    ggsave(paste0('../graph/RFresult_all/timeseries_', calibrMod,
                  '/res_', plotTitle, '_train.tiff'), dpi=300, 
           width=12, height=6)
    
    print(paste0('../graph/RFresult_all/timeseries_', calibrMod,
                 '/res_', plotTitle, '_test.tiff'))
    p2 %>% print
    ggsave(paste0('../graph/RFresult_all/timeseries_', calibrMod,
                  '/res_', plotTitle, '_test.tiff'), dpi=300, 
           width=12, height=6)
    
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
