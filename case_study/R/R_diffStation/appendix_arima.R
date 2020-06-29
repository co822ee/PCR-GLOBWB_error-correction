# This script experiments on using time series model to predict pcrglobwb residuals/errors.
# calibrMod <- 'calibrated'      # calibrated / uncalibrated    
# whether to implement for the calibrated/uncalibrated PCR-GLOBWB model 
trainPeriod <- 1981:1990
testPeriod <- 1991:2000
benchmark=F
repeatedCV=F

gof <- list()
for(calibrMod in c('calibrated', 'uncalibrated')){
    print(calibrMod)
    source('function_0_loadLibrary.R')
    source('function_2_RF_0_setUpDirectory.R')
    library(forecast)
    for(station_i in seq_along(station)){
        source('function_1_readData_excludeChannelStorage.R')
        if(station_i==1){
            df <- matrix(NA, ncol=length(station), nrow=nrow(all %>% filter(datatype=='test')))
            obs <- matrix(NA, ncol=length(station), nrow=nrow(all %>% filter(datatype=='test')))
        }
        print(plotTitle)
        res_train <- ts((all %>% filter(datatype=='train') %>% select(res))[,1], 
                        start = c(1981, 1), 
                        frequency = 365.24)
        
        res_test <- ts((all %>% filter(datatype=='test') %>% select(res))[,1], 
                       start = c(trainPeriod[1], 1), 
                       frequency = 365.24)
        res <- ts(all$res, 
                  start = c(trainPeriod[1], 1),
                  frequency = 365.24)
        # original
        # arima_model <- auto.arima(res_train)
        # summary(arima_model)
        
        res_stl <- stl(res_train, s.window="periodic")       
        # res_decomp <- res_stl$time.series[,3]
        res_arima <- forecast(res_stl,h=length(res_test),method ='arima')
        plot(res_arima, main = paste0('1981-2000: ', plotTitle))
        
        df[, station_i] <- res_arima$mean %>% as.numeric() + 
            (all %>% filter(datatype=='test') %>% select(pcr))[,1]
        obs[, station_i] <- (all %>% filter(datatype=='test') %>% select(obs))[,1]
        
        p_df <- data.frame(pred=df[, station_i]) %>% 
            cbind((all %>% filter(datatype=='test') %>% select(obs, yr, datetime)))
        
        temp <- p_df %>% summarise(KGE=KGE(sim = pred, obs = obs,
                                           s = c(1,1,1), na.rm = TRUE, method = "2009"),
                                   NSE = NSE(sim = pred, obs = obs, 
                                             na.rm = T))%>% 
            cbind(station=stationInfo$station[station_i], plotTitle=stationInfo$plotName[station_i])
        if(station_i==1){
            gof[[calibrMod]] <- temp
        }else{ gof[[calibrMod]] <- rbind(gof[[calibrMod]], temp)}
        
        p1 <- ggplot(p_df %>% gather('key','discharge', -yr, -datetime), 
                     aes(x=datetime, y=discharge, col=key))+
            geom_line()+
            facet_wrap(yr~., scale='free_x')+
            scale_x_date(date_labels = '%m', date_breaks = '1 month')+
            
            # scale_x_date(date_labels = "%y", date_breaks = '1 year')+
            theme_bw()+
            labs(title=paste0('1981-2000: ', plotTitle), y='discharge (m/day)', x='month')+
            scale_colour_manual(values=c('black','red'))+
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
                legend.title = element_text(size = 12))
        p1 %>% print
        print(paste0('../graph/RFresult_all/timeseries_', calibrMod,
                     '/discharge_', plotTitle, '_test_arima.tiff'))
        ggsave(paste0('../graph/RFresult_all/timeseries_', calibrMod,
                      '/discharge_', plotTitle, '_test_arima.tiff'), dpi=300,
               width=12, height=6)
        
        # arima_model <- auto.arima(res_train)
        # arima_model %>%
        #     forecast(h=22) %>%
        #     autoplot()
        
        # decompose
        
        # 
        # arima_model_decomp <- auto.arima(res_decomp)
        
        # comparison
        # summary(arima_model)
        # summary(arima_model_decomp)
        
        # arima_model
        # arima_model_decomp
        
        # coef <- arima_model$coef
        # coef_decomp <- arima_model_decomp$coef
        
        # pred <- vector(mode='numeric', length = length(res_test))
        # predict(arima_model, res_test %>% as.numeric())
        
        # Predict
        # for(i in seq_along(res_test)){
        #     # if(i >= 2){
        #     #     pred[i] <- coef_decomp[1]*res_test[i-1] + coef_decomp[2]*theta[i-1] + 
        #     #         coef_decomp[3] 
        #     #         
        #     # }
        #     if(i >= 5){
        #         pred[i] <- coef[1]*res_test[i-1] + coef[2]*res_test[i-2] + 
        #             coef[3]*res_test[i-3] + coef[4]*res_test[i-4] + 
        #             coef[5] 
        #         
        #     }
        #     
        # }
        
        
        # p1 <- ggplot(data.frame(pred=pred, obs=res_test %>% as.numeric()) %>% 
        #            cbind(all %>% filter(datatype=='test') %>% select(datetime, yr)) %>% 
        #            gather('data','value',-datetime,-yr), 
        #        aes(x=datetime, y=value, col=data))+
        #     geom_line()+
        #     facet_wrap(yr~., scale='free_x')+
        #     scale_x_date(date_labels = '%m', date_breaks = '1 month')+
        # 
        #     # scale_x_date(date_labels = "%y", date_breaks = '1 year')+
        #     theme_bw()+
        #     labs(title=paste0('1981-2000: ', plotTitle), y='res (cms)', x='datetime (yy)')+
        #     scale_colour_manual(values=c('black','red'))+
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
        #         legend.title = element_text(size = 12)) 
        # print(p1)
        
    }
}
for(i in seq_along(gof)){
    if(names(gof[i])=='calibrated') gof[[i]] <- gof[[i]] %>% mutate(pcr_config='PCRcalibr')
    if(names(gof[i])=='uncalibrated') gof[[i]] <- gof[[i]] %>% mutate(pcr_config='PCRun')
    if(i==1) temp <- gof[[i]]
    else{ temp <- rbind(temp, gof[[i]])}
}

gof_arima <- temp 

dir <- c(paste0('../data/analysis/benchmark/', c('result_calibrated/', 'result_uncalibrated/')), 
         paste0('../data/analysis/', c('result_calibrated/', 'result_uncalibrated/'))) %>% as.list
configKey <- list('PCRcalibr-RFd','PCRun-RFd','PCRcalibr-RFds','PCRun-RFds')

calibrL <- lapply(configKey, grepl, pattern='calibr')
bmL <- list(T,T,F,F)

lapply(dir, list.files, pattern='rf_eval')
csvFiles <- lapply(dir, paste0, '/rf_eval_r.csv')
rf.eval <- lapply(csvFiles, read.csv, header=T)
rf.eval[[1]] %>% str
rf.eval[[1]] %>% dim
# pcr <- vector('list',length(rf.eval))
# rf_biasCorrect <- vector('list',length(rf.eval))
for(i in seq_along(rf.eval)){
    rf.eval[[i]] <- rf.eval[[i]] %>% 
        mutate(config=configKey[[i]])
    # rf_config=ifelse(bmL[[i]], 'benchmark', 'RF_pcrState')
    # calibr_config=ifelse(calibrL[[i]], 'PCR_calibr', 'PCR_uncalibr'),
}
eval_all <- do.call(rbind, rf.eval) %>% 
    mutate(config=config %>% 
               factor(., levels = c('PCRun-RFd','PCRun-RFds', 
                                    'PCRcalibr-RFd','PCRcalibr-RFds'))) %>% 
    # mutate(station=factor(station, levels=c('Basel','Lobith','Cochem'))) %>% 
    # mutate(plotTitle=factor(plotTitle, levels = c('Basel (Rhine)','Lobith (Rhine)',
    #                                               'Cochem (Moselle)'))) %>% 
    select(-nMAE, -nMAE_corrected, -Rsquared, -Rsquared_corrected)
# eval_all %>% gather(., 'key','value', c('KGE','nRMSE', 'nMAE', 'Rsquared'))


eval_allG <- eval_all %>% 
    gather(., 'gof','value', -c('datatype','station', 'plotTitle', 
                                'config')) %>% 
    mutate(gof_col=ifelse(grepl('corrected', gof), 
                          'RFcorrected', 'purePCR'),
           pcr_config=sapply(strsplit(config %>% as.character(), "-"), "[", 1) %>% 
               factor(levels=c('PCRun', 'PCRcalibr')))


rf_gof <- eval_allG %>% filter(datatype=='test') %>% 
    dplyr::filter(grepl('KGE|NSE', gof))


gof_p <- rf_gof %>% 
    filter(grepl('corrected', gof), config=='PCRcalibr-RFds'|config=='PCRun-RFds') %>% 
    mutate(gof=sub('_corrected','', gof)) %>% select(-gof_col, -datatype) %>% 
    rbind(gof_arima %>% 
              gather(., 'gof', 'value', KGE, NSE) %>% 
              mutate(config=paste0(pcr_config,'-arima'))) %>% 
    mutate(config=factor(config %>% as.character(), levels = c('PCRun-arima', 'PCRun-RFds',
                                                               'PCRcalibr-arima', 'PCRcalibr-RFds')))

ggplot(data=gof_p)+
    geom_col(position = 'dodge', aes(x=station, y=value, fill=config))+
    #pure PCR model
    facet_wrap(gof~., scale='free_x',ncol=1)+
    theme_linedraw()+
    scale_y_continuous(breaks=c(0,0.25,0.5,0.6,0.7,0.8,0.9))+
    theme(
        axis.text.y = element_text(size = 14),
        # axis.text.y = element_blank(),
        axis.title = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 14),
        axis.ticks = element_line(size=1),
        strip.text.x = element_text(size = 15, color = 'black'),
        strip.background = element_rect(colour = "black", fill = "white"),
        strip.text.y = element_text(size = 15, color = 'black'),
        
        panel.spacing=unit(1.3, "lines"),
        panel.grid.major.y = element_line(color='black', linetype=2),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        
        title = element_text(size = 17),
        plot.subtitle = element_text(size = 15),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 14)
        # legend.position = 'bottom'
    )+
    scale_fill_manual(
        # labels = c('PCRun', 'PCRun-RFd', 'PCRun-RFds', 
        #            'PCRcalibr', 'PCRcalibr-RFd','PCRcalibr-RFds'), 
        # name = 'models',
        values=c('greenyellow','forestgreen',
                 'lightblue','midnightblue'))+
    labs(title = 'Model performance at different stations', 
         subtitle = '1991-2000 (test period)', 
         y='',
         color=paste0('PCR without RF-correction'), 
         fill=paste0('Models'))
ggsave('../graph/RFresult_all/gof_arima02.tiff', dpi = 300,
       width = 6, height = 6)
