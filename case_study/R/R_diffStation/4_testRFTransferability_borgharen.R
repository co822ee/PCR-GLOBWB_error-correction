# input:
calibrMod <- 'uncalibrated'      # calibrated    uncalibrated
trainPeriod <- 1981:1990
testPeriod <- 1991:2000
repeatedCV <- F # whether repeated two-fold cv
benchmark <- F  # benchmark model or not

source('function_0_loadLibrary.R')
onlytest <- c('Borgharen','Maxau','Wuerzburg')
station <- c('Basel','Lobith','Cochem',onlytest)
stationDF <- expand.grid(train=station[!station%in%onlytest], 
                         test=station) %>% sapply(as.character)

for(i in seq_along(stationDF[,1])){
    print(i)
    trainStation <- stationDF[i,1]
    testStation <- stationDF[i,2]
    
    source('function_2_RF_0_setUpDirectory.R')
    
    #---------- can be later put in function_2_RF_0_setUpDirectory.R
    outputFolder <- '../data/analysis/testTransferability/'
    outputGraphFolder <- '../graph/testTransferability/'
    if(!dir.exists(outputFolder)){
        dir.create(outputFolder)
    }
    if(!dir.exists(outputGraphFolder)){
        dir.create(outputGraphFolder)
    }
    # Create subfolders for calibrated and uncalibrated results
    if(!dir.exists(paste0(outputFolder, 'result_', calibrMod, '/'))){
        paste0(outputFolder, 'result_', calibrMod) %>% dir.create()
    }
    if(!dir.exists(paste0(outputGraphFolder, calibrMod))){
        paste0(outputGraphFolder, calibrMod) %>% dir.create()
    }
    if(repeatedCV){
        R_end <- '_repeatedcv.R'
        if(!file.exists(paste0(outputFolder, '/result_', calibrMod, '/repeatedcv'))){
            paste0(outputFolder, 'result_', calibrMod, '/repeatedcv') %>% dir.create()
        }
    }else{
        R_end <- '.R'
    }
    
    #-----------
    station <- factor(c('Basel','Cochem','Lobith','Borgharen','Maxau','Wuerzburg'))
    trainStation_i <- which(station==trainStation)
    testStation_i <- which(station==testStation)
    print(paste0('Train station: ', station[trainStation_i]))
    print(paste0('Test station: ', station[testStation_i]))
    
    
    #------------2. Determine optimal parameter----------
    # call function determineParam(): 
    # Determine the optimal parameter based on either the min OOB RMSE 
    # or the min cv errors:
    station_i <- trainStation_i
    source(paste0('function_2_RF_2_determineParameter', R_end))
    
    outputFolder <- '../data/analysis/'
    print(paste0('Optimal parameter for ', station[station_i]))
    optParam <- determineParam(station_i) %>% as.numeric()
    optParam <- optParam %>% as.data.frame() %>% t()
    colnames(optParam) <- determineParam(station_i) %>% names()
    optParam <- optParam %>% as.data.frame()
    outputFolder <- '../data/analysis/testTransferability/'
    # optParam
    
    #---------- train the RF using the trainStation (station_i)------------
    station_i <- trainStation_i
    # source('function_1_readData_excludeChannelStorage_forTrans.R')   # streamflow (m/day)
    source('function_1_readData_excludeChannelStorage.R')     # streamflow depth (cms)
    print(paste0('Build the RF model for ', station[station_i], ' (m/d)'))
    optimal_ranger <- ranger(
        formula         = res ~ ., 
        data            = df_train, 
        num.trees       = optParam$ntrees,
        mtry            = optParam$mtry,
        min.node.size   = 5,
        seed = 123,
        importance = 'impurity'          # 'permutation'
    )
    
    #-------test the RF for another location--------
    station_i <- testStation_i
    print(paste0('Test station: ', station[station_i], ' (m/d)'))
    if(station[station_i]%in%onlytest){
        stationInfo <- read.csv('../data/rawData/stationLatLon.csv') %>% 
            mutate(plotName=plotName %>% as.character())
        stationInfo <- stationInfo[(stationInfo$station)%in%station,]
    }
    # source('function_1_readData_excludeChannelStorage_forTrans.R')   # streamflow (m/day)
    source('function_1_readData_excludeChannelStorage.R')     # streamflow depth (cms)
    
    if(station[station_i]=='Borgharen'){
        rf.result <- all %>% 
            filter(!yr%in%c(1991,1992,1993)) %>% 
            mutate(mod_res=predict(optimal_ranger, all %>% 
                                       filter(!yr%in%c(1991,1992,1993))) %>% 
                       predictions()) %>%
            mutate(pcr_corrected=pcr+mod_res)
    }else rf.result <- all %>% 
        mutate(mod_res=predict(optimal_ranger, all) %>% predictions()) %>% 
        mutate(pcr_corrected=pcr+mod_res)
    station_i <- which(station==trainStation)
    rf.eval <- rf.result %>%
        group_by(datatype) %>%
        summarise(
            RMSE=(((res)^2) %>% mean(na.rm=T) %>% sqrt),
            RMSE_corrected=(((mod_res)^2) %>% mean(na.rm=T) %>% sqrt),
            MAE=res %>% abs %>% mean(na.rm=T),
            MAE_corrected=mod_res %>% abs %>% mean(na.rm=T)) %>% 
        mutate(trainStation=stationInfo$station[station_i]) %>% 
        mutate(plotTitle=plotTitle)
    
    rf.eval_r <- rf.result %>% 
        group_by(datatype) %>% 
        summarise(KGE=KGE(sim = pcr, obs = obs,
                          s = c(1,1,1), na.rm = TRUE, method = "2009"),
                  KGE_corrected=KGE(sim = pcr_corrected, obs = obs,
                                    s = c(1,1,1), na.rm = TRUE, method = "2009"),
                  NSE = NSE(sim = pcr, obs = obs, 
                            na.rm = T),
                  NSE_corrected = NSE(sim = pcr_corrected, obs = obs, 
                                      na.rm = T),
                  nRMSE=(((res)^2) %>% mean(na.rm=T) %>% sqrt)/mean(obs),
                  nRMSE_corrected=(((mod_res)^2) %>% mean(na.rm=T) %>% sqrt)/mean(obs)) %>% 
                  # nMAE=(res %>% abs %>% mean(na.rm=T))/mean(obs),
                  # nMAE_corrected=(mod_res %>% abs %>% mean(na.rm=T))/mean(obs),
                  # Rsquared=(lm(pcr~obs) %>% summary)$adj.r.squared,
                  # Rsquared_corrected=(lm(pcr_corrected~obs) %>% summary)$adj.r.squared) 
        mutate(trainStation=stationInfo$station[station_i]) %>% 
        mutate(plotTitle=plotTitle)
    # 20200512 Log:
    # The result does not look good because no catchment characteristic is included in the RF.
    
    rf.eval_gather_r <- gather(rf.eval_r %>% mutate(datatype=ifelse(datatype=='train',
                                                                    'train period (1981-1990)', 'test period (1991-2000)')), 
                               key='key', value='value', 
                               -c('datatype','trainStation','plotTitle')) %>%
        mutate(model=ifelse(grepl('corrected', key), 'RF-corrected', 'PCR-GLOBWB')) %>% 
        mutate(gof=key) %>% 
        separate(., gof,'_') %>% 
        rename('gof'='_')
    
    rf.eval_gather_r$gof_f <- factor(rf.eval_gather_r$gof, 
                                     levels=c('KGE','NSE','Rsquared','nRMSE','nMAE'))
    ggplot(data = rf.eval_gather_r, aes(x=gof_f, y=value, fill=model))+
        geom_col(position = 'dodge')+
        facet_grid(.~datatype, scale='free')+
        theme_gray(base_size = 14)+
        labs(title = paste0('Model performance at ', station[testStation_i],
                            '\n(trained at ', station[station_i], ')'), y='GOF value', x='GOF')+
        scale_fill_manual(values=c("#00BFC4", "#F8766D"))+
        geom_text(aes(x=gof_f, y=value,
                      label=round(value, digits=2)),
                  position=position_dodge(width = 1),
                  size=3.5)
    print(paste0(outputGraphFolder, calibrMod, '/', 
                 trainStation, '_', testStation, '_gof.tiff'))
    ggsave(paste0(outputGraphFolder, calibrMod, '/', 
                  trainStation, '_', testStation, '_gof.tiff'), dpi = 300,
           width = 8, height = 5)

    # rf.eval_gather <- gather(rf.eval%>% mutate(datatype=ifelse(datatype=='train',
    #                                                            'train period (1981-1990)', 'test period (1991-2000)')), key='key', value='value', 
    #                          -c('datatype', 'plotTitle','trainStation')) %>%
    #     mutate(gof=ifelse(grepl('RMSE', key), 'RMSE', 'MAE')) %>% 
    #     mutate(model=ifelse(grepl('corrected', key), 'RF-corrected', 'pcr-globwb'))
    # 
    # ggplot(data = rf.eval_gather, aes(x=gof, y=value, fill=model))+
    #     geom_col(position = 'dodge')+
    #     facet_grid(.~datatype, scale='free')+
    #     theme_gray(base_size = 15)+
    #     labs(title = paste0('Model performance at ', station[testStation_i], 
    #                         '\n(trained at ', station[station_i], ')'), y='GOF value (cms)')+
    #     scale_fill_manual(values=c("#00BFC4", "#F8766D"))+
    #     geom_text(aes(x=gof, y=value,
    #                   label=round(value, digits=4)),
    #               position=position_dodge(width = 1),
    #               size=4.5)
    # print(paste0(outputGraphFolder, calibrMod, '/', 
    #              trainStation, '_', testStation, '_gof_abs.tiff'))
    # ggsave(paste0(outputGraphFolder, calibrMod, '/', 
    #               trainStation, '_', testStation, '_gof_abs.tiff'), dpi = 300,
    #        width = 6, height = 4)
    print('---------------------------------')
    
}
#-------Borgharen ts----------
# Data is missing (1991-1993)
library(forecast)
rf.result$obs %>% ts(., frequency = 365.25, start=c(1981,1)) %>% ggtsdisplay()