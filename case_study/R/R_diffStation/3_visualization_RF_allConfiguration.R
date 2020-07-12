source('function_0_loadLibrary.R')
# source('function_2_RF_0_setUpDirectory.R')
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

#----------- GOF: Only corrected--------------
#------- Model performance --------
eval_allG <- eval_all %>% 
    gather(., 'gof','value', -c('datatype','station', 'plotTitle', 
                                'config')) %>% 
    mutate(gof_col=ifelse(grepl('corrected', gof), 
                          'RFcorrected', 'purePCR'),
           pcr_config=sapply(strsplit(config %>% as.character(), "-"), "[", 1) %>% 
             factor(levels=c('PCRun', 'PCRcalibr')))
#------test----------
ggplot(data = eval_allG %>% 
         filter(datatype=='test') %>% 
         filter(grepl('corrected', gof)) %>% 
         mutate(gof=sub('_corrected','', gof)) %>% 
         mutate(gof=factor(gof, levels = c('KGE','NSE','Rsquared',
                                           'nRMSE','nMAE'))), 
       aes(x=station, y=value, fill=config))+
  geom_col(position = 'dodge')+
  #pure PCR model
  geom_col(data = eval_allG %>% 
             filter(datatype=='test') %>% 
             filter(!grepl('corrected', gof)) %>% 
             mutate(gof=factor(gof, levels = c('KGE','NSE','Rsquared',
                                               'nRMSE','nMAE'))), 
           aes(x=station, y=value, col=pcr_config), 
           position = 'dodge', fill='transparent', lwd=1.1)+
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
    values=c('chartreuse2','forestgreen',
             'lightseagreen','midnightblue'))+
  scale_color_manual(
    # labels = c('PCRun', 'PCRun-RFd', 'PCRun-RFds', 
    #            'PCRcalibr', 'PCRcalibr-RFd','PCRcalibr-RFds'), 
    # name = 'models',
    values=c('olivedrab1', 'cadetblue1'))+
  labs(
    # title = 'Model performance at different stations', 
    #    subtitle = '1991-2000 (test period)', 
       y='',
       color=paste0('PCR without RF-correction'), 
       fill=paste0('Model configurations'))
ggsave('../graph/RFresult_all/gof_abs_new_test_noTitles.tiff', dpi = 300,
       width = 6.5, height = 7.2)

#------train----------
ggplot(data = eval_allG %>% 
         filter(datatype=='train') %>% 
         filter(grepl('corrected', gof)) %>% 
         mutate(gof=sub('_corrected','', gof)) %>% 
         mutate(gof=factor(gof, levels = c('KGE','NSE','Rsquared',
                                           'nRMSE','nMAE'))), 
       aes(x=station, y=value, fill=config))+
  geom_col(position = 'dodge')+
  #pure PCR model
  geom_col(data = eval_allG %>% 
             filter(datatype=='train') %>% 
             filter(!grepl('corrected', gof)) %>% 
             mutate(gof=factor(gof, levels = c('KGE','NSE','Rsquared',
                                               'nRMSE','nMAE'))), 
           aes(x=station, y=value, col=pcr_config), 
           position = 'dodge', fill='transparent', lwd=1.1)+
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
    
    panel.spacing=unit(1, "lines"),
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
    values=c('chartreuse2','forestgreen',
             'lightseagreen','midnightblue'))+
  scale_color_manual(
    # labels = c('PCRun', 'PCRun-RFd', 'PCRun-RFds', 
    #            'PCRcalibr', 'PCRcalibr-RFd','PCRcalibr-RFds'), 
    # name = 'models',
    values=c('olivedrab1', 'cadetblue1'))+
  labs(title = 'Model performance at different stations', 
       subtitle = '1981-1990 (train period)', 
       y='GOF value',
       color=paste0('PCR without RF-correction'), 
       fill=paste0('Models'))
ggsave('../graph/RFresult_all/gof_abs_new_train.tiff', dpi = 300,
       width = 6.5, height = 8)
#-------------Variable importance-------------
fileName <- lapply(dir, list.files, pattern='importance')[[1]]
csvFiles <- lapply(dir, paste0, fileName)
vi <- lapply(csvFiles, read.csv, header=T)
# names(vi) <- ifelse(bmL, 'benchmarkRF','RF')

stationInfo <- read.csv('../data/rawData/stationLatLon.csv') %>% 
  mutate(plotName=plotName %>% as.character())
station <- list.files('../data/preprocess/calibrated/','pcr_') %>% 
  sapply(., function(x) substr(x, 5, nchar(x)-4)) %>% as.character()
stationInfo <- stationInfo[(stationInfo$station %>% tolower)%in%station,] %>% 
  mutate(station=factor(station, levels = c('Basel', 'Cochem', 'Lobith')))
station <- factor(c('Basel', 'Cochem', 'Lobith') , levels = c('Basel', 'Cochem', 'Lobith'))
trainPeriod <- 1981:1990
testPeriod <- 1991:2000

# This is obtained from 3_visualization_RF.R
viPlot <- function(){
  imp <- vi[[i]][as.character(station[station_i])]
  imp_df <- data.frame(pred=vi[[i]]$names, importance=imp) %>% 
    mutate(pred=as.character(pred))
  names(imp_df)[2] <- 'importance'
  corData1 <-  all %>% 
    group_by(datatype) %>% 
    mutate(res=(res-mean(res))/sd(res)) %>% as.data.frame() %>% 
    gather(., key='pred','value', c(all_of(feature),-'day')) #%>% 
    # mutate(predtype=case_when(pred%in%snownames ~ 'snow', 
    #                           pred%in%stornames ~ 'storage',
    #                           pred%in%waternames ~ 'water',
    #                           pred%in%wdlnames ~ 'withdrawal',
    #                           pred%in%etnames ~ 'ET',
    #                           pred%in%c('t','p') ~ 'meteor'))
  
  corDatac <- corData1 %>% 
    group_by(pred) %>%      #datatype
    summarise(cor=cor(value, res), 
              corSign=ifelse(cor>0, '+', '-')) %>% 
    as.data.frame() %>% 
    inner_join(., imp_df, by='pred') %>% 
    mutate(importance=sqrt(importance))
  
  p1 <-  ggplot(corDatac %>% gather('key','value', c('cor','importance'))
  )+
    geom_col(aes(reorder(pred, c(value[key=='cor'], 
                                 value[key=='importance']*1000)), 
                 value),
             position = 'dodge', fill='khaki') +
    # geom_col(aes(reorder(pred, importance), cor))+
    # Make the predictor names inside
    # geom_text(aes(label = c(pred[key=='cor'], rep('', length(pred[key=='cor']))), 
    #               x = pred, y = -Inf, hjust = 0), 
    #           size=6)+    #hjust = 0, vjust = 1
    coord_flip() +
    theme_light()+
    facet_grid(.~key, scale='free')+
    # geom_text(aes(label = key), x = Inf, y = Inf, hjust = 1.5, vjust = -1.5)+
    theme(
      axis.text.y = element_text(size = 15.5),
      # axis.text.y = element_blank(),
          axis.title = element_text(size = 0),
          axis.text.x = element_text(size = 14),
          strip.text.x = element_text(size = 15, color = 'black'),
          strip.background = element_rect(colour = "transparent", fill = "white"),
          strip.text.y = element_text(size = 15, color = 'black'),
          # strip.background = element_blank(),
          # strip.text = element_blank(),
          title = element_text(size = 16),
          plot.subtitle = element_text(size = 16))+
    labs(x=NULL, y=NULL, 
         title=plotTitle, subtitle = configKey[[i]])     #mean decrease in node impurity (sd)
    # ggtitle(paste0(configKey[[i]], ": \n", plotTitle))
  return(p1)
}

pListBM <- vector("list", (ncol(vi[[1]])-1)*length(vi)/2)
pListRF <- vector("list", (ncol(vi[[1]])-1)*length(vi)/2)
countBM <- 1
countRF <- 1
for(i in seq_along(calibrL)){
  if(calibrL[[i]]){
    calibrMod <- 'calibrated'
  }else calibrMod <- 'uncalibrated'
  
  
  for(station_i in seq_along(station)){
    if(bmL[[i]]){
      # Benchmark models
      source('function_1_readData_excludeChannelStorage_benchmarkModel.R')
      plotTitle <- stationInfo$plotName[which(stationInfo$station==station[station_i])]
      print(paste0(configKey[[i]], ' (benchmarkRF): ', plotTitle))
      if(countBM<4){
        pListBM[[countBM]] <- viPlot()
        countBM=countBM+1
      }else{
        pListBM[[countBM]] <- viPlot()+
          labs(title = '')
        countBM=countBM+1
      }
      
    }else{
      # RF models with state variables involved
      source('function_1_readData_excludeChannelStorage.R')
      plotTitle <- stationInfo$plotName[which(stationInfo$station==station[station_i])]
      print(paste0(configKey[[i]], ' (RF): ', plotTitle))
      
      if(countRF<4){
        pListRF[[countRF]] <- viPlot()
        countRF=countRF+1
      }else{
        pListRF[[countRF]] <- viPlot()+
          labs(title = '')
        countRF=countRF+1
      }
    }
  }
}
pListBM$ncol <-3 
pListRF$ncol <-3

tiff(paste0("../graph/RFresult_all/VI&cor_bmRF.tiff"), 
     height=8, width=20, units='in', res=300)
do.call(grid.arrange, pListBM)
dev.off()

tiff(paste0("../graph/RFresult_all/VI&cor_RF.tiff"), 
     height=12, width=25, units='in', res=300)
do.call(grid.arrange, pListRF)
dev.off()
# if(!dir.exists('../graph/RFresult_all')){
#   dir.create('../graph/RFresult_all')
# }

# ggplot(data = eval_allG %>% filter(station=='Basel') %>% 
#            filter(grepl('RMSE', gof)|grepl('Rsquared', gof)|grepl('KGE', gof)), 
#        aes(x=gof, y=value, fill=gof_col))+
#     geom_col(position = 'dodge')+
#     facet_grid(config~datatype)+
#     theme_gray(base_size = 16)+
#     scale_fill_manual(values=c("#00BFC4", "#F8766D"))+
#     labs(title = 'Model performance at different stations', y='GOF value (m/day)')
    

#-----------archive--------
## Model performance improvement relative to the PCR-GLOBWB without bias correction by RF.
## 
# eval_all_r <-       
#   (eval_all %>% select(matches('corrected'))-(eval_all %>% select('KGE','NSE','nRMSE','Rsquared')))/(eval_all %>% select('KGE','NSE','nRMSE','Rsquared'))*100
# names(eval_all_r) <- eval_all_r %>% names() %>% lapply(., sub, pattern='_corrected',replacement='') %>% unlist
# eval_all_r <- ((eval_all_r %>% t())*c(1,1,-1,1)) %>% t() %>% as.data.frame()
# eval_all_r <- eval_all_r %>% 
#   mutate(datatype=eval_all$datatype,
#          station=eval_all$station,
#          plotTitle=eval_all$plotTitle,
#          config=eval_all$config) %>% 
#   mutate(rf_config=ifelse(grepl('bm', config), 'benchmark', 'RF_pcrState'),
#          calibr_config=ifelse(grepl('calibr', config), 'PCR_calibr', 'PCR_uncalibr'))
# eval_all_rG <- 
#   gather(eval_all_r, 'GOF', 'value', 
#          c('KGE','NSE','nRMSE','Rsquared')) %>% 
#   mutate(GOF=factor(GOF, levels = c('KGE','NSE','Rsquared',
#                                     'nRMSE')))
# ggplot(data = eval_all_rG, 
#        aes(x=station, y=value, fill=config))+
#   geom_col(position = 'dodge')+
#   facet_grid(GOF~datatype, scale='free')+
#   theme_gray(base_size = 16)+
#   scale_fill_manual(values=c('olivedrab2','olivedrab',
#                              'lightskyblue','midnightblue'))+
#   labs(title = 'Model performance at different stations', 
#        y=paste0('relative improvement of GOF value (%) \n compared to pure PCR predictions'))
# ggsave('../graph/RFresult_all/gof_rel.tiff', dpi = 300,
#        width = 8, height = 7)

# ggplot(data = eval_allG %>% 
#          filter(grepl('corrected', gof)) %>% 
#          mutate(gof=sub('_corrected','', gof)) %>% 
#          mutate(gof=factor(gof, levels = c('KGE','NSE','Rsquared',
#                                            'nRMSE','nMAE'))), 
#        aes(x=station, y=value, fill=config))+
#   geom_col(position = 'dodge')+
#   facet_grid(gof~datatype, scale='free')+
#   theme_gray(base_size = 16)+
#   scale_fill_manual(values=c('olivedrab2','olivedrab',
#                              'lightskyblue','midnightblue'))+
#   labs(title = 'Model performance at different stations', y='GOF value')
# ggsave('../graph/RFresult_all/gof_abs.tiff', dpi = 300,
#        width = 8, height = 7)

# ggplot(data = eval_allG %>% 
#          filter(grepl('corrected', gof)) %>% 
#          mutate(gof=sub('_corrected','', gof)) %>% 
#          mutate(gof=factor(gof, levels = c('KGE','NSE','Rsquared',
#                                            'nRMSE','nMAE'))), 
#        aes(x=station, y=value, fill=config))+
#   geom_col(position = 'dodge')+
#   #pure PCR model
#   geom_col(data = eval_allG %>% 
#              filter(!grepl('corrected', gof)) %>% 
#              mutate(gof=factor(gof, levels = c('KGE','NSE','Rsquared',
#                                                'nRMSE','nMAE'))), 
#            aes(x=station, y=value, col=pcr_config), 
#            position = 'dodge', fill='transparent', lwd=1.1)+
#   facet_grid(gof~datatype, scale='free')+
#   theme_light()+
#   theme(
#     axis.text.y = element_text(size = 12),
#     # axis.text.y = element_blank(),
#     axis.title = element_text(size = 12),
#     axis.text.x = element_text(size = 12),
#     strip.text.x = element_text(size = 15, color = 'black'),
#     strip.background = element_rect(colour = "grey", fill = "white"),
#     strip.text.y = element_text(size = 15, color = 'black'),
#     # strip.background = element_blank(),
#     # strip.text = element_blank(),
#     title = element_text(size = 17),
#     legend.text = element_text(size = 12),
#     legend.title = element_text(size = 15))+
#   scale_fill_manual(values=c('chartreuse2','forestgreen',
#                              'lightseagreen','midnightblue'))+
#   scale_color_manual(values=c('olivedrab1', 'cadetblue1'))+
#   labs(title = 'Model performance at different stations', y='GOF value',
#        color=paste0('Pure PCR-GLOBWB \nwithout RF-correction'), fill='Model configurations')
# ggsave('../graph/RFresult_all/gof_abs_new.tiff', dpi = 300,
#        width = 8.5, height = 7)



# PCR only
# pcr[[i]] <- rf.eval[[i]] %>% 
#     select(!matches('_corrected')) %>% 
#     mutate(calibr_config=ifelse(calibrL[[i]], 'PCR_calibr', 'PCR_uncalibr'))
# # RF bias correction
# rf_biasCorrect[[i]] <- rf.eval[[i]] %>% 
#     select(matches('_corrected'),'datatype','station','plotTitle') %>% 
#     mutate(config=ifelse(bmL[[i]], 'benchmark', 'RF_pcrState'),
#            key=configKey[[i]],
#            calibr_config=ifelse(calibrL[[i]], 'PCR_calibr', 'PCR_uncalibr'))
# }
# pcr_all <- do.call(rbind, pcr)
# rf_all <- do.call(rbind, rf_biasCorrect)
# pcr_all <- pcr_all %>% distinct
# gof_names <- rf_all %>% select(matches('corrected')) %>% names() %>% 
#     lapply(sub, pattern="_corrected", replacement='') %>% unlist
# names(rf_all)[rf_all %>% names() %>% lapply(., grepl, pattern='corrected') %>% unlist] <- gof_names
# 
# eval_all <- left_join(rf_all, pcr_all, by=c('calibr_config','station','plotTitle','datatype'))
