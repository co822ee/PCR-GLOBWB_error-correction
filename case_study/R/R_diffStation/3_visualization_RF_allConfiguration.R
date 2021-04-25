source('function_0_loadLibrary.R')
source('function_3_vis_0_setUpDirectory.R')
dir <- c(paste0('../data/analysis/', c('result_calibrated/', 'result_uncalibrated/'))) %>% 
  as.list
# configKey <- list('PCRcalibr-RFd','PCRun-RFd','PCRcalibr-RFds','PCRun-RFds')
# bmL <- list(T,T,F,F)
configKey <- c('RFd-lagged','RFd_s','RFd-lagged_s')

files <- lapply(dir, list.files, pattern='rf_eval')
csvFiles <- lapply(dir, paste0, files[[1]])
rf.eval_calib <- lapply(csvFiles[[1]], read.csv, header=T)
rf.eval_uncalib <- lapply(csvFiles[[2]], read.csv, header=T)
rf.eval_calib[[1]] %>% str
rf.eval_calib[[1]] %>% dim


# rf.eval <- rf.eval_calib
# pcr_config <- "PCR calibrated"  #"PCR uncalibrated"
sort_fun <- function(rf.eval, pcr_config){
  for(i in seq_along(rf.eval)){
    rf.eval[[i]] <- rf.eval[[i]] %>% 
      mutate(config=configKey[i])
  }
  eval_all <- do.call(rbind, rf.eval) %>% 
    select(datatype, station, plotTitle, config,
           KGE, KGE_corrected, NSE, NSE_corrected) %>% 
    mutate(config=config %>% 
             factor(., levels = c('RFd-lagged','RFd_s','RFd-lagged_s')),
           pcr_config = pcr_config) 
}
rf.eval_calib <- sort_fun(rf.eval_calib, "PCR calibrated")
rf.eval_uncalib <- sort_fun(rf.eval_uncalib, "PCR uncalibrated")
eval_all <- rbind(rf.eval_calib, rf.eval_uncalib)

#----------- GOF: Only corrected--------------
#!------- Model performance --------
eval_allG <- eval_all %>% 
    gather(., 'gof','value', -c('datatype','station', 'plotTitle', 
                                'config','pcr_config')) %>% 
    mutate(gof_col=ifelse(grepl('corrected', gof), 
                          'RFcorrected', 'purePCR')) #pcr_config=sapply(strsplit(config %>% as.character(), "-"), "[", 1) %>% 
  # factor(levels=c('PCRun', 'PCRcalibr'))
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
           aes(x=station, y=value), col='black',   #col=pcr_config 
           position = 'dodge', fill='transparent', lwd=1.1)+
  facet_grid(gof~pcr_config, scale='free_x')+
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
  )+
  # scale_fill_manual(
  #   values=c('chartreuse2','forestgreen',
  #            'lightseagreen','midnightblue'))+
  # scale_color_manual(
  #   values=c('olivedrab1', 'cadetblue1'))+
  labs(     #title = 'Model performance at different stations', 
    subtitle = '1991-2000 (validation period)', 
    y='GOF value',
    color=paste0('PCR without RF-correction'), 
    fill=paste0('Models'))
ggsave('../graph/RFresult_all/gof_abs_new_test_noTitles.tiff', dpi = 300,
       width = 7.5, height = 6)

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
           aes(x=station, y=value), col='black',   #col=pcr_config 
           position = 'dodge', fill='transparent', lwd=1.1)+
  facet_grid(gof~pcr_config, scale='free_x')+
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
  )+
  # scale_fill_manual(
  #   values=c('chartreuse2','forestgreen',
  #            'lightseagreen','midnightblue'))+
  # scale_color_manual(
  #   values=c('olivedrab1', 'cadetblue1'))+
  labs(     #title = 'Model performance at different stations', 
       subtitle = '1981-1990 (train period)', 
       y='GOF value',
       color=paste0('PCR without RF-correction'), 
       fill=paste0('Models'))
ggsave('../graph/RFresult_all/gof_abs_new_train_noTitles.tiff', dpi = 300,
       width = 6.5, height = 8)
#!-------------Variable importance-------------
fileName <- lapply(dir, list.files, pattern='importance')[[1]]
csvFiles <- lapply(dir, paste0, fileName)
vi <- lapply(csvFiles, read.csv, header=T)

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
    gather(., key='pred','value', c(all_of(feature),-'day'))

  corDatac <- corData1 %>% 
    group_by(pred) %>%      #datatype
    summarise(cor=cor(value, res), 
              corSign=ifelse(cor>0, '+', '-')) %>% 
    as.data.frame() %>% 
    inner_join(., imp_df, by='pred') %>% 
    mutate(importance=sqrt(importance))
  
  p1 <-  ggplot(corDatac %>% 
                  tidyr::gather('key','value', c('cor','importance')) %>% 
                  mutate(key=case_when(key=='cor'~'(1) cor',
                                       key=='importance'~'(2) importance'))
  )+
    geom_col(aes(reorder(pred, c(value[key=='(1) cor'], 
                                 value[key=='(2) importance']*1000)), 
                 value),
             position = 'dodge', fill='khaki') +
    coord_flip() +
    theme_light()+
    facet_grid(.~key, scale='free')+
    theme(
      axis.text.y = element_text(size = 15.5),
          axis.title = element_text(size = 0),
          axis.text.x = element_text(size = 14),
          strip.text.x = element_text(size = 15, color = 'black'),
          strip.background = element_rect(colour = "transparent", fill = "white"),
          strip.text.y = element_text(size = 15, color = 'black'),
          title = element_text(size = 16),
          plot.subtitle = element_text(size = 16))+
    labs(x=NULL, y=NULL, 
         title=plotTitle, 
         subtitle = paste0(list('(a) ','(b) ', '(a) ', '(b) '), configKey)[[i]])     #mean decrease in node impurity (sd)
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
      plotTitle <- paste0(c('A. ', 'B. ', 'C. ')[station_i], plotTitle)
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
      plotTitle <- paste0(c('A. ', 'B. ', 'C. ')[station_i], plotTitle)
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