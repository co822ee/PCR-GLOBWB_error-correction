source('function_0_loadLibrary.R')
source('function_3_vis_0_setUpDirectory.R')
dir <- c(paste0('../data/analysis/', c('result_calibrated/', 'result_uncalibrated/'))) %>% 
  as.list
# configKey <- list('PCRcalibr-RFd','PCRun-RFd','PCRcalibr-RFds','PCRun-RFds')
# bmL <- list(T,T,F,F)
configKey <- c('RFd-lagged','RFd_s','RFd-lagged_s')
if(!dir.exists("../graph/RFresult_all_ar/")) dir.create("../graph/RFresult_all_ar/")
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
       width = 7.5, height = 6)
