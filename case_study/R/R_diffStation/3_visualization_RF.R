# input:
calibrMod <- 'calibrated'      # calibrated    uncalibrated
# station_i <- 1                         # station id
trainPeriod <- 1981:1990
testPeriod <- 1991:2000
# plotTitle <- stationInfo$plotName[station_i]
repeatedCV <- F # whether repeated two-fold cv
benchmark <- F  # benchmark model or not

source('function_0_loadLibrary.R')
source('function_2_RF_0_setUpDirectory.R')


if(repeatedCV){
    rf.eval <- read.csv(paste0(outputFolder, 'result_', calibrMod,'/repeatedcv/rf_eval.csv'))
    rf.eval_r <- read.csv(paste0(outputFolder, 'result_', calibrMod,'/repeatedcv/rf_eval_r.csv'))
    vi <- read.csv(paste0(outputFolder, 'result_', calibrMod,'/repeatedcv/variable_importance.csv'))
}else{
    rf.eval <- read.csv(paste0(outputFolder, 'result_', calibrMod,'/rf_eval.csv'))
    rf.eval_r <- read.csv(paste0(outputFolder, 'result_', calibrMod,'/rf_eval_r.csv'))
    vi <- read.csv(paste0(outputFolder, 'result_', calibrMod,'/variable_importance.csv'))
}


#------------KGE--------------

rf.eval %>% str
rf.eval$station %>% levels
rf.eval <- rf.eval %>% 
    mutate(station=factor(station, 
                          levels = c('Basel','Maxau','Lobith','Cochem','Wuerzburg'))) %>% 
    mutate(plotTitle=factor(plotTitle,
                            levels = c('Basel (Rhine)','Maxau (Rhine)', 'Lobith (Rhine)',
                                       'Cochem (Moselle)', 'Wuerzburg (Main)')))

rf.eval_gather <- gather(rf.eval, key='key', value='value', 
                         -c('datatype', 'plotTitle','station')) %>%
    mutate(gof=ifelse(grepl('RMSE', key), 'RMSE', 'MAE')) %>% 
    mutate(model=ifelse(grepl('corrected', key), 'RF-corrected', 'pcr-globwb'))

# ggplot(data = rf.eval_gather)+
#     geom_line(data = rf.eval_gather %>% filter(model=='RF-corrected'), 
#               group=1, aes(x=station, y=value, col=model))+
#     geom_line(data = rf.eval_gather %>% filter(model=='pcr-globwb'), 
#               group=1, aes(x=station, y=value, col=model))+
#     facet_grid(gof~datatype, scale='free_y')

ggplot(data = rf.eval_gather, aes(x=station, y=value, fill=model))+
    geom_col(position = 'dodge')+
    facet_grid(gof~datatype, scale='free')+
    theme_gray(base_size = 16)+
    labs(title = 'Model performance at different stations', y='GOF value (m/day)')+
    scale_fill_manual(values=c("#00BFC4", "#F8766D"))+
    geom_text(aes(x=station, y=value,
                  label=round(value, digits=4)),
              position=position_dodge(width = 1),
              size=4.5)
ggsave(paste0(outputGraphFolder, calibrMod, '/gof_abs.tiff'), dpi = 300,
       width = 8, height = 4)

ggplot(data = rf.eval_gather, aes(x=station, y=value, fill=model))+
    geom_col(position = 'dodge')+
    facet_grid(gof~datatype, scale='free')+
    theme_gray(base_size = 16)+
    labs(title = 'Model performance at different stations', y='GOF value (m/day)')+
    scale_fill_manual(values=c("#00BFC4", "#F8766D"))
ggsave(paste0(outputGraphFolder, calibrMod, '/gof_abs_nText.tiff'), dpi = 300,
       width = 8, height = 4)

#----------relative performance---------
rf.eval_r <- rf.eval_r %>%
    mutate(station=factor(station, 
                          levels = c('Basel','Maxau','Lobith','Cochem','Wuerzburg'))) %>% 
    mutate(plotTitle=factor(plotTitle,
                            levels = c('Basel (Rhine)','Maxau (Rhine)', 'Lobith (Rhine)',
                                       'Cochem (Moselle)', 'Wuerzburg (Main)')))

rf.eval_gather_r <- gather(rf.eval_r, key='key', value='value', 
                         -c('datatype','station','plotTitle')) %>%
    mutate(model=ifelse(grepl('corrected', key), 'RF-corrected', 'PCR-GLOBWB')) %>% 
    mutate(gof=key) %>% 
    separate(., gof,'_') %>% 
    rename('gof'='_')

rf.eval_gather_r$gof_f <- factor(rf.eval_gather_r$gof, 
                                 levels=c('KGE','Rsquared','nRMSE','nMAE'))
ggplot(data = rf.eval_gather_r, aes(x=station, y=value, fill=model))+
    geom_col(position = 'dodge')+
    facet_grid(gof_f~datatype, scale='free')+
    theme_gray(base_size = 15)+
    labs(title = 'Model performance at different stations', y='GOF value')+
    scale_fill_manual(values=c("#00BFC4", "#F8766D"))+
    geom_text(aes(x=station, y=value,
                  label=round(value, digits=2)),
              position=position_dodge(width = 1),
              size=3)
ggsave(paste0(outputGraphFolder, calibrMod, '/gof.tiff'), dpi = 300,
       width = 8, height = 5)
ggplot(data = rf.eval_gather_r, aes(x=station, y=value, fill=model))+
    geom_col(position = 'dodge')+
    facet_grid(gof_f~datatype, scale='free')+
    theme_gray(base_size = 15)+
    labs(title = 'Model performance at different stations', y='GOF value')+
    scale_fill_manual(values=c("#00BFC4", "#F8766D"))
    # annotate('rect', xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf, fill='transparent',
    #          col='red', lty=2)
ggsave(paste0(outputGraphFolder, calibrMod, '/gof_nText.tiff'), dpi = 300,
       width = 8, height = 5)

#-------- variable importance--------------

etnames <- names(all)[sapply(names(all), 
                             function(x) grepl('ET',x))|sapply(names(all), 
                                                               function(x) grepl('vap',x))|sapply(names(all), 
                                                                                                  function(x) grepl('et',x))]

snownames <- names(all)[sapply(names(all), function(x) grepl('snow',x))]
waternames <- names(all)[sapply(names(all), function(x) grepl('gw',x))|sapply(names(all), function(x) grepl('groundwater',x))|sapply(names(all), function(x) grepl('stor',x))|sapply(names(all), function(x) grepl('flow',x))|sapply(names(all), function(x) grepl('Runoff',x))]
stornames <- names(all)[sapply(names(all), function(x) grepl('Storage',x))]
# stornames <- c(stornames, 'channelStorage')
wdlnames <- names(all)[sapply(names(all), function(x) grepl('Withdrawal',x))|sapply(names(all), function(x) grepl('Consumption',x))]

vi <- vi %>% 
    mutate(predtype=case_when(names%in%snownames ~ 'snow', 
                              names%in%stornames ~ 'storage',
                              names%in%waternames ~ 'water',
                              names%in%wdlnames ~ 'withdrawal',
                              names%in%etnames ~ 'ET',
                              names%in%c('t','p') ~ 'meteor',
                              names=='d'~'time'))
vi_gather <- vi %>% gather('station', 'importance', -names, -predtype) %>% 
    mutate(station=factor(station, levels = c('Basel','Maxau','Lobith','Cochem','Wuerzburg')))%>% 
    group_by(station) %>% 
    mutate(rank=importance %>% desc %>% rank %>% as.numeric()) %>% 
    mutate(rank=ifelse(rank>5,'',rank))
ggplot(vi_gather, 
       aes(reorder(names, importance), sqrt(importance), fill=station)) +
    geom_col(position = 'dodge') +
    coord_flip() +
    facet_grid(predtype~station, scale='free')+
    labs(x='names', y='mean decrease in node impurity (sd)')+
    ggtitle("variable importance")+
    scale_fill_discrete(name = "Station", 
                        labels = c('Basel','Maxau','Lobith','Cochem','Wuerzburg'))+
    geom_text(aes(x=reorder(names, importance), y=sqrt(importance), label=rank),
              size=3, position = position_dodge(0.1))


#-------------VI with correlations between state variables and residuals------
pList <- vector("list", length(station))
for(id in seq_along(station)){
    station_i <- stationOrder[id]
    source(paste0('function_1_readData_excludeChannelStorage',R_B_end))
    plotTitle <- stationInfo$plotName[station_i]
    print(plotTitle)
    imp <- vi[as.character(station[station_i])]
    imp_df <- data.frame(pred=vi$names, importance=imp) %>% 
        mutate(pred=as.character(pred))
    names(imp_df)[2] <- 'importance'
    corData1 <-  all %>% 
        group_by(datatype) %>% 
        mutate(res=(res-mean(res))/sd(res)) %>% as.data.frame() %>% 
        gather(., key='pred','value', c(all_of(feature),-'day')) %>% 
        mutate(predtype=case_when(pred%in%snownames ~ 'snow', 
                                  pred%in%stornames ~ 'storage',
                                  pred%in%waternames ~ 'water',
                                  pred%in%wdlnames ~ 'withdrawal',
                                  pred%in%etnames ~ 'ET',
                                  pred%in%c('t','p') ~ 'meteor'))
    
    corDatac <- corData1 %>% 
        group_by(pred) %>%      #datatype
        summarise(cor=cor(value, res), 
                  corSign=ifelse(cor>0, '+', '-')) %>% 
        as.data.frame() %>% 
        inner_join(., imp_df, by='pred') %>% 
        mutate(importance=sqrt(importance))
    
    pList[[id]] <- ggplot(corDatac %>% 
                              gather('key','value', c('cor','importance'))
    ) +
        geom_col(aes(reorder(pred, c(value[key=='cor'], 
                                     value[key=='importance']*1000)), 
                     value),
                 position = 'dodge', fill='khaki') +
        # geom_col(aes(reorder(pred, importance), cor))+
        coord_flip() +
        facet_grid(.~key, scale='free')+
        labs(x='names', y='mean decrease in node impurity (sd)')+
        ggtitle(plotTitle)
    # scale_fill_discrete(name = "Station", 
    #                     labels = station[station_i])+
    # geom_text(aes(x=reorder(pred, value), y=value, 
    #               label=round(value,2)),
    #           size=3, position = position_dodge(0.1))
    
}
pList$nrow=1
tiff(paste0("../graph/", calibrMod,"/VI&cor.tiff"), 
     height=4, width=15, units='in', res=300)
     # height=4, width=25, units='in', res=300)
do.call(grid.arrange, pList)
dev.off()

# vi_rel <- vi_gather %>% 
#     group_by(station) %>% mutate(relI=importance/sum(importance)) %>% 
#     group_by(station, predtype) %>% summarise(avgRelI=sum(relI)) %>% 
#     group_by(station) %>% mutate(rank=avgRelI %>% desc %>% rank %>% as.numeric()) %>% 
#     mutate(rank=ifelse(rank>3,'',rank))
# 
# 
# # ggplot(vi_rel, aes(reorder(predtype, avgRelI), avgRelI)) +
# #     geom_line(group=1, aes(col=station)) +
# #     # geom_col(aes(fill=station))+
# #     facet_grid(station~., scale='free')+
# #     # coord_flip() +
# #     labs(x='names', y='relative decrease in node impurity')+
# #     ggtitle("variable importance")
# 
# ggplot(vi_rel, 
#        aes(reorder(predtype, avgRelI), avgRelI, group=station)) +
#     geom_line(aes(col=station), lwd=1.2) +
#     # geom_col(aes(fill=station))+
#     # facet_grid(station~., scale='free')+
#     # coord_flip() +
#     labs(x='', y='relative decrease in node impurity')+
#     ggtitle("variable importance")+
#     theme_gray(base_size = 16)+
#     geom_text(aes(x=predtype, y=avgRelI, label=rank),
#               size=5)

#----------old plot (not suitable)-----------------
p1 <- ggplot(data = rf.eval_gather %>% filter(gof=='RMSE')%>% as.data.frame, aes(x=key, y=value))+
    geom_bar(stat="identity", fill='cornflowerblue')+
    labs(y = 'RMSE', title='RF', x='')+
    # ylim(c(0,500))+
    geom_label(aes(x=key, y=value, label=round(value, digits=2)), size=6, fill='transparent')+
    facet_grid(datatype~station)+
    theme_grey(base_size = 13)
p2 <- p1%+%(rf.eval_gather %>% filter(gof=='KGE'))+
    # ylim(c(0,1))+
    labs(y = 'KGE', title = '', x='')
grid.arrange(p1,p2)



#----------tuning result evaluation (old)---------

# hyper_grid %>% 
#     dplyr::arrange(OOB_RMSE) %>%
#     head(10)         
# hyper_grid %>% 
#     dplyr::arrange(two.cv_RMSE) %>%
#     head(10)         
# 
# 
# # hyper_grid %>% 
# #     dplyr::arrange(desc(OOB_R2)) %>%
# #     head(10)         
# # hyper_grid %>% 
# #     dplyr::arrange(two.cv_R2) %>%
# #     head(10)         
# 
# p1 <- hyper_grid %>%
#     filter(ntrees==500) %>% 
#     gather(Metric, RMSE, c(OOB_RMSE, two.cv_RMSE)) %>%
#     ggplot(aes(mtry, RMSE, color = Metric)) +
#     geom_line() +
#     # scale_y_continuous(labels = scales::dollar) +
#     xlab("number of variables sampled as candicates at each split")
# 
# p3 <- hyper_grid %>%
#     filter(ntrees==500) %>% 
#     gather(Metric, R2, c(OOB_R2, two.cv_R2)) %>%
#     ggplot(aes(mtry, R2, color = Metric)) +
#     geom_line()+
#     xlab("number of variables sampled as candicates at each split")
# # labs(title = 'R2')
# 
# p2 <- hyper_grid %>%
#     filter(mtry==5) %>% 
#     gather(Metric, RMSE, c(OOB_RMSE, two.cv_RMSE)) %>%
#     ggplot(aes(ntrees, RMSE, color = Metric)) +
#     geom_line() +
#     # scale_y_continuous(labels = scales::dollar) +
#     xlab("number of trees")
# 
# p4 <- hyper_grid %>%
#     filter(mtry==5) %>% 
#     gather(Metric, R2, c(OOB_R2, two.cv_R2)) %>%
#     ggplot(aes(ntrees, R2, color = Metric)) +
#     geom_line() +
#     xlab("number of trees")
# p1
# p2
# 
# # grid.arrange(p1+theme(legend.position = 'none')+labs(x=''),
# #              p2+theme(legend.position = 'none')+labs(x=''),
# #              p3+theme(legend.position = 'none'),
# #              p4+theme(legend.background = element_rect(fill='transparent',color='transparent'),
# #                       legend.justification=c(1,0), 
# #                       legend.position=c(0.9,0.6),
# #                       # legend.margin = margin(r=0.2, unit="cm"),
# #                       legend.key = element_rect(colour = 'transparent', fill = 'transparent'),
# #                       legend.title=element_blank()), 
# #              nrow=2)
# # 
# # grid.arrange(p1+theme(legend.position = 'none'),
# #              p2+theme(legend.position = 'none')+
# #                  theme(legend.background = element_rect(fill='transparent',color='transparent'),
# #                        legend.justification=c(1,0), 
# #                        legend.position=c(0.9,0.6),
# #                        # legend.margin = margin(r=0.2, unit="cm"),
# #                        legend.key = element_rect(colour = 'transparent', fill = 'transparent'),
# #                        legend.title=element_blank()), 
# #              nrow=2)
# 
# ggplot(hyper_grid, aes(x=ntrees, y=OOB_RMSE, col=factor(mtry)))+
#     geom_line()
# ggplot(hyper_grid, aes(x=ntrees, y=OOB_R2, col=factor(mtry)))+
#     geom_line()+
#     scale_y_reverse()
# 
# ggplot(hyper_grid, aes(x=ntrees, y=two.cv_RMSE, col=factor(mtry)))+
#     geom_line()
# ggplot(hyper_grid, aes(x=ntrees, y=two.cv_R2, col=factor(mtry)))+
#     geom_line()
