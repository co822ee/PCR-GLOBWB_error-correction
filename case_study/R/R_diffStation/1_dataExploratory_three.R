# input:
calibrMod <- 'calibrated'      # calibrated    uncalibrated
# station_i <- 1                         # station id
trainPeriod <- 1981:1990
testPeriod <- 1991:2000
# plotTitle <- stationInfo$plotName[station_i]
repeatedCV <- F # whether repeated two-fold cv
benchmark <- F  # benchmark model or not   (for baenchmark model, only need to run either calibrated/uncalibrated)

source('function_0_loadLibrary.R')
source('function_2_RF_0_setUpDirectory.R')

#----------------correlation with residuals-------------
#---------scatterplot------
for(station_i in 1:length(station)){
    source(paste0('function_1_readData_excludeChannelStorage', R_B_end))
    plotTitle <- stationInfo$plotName[station_i]
    print(plotTitle)
    predP <- gather(pred %>% inner_join(q, by='datetime'), 
                    'predictors','values', all_of(feature))
    p1 <- ggplot(predP %>% 
                     group_by(predictors) %>% 
                     mutate(cor=cor(values, res), corSign=ifelse(cor>0, '+', '-')), 
                 aes(x=values, y=res))+
        geom_point()+
        labs(x='predictor values',y='residuals (cms)', title = plotTitle)+
        facet_wrap(predictors~., scales = 'free_x')+
        geom_text(aes(label=round(cor, 2), col=corSign,
                      x = Inf, y = Inf), hjust = 1, vjust = 1, size=5)
    p1
    print(paste0('Output tiff: ', outputGraphFolder, calibrMod,'/cor_state_res_', 
                 plotTitle, '.tiff'))
    ggsave(paste0(outputGraphFolder, calibrMod,'/cor_state_res_', plotTitle, '.tiff'), 
           dpi=300, 
           width=10, height=6.5)
    # The correlation values are not different between training and test period.
    # p1 %+% (predP %>% cbind(., datatype=all$datatype) %>% 
    #             filter(datatype=='test') %>% 
    #             group_by(predictors) %>% 
    #             mutate(cor=cor(values, res), corSign=ifelse(cor>0, '+', '-')))
    # ggsave(paste0(paste0('../graph/', calibrMod,'/cor_state_res_', plotTitle, '_test.tiff')), dpi=300, 
    #        width=10, height=6.5)
    # 
    # p1%+%(predP %>% cbind(., datatype=all$datatype) %>% 
    #           filter(datatype=='train') %>% 
    #           group_by(predictors) %>% 
    #           mutate(cor=cor(values, res), corSign=ifelse(cor>0, '+', '-')))
    # ggsave(paste0(paste0('../graph/', calibrMod,'/cor_state_res_train', plotTitle, '_train.tiff')), dpi=300, 
    #        width=10, height=6.5)
    
    # ggplot(predP %>% 
    #            group_by(predictors) %>% 
    #            mutate(cor=cor(values, pcr), corSign=ifelse(cor>0, '+', '-')), 
    #        aes(x=values, y=pcr))+
    #     geom_point()+
    #     labs(x='predictor values',y='pcr predictions (cms)', title = plotTitle)+
    #     facet_wrap(predictors~., scales = 'free_x')+
    #     geom_text(aes(label=round(cor, 2), col=corSign,
    #                   x = Inf, y = Inf), hjust = 1, vjust = 1, size=5)
    # ggsave(paste0(paste0('../graph/', calibrMod,'/cor_state_pcr_', plotTitle, '.tiff')), dpi=300, 
    #        width=10, height=6.5)
    # 
    # ggplot(predP %>% 
    #            group_by(predictors) %>% 
    #            mutate(cor=cor(values, obs), corSign=ifelse(cor>0, '+', '-')), 
    #        aes(x=values, y=obs))+
    #     geom_point()+
    #     labs(x='predictor values',y='observed discharge (cms)', title = plotTitle)+
    #     facet_wrap(predictors~., scales = 'free_x')+
    #     geom_text(aes(label=round(cor, 2), col=corSign,
    #                   x = Inf, y = Inf), hjust = 1, vjust = 1, size=5)
    # ggsave(paste0(paste0('../graph/', calibrMod,'/cor_state_obs_', plotTitle, '.tiff')), dpi=300, 
    #        width=10, height=6.5)
}
#------------boxplot-------------
# pred_class <- rbind(pred[flux] %>% mutate(class='flux') %>% cbind(datatype=all$datatype) %>%
#                         gather('names','value', c(-class, -datatype)),
#                     pred[depth] %>% mutate(class='depth') %>% cbind(datatype=all$datatype) %>%
#                         gather('names','value', c(-class, -datatype)),
#                     pred[volume] %>% mutate(class='volume') %>% cbind(datatype=all$datatype) %>%
#                         gather('names','value', c(-class, -datatype)))
# 
# pred_class <- pred %>% mutate(datatype=all$datatype) %>% select(-datetime) %>% 
#     gather('names','value', -datatype) %>% 
#     mutate(class=case_when(names%in%snownames ~ 'snow', 
#                            names%in%stornames ~ 'storage',
#                            names%in%waternames ~ 'water',
#                            names%in%wdlnames ~ 'withdrawal',
#                            names%in%etnames ~ 'ET',
#                            names%in%c('t','p') ~ 'meteor'))
# 
# 
# ggplot(pred_class, aes(names, value))+
#     geom_boxplot()+
#     xlab('')+
#     ylab('depth (m)')+
#     coord_flip()+
#     labs(title=plotTitle)+
#     facet_grid(class~datatype, scales = 'free_y')+
#     theme_grey(base_size = 13)
# 
# 
# ggplot(all %>% select(all_of(qVar), 'datatype') %>% 
#            gather('names','value',-datatype), aes(names, value))+
#     geom_boxplot()+
#     xlab('')+
#     labs(title=plotTitle)+
#     facet_grid(.~datatype, scales = 'free')+
#     theme_grey(base_size = 13)

# p1 <- ggplot(pred_class %>% filter(class=='depth'))+
#     geom_boxplot(aes(x=names, y=value))+
#     # facet_wrap(datatype~., scale='free_x')+
#     xlab('')+
#     ylab('depth (m)')+
#     facet_wrap(class~., scales = 'free', nrow=3)+
# 
# p2 <- p1%+%(pred_class %>% filter(class=='flux'))+
#     theme(axis.text.x = element_text(angle=15))+
#     ylab('flux (m/day)')
# 
# p3 <- p1%+%(pred_class %>% filter(class=='volume'))+
#     ylab('volume (m^3)')
# 
# grid.arrange(p1,p2,p3, ncol=1)


# p1%+%(all %>% select(all_of(qVar), 'datatype') %>% 
#           gather('names','value',-datatype))




#----------time series of predictors----------
for(station_i in 1:length(station)){
    source(paste0('function_1_readData_excludeChannelStorage', R_B_end))
    plotTitle <- stationInfo$plotName[station_i]
    print(plotTitle)
    if(benchmark){
        r <- 3
        j=1
        print(feature[(1+r*(j-1)):(r*j)])
        predP <- gather(pred %>% inner_join(q, by='datetime') %>% 
                            select(all_of(feature[(1+r*(j-1)):(r*j)])) %>% 
                            mutate(day=as.Date(ymd$d),
                                   datatype=all$datatype), 
                        'predictors','values', all_of(feature[(1+r*(j-1)):(r*j)]))
        
        p1 <- ggplot(data=predP, 
                     aes(x=day, y=values))+
            geom_line(col='grey')+
            geom_line(data=predP %>%
                          group_by(datatype,day,predictors) %>%
                          mutate(meanV=mean(values)),
                      aes(x=day, y=meanV), col='blue')+
            
            geom_ribbon(data=predP %>%
                            group_by(datatype,day,predictors) %>%
                            mutate(meanV=mean(values),
                                   sdU=meanV+sd(values),
                                   sdL=meanV-sd(values)),
                        aes(ymin=sdL, ymax=sdU), 
                        fill='light blue', alpha=0.1)+
            facet_grid(predictors~datatype, scales = 'free_y')+
            labs(x='month', y='normalized predictor values', title = plotTitle)+
            scale_x_date(date_labels = '%b', date_breaks = '1 month')
        print(p1)
        print(paste0('Output tiff: ', outputGraphFolder, calibrMod,
                     '/timeSeries_pred',j,'_', plotTitle,'.tiff'))
        ggsave(paste0(outputGraphFolder, calibrMod,
                      '/timeSeries_pred',j,'_', plotTitle,'.tiff'), dpi=300,
               width=7, height=7.5)
    }
    else{
        r <- 6
        for(j in 1:(length(feature)%%r-1)){
            
            if(j!=(length(feature)%%r-1)){
                print(feature[(1+r*(j-1)):(r*j)])
                predP <- gather(pred %>% inner_join(q, by='datetime') %>% 
                                    select(all_of(feature[(1+r*(j-1)):(r*j)])) %>% 
                                    mutate(day=as.Date(ymd$d),
                                           datatype=all$datatype), 
                                'predictors','values', all_of(feature[(1+r*(j-1)):(r*j)]))
            }else{
                print(feature[(1+r*(j-1)):length(feature)])
                predP <- gather(pred %>% inner_join(q, by='datetime') %>% 
                                    select(all_of(feature[(1+r*(j-1)):length(feature)])) %>% 
                                    mutate(day=as.Date(ymd$d),
                                           datatype=all$datatype), 
                                'predictors','values', all_of(feature[(1+r*(j-1)):length(feature)]))
            }
            p1 <- ggplot(data=predP, 
                         aes(x=day, y=values))+
                geom_line(col='grey')+
                geom_line(data=predP %>%
                              group_by(datatype,day,predictors) %>%
                              mutate(meanV=mean(values)),
                          aes(x=day, y=meanV), col='blue')+
                
                geom_ribbon(data=predP %>%
                                group_by(datatype,day,predictors) %>%
                                mutate(meanV=mean(values),
                                       sdU=meanV+sd(values),
                                       sdL=meanV-sd(values)),
                            aes(ymin=sdL, ymax=sdU), 
                            fill='light blue', alpha=0.1)+
                facet_grid(predictors~datatype, scales = 'free_y')+
                labs(x='month', y='normalized predictor values', title = plotTitle)+
                scale_x_date(date_labels = '%b', date_breaks = '1 month')
            print(p1)
            print(paste0('Output tiff: ', outputGraphFolder, calibrMod,
                         '/timeSeries_pred',j,'_', plotTitle,'.tiff'))
            ggsave(paste0(outputGraphFolder, calibrMod,
                          '/timeSeries_pred',j,'_', plotTitle,'.tiff'), dpi=300,
                   width=7, height=7.5)
        }
    }
    
}


#------------time series of residuals/discharge------------
plotList <- vector('list',length(station)*3)
for(id in 1:length(station)){
    station_i <- stationOrder[id]
    source(paste0('function_1_readData_excludeChannelStorage', R_B_end))
    plotTitle <- stationInfo$plotName[station_i]
    print(plotTitle)
    allP <- all %>% mutate(day=as.Date(ymd$d))
    
    plotList[[id]] <- ggplot(data=allP, 
                             aes(x=day, y=res, group=factor(yr)))+
        geom_line(col='grey')+
        geom_line(data=allP %>%
                      group_by(datatype,day) %>%
                      mutate(meanV=mean(res)),
                  aes(x=day, y=meanV), col='blue')+
        geom_ribbon(data=allP %>%
                        group_by(datatype,day) %>%
                        mutate(meanV=mean(res),
                               sdU=meanV+sd(res),
                               sdL=meanV-sd(res)),
                    aes(ymin=sdL, ymax=sdU), 
                    fill='light blue', alpha=0.1)+
        # stat_summary(data=allP %>%
        #                  group_by(datatype,day) %>%
        #                  summarise(meanV=mean(res), 
        #                         sdV=sd(res)),
        #              geom="ribbon", fun.ymin="min", fun.ymax="max", 
        #              aes(x=day), alpha=0.3) +
        
        facet_grid(datatype~.)+
        labs(x='', y='residuals (m/d)', title = plotTitle)+
        scale_x_date(date_labels = '%b', date_breaks = '1 month')
    if(id!=1) plotList[[id]] <- plotList[[id]]+labs(y='')
    
}
for(id in 1:length(station)){
    station_i <- stationOrder[id]
    source(paste0('function_1_readData_excludeChannelStorage', R_B_end))
    plotTitle <- stationInfo$plotName[station_i]
    print(plotTitle)
    allP <- all %>% mutate(day=as.Date(ymd$d))
    
    plotList[[id+length(station)]] <- ggplot(data=allP, 
                               aes(x=day, y=obs, group=factor(yr)))+
        geom_line(col='grey')+
        geom_line(data=allP %>%
                      group_by(datatype,day) %>%
                      mutate(meanV=mean(obs)),
                  aes(x=day, y=meanV), col='blue')+
        # stat_summary(data=allP %>%
        #                  group_by(datatype,day) %>%
        #                  summarise(meanV=mean(res), 
        #                         sdV=sd(res)),
        #              geom="ribbon", fun.ymin="min", fun.ymax="max", 
        #              aes(x=day), alpha=0.3) +
        geom_ribbon(data=allP %>%
                        group_by(datatype,day) %>%
                        mutate(meanV=mean(obs),
                               sdU=meanV+sd(obs),
                               sdL=meanV-sd(obs)),
                    aes(ymin=sdL, ymax=sdU), 
                    fill='light blue', alpha=0.1)+
        facet_grid(datatype~.)+
        labs(x='', y='observations (m/d)')+
        scale_x_date(date_labels = '%b', date_breaks = '1 month')
    if(id!=1) plotList[[id+length(station)]] <- plotList[[id+length(station)]]+labs(y='')
    
}
for(id in 1:length(station)){
    station_i <- stationOrder[id]
    source(paste0('function_1_readData_excludeChannelStorage', R_B_end))
    plotTitle <- stationInfo$plotName[station_i]
    print(plotTitle)
    allP <- all %>% mutate(day=as.Date(ymd$d))
    
    
    plotList[[id+length(station)*2]] <- ggplot(data=allP, 
                                aes(x=day, y=pcr, group=factor(yr)))+
        geom_line(col='grey')+
        geom_line(data=allP %>%
                      group_by(datatype,day) %>%
                      mutate(meanV=mean(pcr)),
                  aes(x=day, y=meanV), col='blue')+
        # stat_summary(data=allP %>%
        #                  group_by(datatype,day) %>%
        #                  summarise(meanV=mean(res), 
        #                         sdV=sd(res)),
        #              geom="ribbon", fun.ymin="min", fun.ymax="max", 
        #              aes(x=day), alpha=0.3) +
        geom_ribbon(data=allP %>%
                        group_by(datatype,day) %>%
                        mutate(meanV=mean(pcr),
                               sdU=meanV+sd(pcr),
                               sdL=meanV-sd(pcr)),
                    aes(ymin=sdL, ymax=sdU), 
                    fill='light blue', alpha=0.1)+
        facet_grid(datatype~.)+
        labs(x='month', y='pcr predictions (m/d)')+
        scale_x_date(date_labels = '%b', date_breaks = '1 month')
    if(id!=1) plotList[[id+length(station)*2]] <- plotList[[id+length(station)*2]]+labs(y='')
    
}
plotList$nrow <- 3
print(paste0('Output tiff: ', outputGraphFolder, calibrMod,'/timeSeries.tiff'))
tiff(paste0(outputGraphFolder, calibrMod,'/timeSeries.tiff'), 
     height=7, width=13, units='in', res=300)
do.call(grid.arrange, plotList)
dev.off()

allP <- all %>% mutate(day=as.Date(ymd$d))
# ggplot(data=all, 
#        aes(x=day, y=res, group=factor(yr)))+
#     geom_line(col='grey')+
#     geom_line(data=all %>%
#                   group_by(datatype,day) %>%
#                   mutate(meanRes=mean(res)),
#                   aes(x=day, y=meanRes), col='blue')+
#     facet_grid(datatype~.)
#     # scale_x_date(date_labels = '%b', date_breaks = '1 month')

# ggplot(data=allP, 
#        aes(x=day, y=res, group=factor(yr)))+
#     geom_line(col='grey')+
#     geom_line(data=allP %>%
#                   group_by(datatype,day) %>%
#                   mutate(meanRes=mean(res)),
#               aes(x=day, y=meanRes), col='blue')+
#     facet_grid(datatype~.)+
#     labs(x='month', y='residuals (cms)', title = plotTitle)+
#     scale_x_date(date_labels = '%b', date_breaks = '1 month')
# 
# 
# ggplot(data=allP, 
#        aes(x=day, y=pcr, group=factor(yr)))+
#     geom_line(col='grey')+
#     geom_line(data=allP %>%
#                   group_by(datatype,day) %>%
#                   mutate(meanRes=mean(pcr)),
#               aes(x=day, y=meanRes), col='blue')+
#     facet_grid(datatype~.)+
#     labs(x='month', y='pcr predictions (cms)', title = plotTitle)+
#     scale_x_date(date_labels = '%b', date_breaks = '1 month')
# 
# ggplot(data=allP, 
#        aes(x=day, y=obs, group=factor(yr)))+
#     geom_line(col='grey')+
#     geom_line(data=allP %>%
#                   group_by(datatype,day) %>%
#                   mutate(meanRes=mean(obs)),
#               aes(x=day, y=meanRes), col='blue')+
#     facet_grid(datatype~.)+
#     labs(x='month', y='observations (cms)', title = plotTitle)+
#     scale_x_date(date_labels = '%b', date_breaks = '1 month')
# 
# 
# 
# 
# p1 <- ggplot(data=all %>% filter(datatype=='train'), 
#              aes(x=datetime, y=res))+
#     geom_line()+
#     facet_wrap(yr~., scale='free')+
#     scale_x_date(date_labels = '%b', date_breaks = '1 month')+
#     geom_vline(xintercept=as.numeric((all %>% 
#                                           filter(grepl('-05-01', datetime)))$datetime), lty=2)+
#     geom_vline(xintercept=as.numeric((all %>% 
#                                           filter(grepl('-10-01', datetime)))$datetime), lty=2)+
#     labs(title='train period (1981-1990)', y='residuals (cms)')+
#     theme( axis.text.x = element_text(size=7))+
#     scale_colour_manual(values=c('black','green','blue'))
# p1
# p1%+%(all %>% filter(datatype=='test'))+labs(title='test period (1991-2000)')
# 
# ggplot(data=all %>% 
#            gather('discharge','values',all_of(qVar)) %>% 
#            filter(datatype=='train'), aes(x=datetime, y=values, col=discharge))+
#     geom_line()+
#     facet_wrap(yr~., scale='free')+
#     scale_x_date(date_labels = '%b', date_breaks = '1 month')+
#     geom_vline(xintercept=as.numeric((all %>% 
#                                           filter(grepl('-05-01', datetime)))$datetime), lty=2)+
#     geom_vline(xintercept=as.numeric((all %>% 
#                                           filter(grepl('-10-01', datetime)))$datetime), lty=2)+
#     labs(title='train period (1981-1990)', y='residuals (cms)')+
#     theme( axis.text.x = element_text(size=7))+
#     scale_colour_manual(values=c('black','red','orange'))
# 
# ggplot(data=all %>% 
#            gather('discharge','values',all_of(qVar)) %>% 
#            filter(datatype=='test'), aes(x=datetime, y=values, col=discharge))+
#     geom_line()+
#     facet_wrap(yr~., scale='free')+
#     scale_x_date(date_labels = '%b', date_breaks = '1 month')+
#     geom_vline(xintercept=as.numeric((all %>% 
#                                           filter(grepl('-05-01', datetime)))$datetime), lty=2)+
#     geom_vline(xintercept=as.numeric((all %>% 
#                                           filter(grepl('-10-01', datetime)))$datetime), lty=2)+
#     labs(title='test period (1991-2000)', y='residuals (cms)')+
#     theme( axis.text.x = element_text(size=7))+
#     scale_colour_manual(values=c('black','red','orange'))


#--------------create names' class for predictors------------

flux <- c('baseflow',    #actualET
          'directRunoff',
          'domesticWaterWithdrawal',
          'gwRecharge',
          'industryWaterWithdrawal',
          'interflowTotal',
          'irrigationWaterWithdrawal',
          'livestockWaterWithdrawal',
          'nonIrrWaterConsumption',
          'totalEvaporation',
          'totLandSurfaceActuaET')

depth <- c('snowCoverSWE',
           'snowFreeWater',
           'storGroundwater',
           'storLowTotal',
           'storUppTotal',
           'surfaceWaterStorage')
volume <- c('channelStorage')
#4
etnames <- names(all)[sapply(names(all), 
                             function(x) grepl('ET',x))|sapply(names(all), 
                                                               function(x) grepl('vap',x))|sapply(names(all), 
                                                                                                  function(x) x=='et')]
etnames
#2
snownames <- names(all)[sapply(names(all), function(x) grepl('snow',x))]
#7
waternames <- names(all)[sapply(names(all), function(x) grepl('gw',x))|sapply(names(all), function(x) grepl('groundwater',x))|sapply(names(all), function(x) grepl('stor',x))|sapply(names(all), function(x) grepl('flow',x))|sapply(names(all), function(x) grepl('Runoff',x))]
#2
stornames <- names(all)[sapply(names(all), function(x) grepl('Storage',x))]
#5
wdlnames <- names(all)[sapply(names(all), function(x) grepl('Withdrawal',x))|sapply(names(all), function(x) grepl('Consumption',x))]


#------------archive-----------
#correlation with residuals
# corData1 <-  all %>% 
#     group_by(datatype) %>% 
#     mutate(res=(res-mean(res))/sd(res)) %>% as.data.frame() %>% 
#     gather(., key='pred','value', c(all_of(feature),-'day')) %>% 
#     mutate(predtype=case_when(pred%in%snownames ~ 'snow', 
#                               pred%in%stornames ~ 'storage',
#                               pred%in%waternames ~ 'water',
#                               pred%in%wdlnames ~ 'withdrawal',
#                               pred%in%etnames ~ 'ET',
#                               pred%in%c('t','p') ~ 'meteor'))
# 
# corData1 <- corData1 %>% 
#     group_by(pred) %>% 
#     mutate(cor=cor(value, res), 
#            corSign=ifelse(cor>0, '+', '-'))
# 
# # corData <- corData1 %>% 
# #     group_by(pred, datatype, predtype) %>% 
# #     summarise(cor=cor(value, res)) %>% 
# #     mutate(colCor=ifelse(cor < 0, "-","+"))
# 
# 
# ggplot(corData1, aes(x=value, y=res, group=pred))+
#     geom_point()+
#     facet_grid(.~pred, scale='free_x')+
#     geom_text(aes(label=round(cor, 2), col=corSign,
#                   x=max(value)-8, y=max(res)-1) 
#     )
# # annotate(geom = 'text', label = 'cor', #aes(col=corSign),
# #          size=6,
# #          x = -Inf, y = Inf, hjust = 0, vjust = 1)
# 
# 
# ggsave(paste0('../graph/', calibrMod,'/cor_state_res.tiff'), dpi=300, 
#        width=25, height=2.5)
# 
# ggplot(corData1, aes(x=value, y=res, group=pred))+
#     geom_point()+
#     geom_text(aes(label=round(cor, 2), col=corSign,
#                   x=max(value)-8, y=max(res)-1) 
#     )+
#     facet_wrap(.~pred, scale='free_x', nrow = 4)
# ggsave(paste0('../graph/', calibrMod,'/cor_state_res_1.tiff'), dpi=300, 
#        width=10, height=6.5)
# 
# # p1 <- ggplot(corData %>% 
# #            filter(datatype=='train'))+
# #     geom_bar(stat='identity', 
# #              aes(x=reorder(pred, cor), y=cor, fill=colCor))+
# #     coord_flip()+
# #     scale_fill_manual(values=c("steelblue", "firebrick1"))+
# #     xlab('')+
# #     ylab('correlation value')+
# #     labs(fill='sign', title='train')+
# #     theme(legend.position = 'none')
# # 
# # p2 <- p1 %+% (corData %>% 
# #                   filter(datatype=='test'))+
# #     theme(legend.position = 'right')+    #axis.text.y=element_blank()
# #     labs(title='test')
# # grid.arrange(p1,p2, nrow=1)
# 
# 
# 
# p3 <- ggplot(corData)+
#     geom_bar(stat='identity', 
#              aes(x=reorder(pred, cor), y=cor, fill=colCor))+
#     coord_flip()+
#     scale_fill_manual(values=c("steelblue", "firebrick1"))+
#     xlab('')+
#     ylab('correlation value')+
#     labs(fill='sign')+
#     facet_grid(predtype~datatype, scale='free_y')+
#     theme_grey(base_size = 13.5)
# p3
# # combine plot (but it seems that p3 is enough?)
# # grid.arrange(p1,
# #              p2+theme(legend.position = 'none'),
# #              p3, nrow=1, layout_matrix=matrix(c(1,2,3,3), nrow=1))
