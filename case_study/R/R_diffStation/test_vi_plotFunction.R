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
    
    df <- corDatac %>% gather('key','value', c('cor','importance'))
    dummy <- data.frame(day=NA, key=rep(c("cor", "importance"), each=2), 
                        value=c(0.1*max(df$value[df$key=="cor"]), 
                                1.1*max(df$value[df$key=="cor"]),
                                0.5*max(df$value[df$key=="importance"]), 
                                1.1*max(df$value[df$key=="importance"])))
    
    p1 <-  ggplot(df)+
        geom_col(aes(reorder(pred, c(value[key=='cor'], 
                                     value[key=='importance']*1000)), 
                     value),
                 position = 'dodge', fill='khaki') +
        geom_blank(data=dummy, aes(day, value))+
        # Make the predictor names inside
        geom_text(aes(label = c(pred[key=='cor'], rep('', length(pred[key=='cor']))),
                      x = pred, y = -Inf, hjust = 0),
                  size=6)+    #hjust = 0, vjust = 1
        coord_flip() +
        theme_light()+
        facet_grid(.~key, scale='free')+
        # geom_text(aes(label = key), x = Inf, y = Inf, hjust = 1.5, vjust = -1.5)+
        theme(
            # axis.text.y = element_text(size = 15.5),
            axis.text.y = element_blank(),
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
    p1
    
    return(p1)
}