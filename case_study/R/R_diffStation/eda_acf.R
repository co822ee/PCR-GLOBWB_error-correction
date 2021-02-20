library(dplyr)
## better ACF plot
plot.acf <- function(ACFobj) {
   rr <- ACFobj$acf[-1]
   kk <- length(rr)
   nn <- ACFobj$n.used
   plot(seq(kk), rr, type = "h", lwd = 2, yaxs = "i", xaxs = "i", 
        ylim = c(floor(min(rr)), 1), xlim = c(0, kk + 1), xlab = "Lag", 
        ylab = "Correlation", las = 1)
   abline(h = -1/nn + c(-2, 2)/sqrt(nn), lty = "dashed", col = "blue")
   abline(h = 0)
}
calibrMod <- "uncalibrated"
stations <- list.files('../data/preprocess/calibrated/','pcr_') %>% 
   sapply(., function(x) substr(x, 5, nchar(x)-4)) %>% as.character()
station_i <- 1
stations[station_i]
df_datas <- lapply(stations, function(station){
   read.csv(paste0('../data/preprocess/', calibrMod, '/q_', station, '.csv')) %>% 
                     mutate(datetime=as.Date(datetime))
})
df_data <- df_datas[[station_i]]   
plot(df_data$datetime, df_data$res)
acf(df_data$res)
res_acf <- acf(df_data$res)
res_acf$acf[-1]
res_acf$n.used
dim(df_data)

par(mfrow=c(1,2))
plot.acf(res_acf)
plot.ts(df_data$res)

# The interval decreases as the number of samples for estimating acf increases
acf(df_data$res)
acf(df_data[1:365,]$res)

par(mfrow=c(1,2))
acf(df_data$res)
pacf(df_data$res)
print(stations)
par(mfrow=c(3,2))
lapply(df_datas, function(df_1){
   acf(df_1$res)
   pacf(df_1$res)
})

lapply(df_datas, function(df_1){
   acf(df_1[1:3652,]$res)
   pacf(df_1[1:3652,]$res)
})
