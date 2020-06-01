library(elevatr)
library(sp)
stationInfo <- read.csv('../data/rawData/stationLatLon.csv')
station <- list.files('../data/preprocess/calibrated/','pcr_') %>% 
    sapply(., function(x) substr(x, 5, nchar(x)-4)) %>% as.character()
stationInfo <- stationInfo[(stationInfo$station %>% tolower)%in%station,]

prj_dd <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "
station_sp <- SpatialPointsDataFrame(cbind(stationInfo$y, stationInfo$x),
                                     stationInfo, proj4string = CRS(prj_dd))
# station_sp <- st_as_sf(stationInfo, coords = c("y","x"), crs=prj_dd)
# station_sp_t <- spTransform(station_sp, CRSobj = prj_dd_t)
# dem <- get_elev_raster(station_sp, z=9)
# dem11 <- get_elev_raster(station_sp, z=11)
# plot(dem11)
# plot(station_sp, add=T)
#-----1-------
library(raster)
library(tmap)

countID <- c('CHE','DEU','NLD','FRA','BEL')
bndL <- lapply(1:length(countID), function(i){
    getData(country=countID[i], name='GADM', level=0)})
countBnd <- do.call(bind, bndL) 

#SRTM 90 Elevation
#1
# srtm <- getData('SRTM', lon=7.5, lat=47.5)
# srtm2 <- getData('SRTM', lon=7.5, lat=52.5)
# srtmmosaic <- mosaic(srtm, srtm2, fun=mean)
# writeRaster(srtmmosaic, '../data/rawData/dem_srtm/srtm_mosaic_smaller.grd')
#2 (this is better)
srtmFiles <- list.files('../data/rawData/dem_srtm/', pattern='srtm_3')
srtmFiles <- srtmFiles[which(grepl('.tif',srtmFiles))] %>% 
    paste0('../data/rawData/dem_srtm/',.)
srtmList <- lapply(srtmFiles, raster)
srtmList$fun <- mean
srtmmosaic <- do.call(mosaic, srtmList)

# srtm_d <- raster('../data/rawData/dem_srtm/srtm_38_03.tif')
# srtm_d2 <- raster('../data/rawData/dem_srtm/srtm_38_02.tif')
# srtm_d3 <- raster('../data/rawData/dem_srtm/srtm_39_02.tif')
# srtm_d4 <- raster('../data/rawData/dem_srtm/srtm_39_03.tif')
# srtmmosaic2 <- mosaic(srtm_d, srtm_d2, srtm_d3, srtm_d4, fun=mean)
# writeRaster(srtmmosaic2, '../data/rawData/dem_srtm/srtm_mosaic.tif', overwrite=T)
# writeRaster(srtmmosaic2, '../data/rawData/dem_srtm/srtm_mosaic.grd', overwrite=T)
# srtmmosaic2 <- raster('../data/rawData/dem_srtm/srtm_mosaic.grd')
countBnd_c <- crop(countBnd, extent(c(5,15,46,65)))   #52
srtmmosaic2_c <- crop(srtmmosaic2, extent(c(5,15,46,55)))

# plot(srtmmosaic2)
# plot(srtmmosaic2_c)
# plot(countBnd_c, add=T)
# plot(station_sp, add=T)

tmap_mode(mode = 'plot')
demMap <- 
    tm_shape(countBnd_c) +
    tm_borders(col='grey')+
    tm_shape(srtmmosaic2_c)+
    tm_raster(breaks=c(-100, 0, 10, 20, 30, 40, 50, 70, 90,
                       150, 200, 250, 400, 800, 1000, 2000, 3000, 4000, 10000),  
            palette = terrain.colors(18), title="Elevation", style = 'cont') +
    tm_shape(countBnd_c) +
    tm_borders(col='grey')+
    tm_shape(station_sp)+
    tm_dots(col='red')+  #size='area'
    tm_text('plotName',
            xmod=1.8, ymod=0.2, size=0.5)+   #xmod=1.4, ymod=0.55
    # tm_dots('plotName', col = 'plotName', style='cat')+
    tm_scale_bar(text.size=1, text.color='black', 
                 position = c('center','bottom'))+
    tm_compass(text.size = 0.5, position = c('right','bottom'))+
    tm_layout(legend.title.size = 0.77, legend.text.size = 0.5,
              legend.outside.size = 0.4, legend.outside = T,
              attr.outside = T)

tmap_save(demMap, filename = paste0('../graph/', 
                                    'demMap.tiff'), 
          dpi=300, height=3, width=3, units='in')
    
# plot(germany0, add=T)
# plot(netherlands0, add=T)
# plot(austria0, add=T)
# plot(france0, add=T)
# plot(BEL, add=T)
#------------2------------
require(XML)
doc <- htmlParse("http://www.viewfinderpanoramas.org/dem3.html#alps")
urls <- paste0("http://www.viewfinderpanoramas.org", 
               xpathSApply(doc,'//*/a[contains(@href,"/dem1/N4")]/@href'))
names <- gsub(".*dem1/(\\w+\\.zip)", "\\1", urls)
for (i in 1:length(urls)) download.file(urls[i], names[i]) 
# unzip all files in dir and delete them afterwards
sapply(list.files(pattern = "*.zip"), unzip)
unlink(list.files(pattern = "*.zip"))

# df_elev_epqs <- get_elev_point(stationInfo, prj = prj_dd, src = "epqs")
# df_elev_epqs <- get_elev_point(station_sp_t, prj = prj_dd_t, src = "epqs")
# data.frame(df_elev_epqs)
