This directory contains files of raw data: 

* **observed discharge (m^3/s)** obtained from the Global Runoff Data Center ([GRDC](http://www.bafg.de/GRDC)) in csv format
* **simulated discharge (m^3/s)** from the PCR-GLOBWB (calibrated & uncalibrated) in netCDF format
* **meteorological driving variables** averaged over the upstream grids of the gauging station in netCDF format
* **key hydrological state variables** from the PCR-GLOBWB (calibrated & uncalibrated) averaged over the upstream grids of the gauging station in netCDF format 


### Metadata for stationLatLon.csv:
The information in stationLatLon.csv was acquired from GRDC [station catalogues](https://www.bafg.de/SharedDocs/ExterneLinks/GRDC/grdc_stations_ftp.html;jsessionid=6443BB229927568346A2D6C4DCEB0D3C.live11293?nn=201352).


1. station: station name
2. x: latitude (decimal degree)
3. y: longitude (decimal degree)
4. river: river name
5. area: catchment size (km^2)
6. altitude: heigth of gauge (m above sea level)
7. d_start: daily data available from year
8. d_end: daily data available until year
9. d_yrs: length of time series, daily data
10. d_miss: percentage of missing values (daily data)
11. lta_discharge: mean annual streamflow (m3/s)
12. r_volume_yr: mean annual volume (km3)
13. r_height_yr: mean annual runoff depth (mm)
14. plotName: station name (river name)


