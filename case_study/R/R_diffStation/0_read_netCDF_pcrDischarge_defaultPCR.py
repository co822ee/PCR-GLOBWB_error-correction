#!/usr/bin/env python2
# -*- coding: utf-8 -*-
"""
Created on Thu Mar 19 14:13:29 2020

@author: jessicaruijsch
"""
#========================================================================
#
# * This script extracts values from all netCDF files in a folder and 
#   outputs the values as a csv file. 
# * (This folder should contain only the netCDF files you want to extract)
# * The values at different locations are saved in different csv files 
#   in the output file path, with location names indicated at the end of the file names.
#========================================================================

import pandas
import netCDF4
import numpy as np
from os import listdir
import os,fnmatch


# xin,yin = 47.5594,7.6167
filePath = '../data/rawData/PCR-discharge/'
outputPath = '../data/preprocess/uncalibrated/'
loc = pandas.read_csv('../data/rawData/stationLatLon.csv')

def near(array,value):
    idx=(np.abs(array-value)).argmin()
    return idx

if not os.path.exists(outputPath):
    os.mkdir(outputPath)

fileName = fnmatch.filter(os.listdir(filePath), '*.nc')
obs = pandas.read_csv('../data/rawData/observed_discharge_basel.csv')

for i in range(len(fileName)):   #calibrated/uncalibrated
    
    file_name = fileName[i]
    var_name = fileName[i][26:-3]
    

    
    nc = netCDF4.Dataset(filePath+file_name)
    print(i+1, var_name+':')
    
    for j in range(len(loc)):
        xin, yin = loc['x'][j], loc['y'][j]
        station_name = loc['station'][j].lower()
    
        
        lat = nc.variables['lat'][:]
        lon = nc.variables['lon'][:]
        var = nc.variables['discharge']
        
        #find nearest point to desired location
        ix = near(lat, xin)
        iy = near(lon, yin)
        timeLength = nc.dimensions['time'].size
        
        # The PCR-GLOBWB model was run for 1900-2010, and
        # the year 1900 was a spin-up year, so the first
        # 365 days are ignored.
        upstream = pandas.DataFrame(var[365:,ix,iy],columns=['discharge'])
        upstream['datetime'] = obs['datetime']

        
        print(j+1, ':', station_name) 
        upstream.to_csv(outputPath+'/pcr_'+station_name+'.csv')
