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
import os 


# xin,yin = 47.5594,7.6167
filePath = '../data/rawData/upstreamStateVariables/Upstream states Default parameterization/'
outputPath = '../data/preprocess/uncalibrated/'
loc = pandas.read_csv('../data/rawData/stationLatLon.csv')

def near(array,value):
    idx=(np.abs(array-value)).argmin()
    return idx

fileName = listdir(filePath)
if not os.path.exists(outputPath):
    os.mkdir(outputPath)
    
for j in range(len(loc)):
    xin, yin = loc['x'][j], loc['y'][j]
    station_name = loc['station'][j].lower()
    for i in range(len(fileName)):
        file_name = fileName[i]
        var_name = file_name[:-38]
        nc = netCDF4.Dataset(filePath+file_name)
        lat = nc.variables['lat'][:]
        lon = nc.variables['lon'][:]
        var = nc.variables['Band1']
        
        #find nearest point to desired location
        ix = near(lat, xin)
        iy = near(lon, yin)
        if i==0:
            timeLength = nc.dimensions['record'].size
            upstream = pandas.DataFrame([['NaN']*len(fileName)]*timeLength)
            
        upstream[i] = var[:,ix,iy]
        upstream.rename(columns={i:var_name}, inplace=True)
    print(loc.iloc[j])
    upstream.to_csv(outputPath+'upstream_avg_state_var_1981_2000_'+station_name+'.csv')
