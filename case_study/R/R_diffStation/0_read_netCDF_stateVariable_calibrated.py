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
filePath = '../data/rawData/upstreamStateVariables/calibrated/'
outputPath = '../data/preprocess/calibrated/'
loc = pandas.read_csv('../data/rawData/stationLatLon.csv')

def near(array,value):
    idx=(np.abs(array-value)).argmin()
    return idx

dirName = listdir(filePath)



for j in range(len(loc)):
    
    xin, yin = loc['x'][j], loc['y'][j]
    station_name = loc['station'][j].lower()
    
    for i in range(len(dirName)):
        dir_name = dirName[i]
        
        varName = listdir(filePath+dir_name)
        
        if (station_name==dir_name):
            
            for k in range(len(varName)):
                if dir_name=='lobith':
                    var_name=varName[k][:-29]
                else:
                    var_name=varName[k][:-38]
            
                nc = netCDF4.Dataset(filePath+dir_name+'/'+varName[k])
                lat = nc.variables['lat'][:]
                lon = nc.variables['lon'][:]
                var = nc.variables['Band1']
                
                #find nearest point to desired location
                ix = near(lat, xin)
                iy = near(lon, yin)
                if k==0:
                    timeLength = nc.dimensions['record'].size
                    upstream = pandas.DataFrame([['NaN']*len(varName)]*timeLength)
                    
                upstream[k] = var[:,ix,iy]
                upstream.rename(columns={k:var_name}, inplace=True)
            print('output csv:')
            print(station_name)
            upstream.to_csv(outputPath+'/upstream_avg_state_var_1981_2000_'+station_name+'.csv')
        else:
            print('\n no csv:')
            print(station_name)
