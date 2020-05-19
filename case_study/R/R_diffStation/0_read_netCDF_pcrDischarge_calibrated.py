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
filePath = '../data/rawData/PCR-discharge/calibrated'
outputPath = '../data/preprocess/calibrated'
loc = pandas.read_csv('../data/rawData/stationLatLon.csv')

def near(array,value):
    idx=(np.abs(array-value)).argmin()
    return idx
if not os.path.exists(outputPath):
    os.mkdir(outputPath)
    
    
fileName = listdir(filePath)
obs = pandas.read_csv('../data/rawData/observed_discharge_basel.csv')

for i in range(len(fileName)):   #calibrated/uncalibrated
    
    file_name = fileName[i]
    calibr_mode = fileName[i][26:36]
    loc_name = fileName[i][37:-3]
    
    
    nc = netCDF4.Dataset(filePath+'/'+file_name)
    print(i+1, calibr_mode+' '+loc_name+':')
    
    station = loc.loc[loc['station'].str.lower()==loc_name]
    
    xin, yin = station['x'].to_numpy(), station['y'].to_numpy()
#    station_name = station['station'].str.lower().to_numpy(dtype=str)
    station_name = (station['station'].str.lower().astype(str).values.tolist())[0]
    lat = nc.variables['lat'][:]
    lon = nc.variables['lon'][:]
    var = nc.variables['discharge']
    
    #find nearest point to desired location
    ix = near(lat, xin)
    iy = near(lon, yin)
    timeLength = nc.dimensions['time'].size
    
    upstream = pandas.DataFrame(var[365:,ix,iy],columns=['discharge'])
    upstream['datetime'] = obs['datetime']

    
    print('output csv for', station_name) 
    upstream.to_csv(outputPath+'/pcr_'+station_name+'.csv')
