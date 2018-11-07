# Script to read in netCDF files (.nc) for gridded precipitation from APHRODITE. This script is very specific to the data
# downloaded for the NASA Mekong IDS project and will need significant modification if used in the future with different data.

# Written by Jake Roush on 11/10/2017
# Last modified 11/10/2017

import numpy as np
import pandas as pd
import netCDF4
import arcpy

# Declaring variables and making an empty list
yearRange = range(1951, 2007+1) # Defining range of years covered by data
precipData = list() # Making empty list to iteratively populate
latRange = np.arange(55,-15,-0.25) # Defining limits of latitude for the dataset
lonRange = np.arange(60,150,0.25) # Defining limits of longitude for dataset

## Setting Arc parameters
outDir = "S:\\Users\\roushjac\\NASA_Mekong\\Codes\\Precip\\precipGDB.gdb"
arcpy.env.workspace = outDir
arcpy.env.scratchWorkspace = "S:\\Users\\roushjac\\NASA_Mekong\\Codes\\Precip\\pre_scratch.gdb"
arcpy.CheckOutExtension('Spatial')
arcpy.env.overwriteOutput = True
# This is a sample raster, manually created using ArcMap's netcdfToRaster tool. It is only for setting raster extent.
sampleRas = arcpy.Raster('S:\\Users\\roushjac\\NASA_Mekong\\Codes\\Precip\\precipGDB.gdb\\precip_sample')



for b in yearRange: # Iterating through my list of years to select each file from the google drive
    
    # Reading in one .nc file from the APHRODITE precip data
    # File path is valid as of last edit, may need to be changed when data moves
    oneFile = netCDF4.Dataset("D:/Mekong NASA IDS/Mekong Data/Precip_Data/APHRODITE/netcdf/extracted/APHRO_MA_025deg_V1101."+str(b)+".nc/APHRO_MA_025deg_V1101."+str(b)+".nc")
    
    # Writing the contents of the .nc to an array
    pData = oneFile.variables['precip'][:] # Shape is time, latitude, longitude
    
    # Attempting to store each day, which is a numpy array, as a separate index in a list
    # This will hopefully allow for easy selecting of any given day by simply inputting the day as an index to the list
    pList = list()
    
    for a in range(0,len(pData)): # Iterating through all days for 1 year - should be 365
        pOneDayArray = pData[a] # Selecting one day
        pList.append(pOneDayArray) # Appending that day to list
    
    precipData.append(pList) # Append yearly data to total list - 1 year for each index
        



# Writing a function that converts the dataframe of one day of precip data into 1) a numpy array,
# then 2) a raster in Arc format.

def makePrecipRaster(year, day, rasName):
    # Function arguments are a year, day, and a raster name. Saves to given Arcpy workspace
    oneDay = precipData[(year-min(yearRange))][day-1]
    oneDayDF = pd.DataFrame(oneDay)
    # Don't really need to rename columns/rows anymore, but keeping the code here in case something changes
    oneDayDF.columns = lonRange # Renaming column names to match up with longitude coordinates
    oneDayDF.index = latRange # Similarly renaming index column (rows)
    oneDayArr = oneDayDF.as_matrix() # Converting dataframe to numpy array. Arcpy's functions only work with NumPy arrays.
    oneDayArr = np.flipud(oneDayArr) # Need to flip the array to match how Arc reads in rasters
    oneDayArr[np.isnan(oneDayArr)]=9999
    ras = arcpy.NumPyArrayToRaster(oneDayArr, sampleRas.extent.lowerLeft, sampleRas.meanCellWidth,sampleRas.meanCellHeight, 9999) # Converting array to raster, using the sample raster to set extents and cell sizes
    # Replace a layer/table view name with a path to a dataset (which can be a layer file) or create the layer/table view within the script
    ras.save(rasName)
    arcpy.DefineProjection_management(in_dataset=rasName, coor_system="GEOGCS['GCS_WGS_1984',DATUM['D_WGS_1984',SPHEROID['WGS_1984',6378137.0,298.257223563]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]]")

