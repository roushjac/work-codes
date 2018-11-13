# This is a script for homework 1 of GLG 893
# Attempting to use pandas, geopandas, and seaborn with a jupyter notebook for the first time - wishing myself luck

import pandas as pd
import geopandas as gpd
import seaborn as sns

# Loading shapefiles for my selected watershed and counties
wshedShape = gpd.read_file('S:/Users/roushjac/Misc/Class/GLG 893/Homework 1/Watershed/SanFran_watershed.shp')
countyShape = gpd.read_file('S:/Users/roushjac/Misc/Class/GLG 893/Homework 1/TIGER Counties/tl_2017_us_county.shp')

# Selecting only counties in the state my watershed is in (California)
countyShape = countyShape.loc[countyShape['STATEFP'] == '06']

# Intersecting watershed and counties
intersecWater = gpd.overlay(countyShape,wshedShape, how = 'intersection')

# Saving shapefile
intersecWater.to_file('S:/Users/roushjac/Misc/Class/GLG 893/Homework 1/intersectWater.shp', driver='ESRI Shapefile')

# Loading discharge files - each one is from a different spreadsheet, could combine into one but don't think I need to
disOyster = pd.read_excel('S:/Users/roushjac/Misc/Class/GLG 893/Homework 1/Data/cuahsi_discharge/master_discharge.xlsx',sheetname='Oyster')
disNarrows = pd.read_excel('S:/Users/roushjac/Misc/Class/GLG 893/Homework 1/Data/cuahsi_discharge/master_discharge.xlsx',sheetname='Narrows')
disFrench = pd.read_excel('S:/Users/roushjac/Misc/Class/GLG 893/Homework 1/Data/cuahsi_discharge/master_discharge.xlsx',sheetname='French')
disAmer = pd.read_excel('S:/Users/roushjac/Misc/Class/GLG 893/Homework 1/Data/cuahsi_discharge/master_discharge.xlsx',sheetname='MF American')
disDeer = pd.read_excel('S:/Users/roushjac/Misc/Class/GLG 893/Homework 1/Data/cuahsi_discharge/master_discharge.xlsx',sheetname='Deer')

# Loading precip and temperature data - contained in the same file from NOAA
TempPrecip = pd.read_csv('S:/Users/roushjac/Misc/Class/GLG 893/Homework 1/Data/NOAA_precip_temp.csv')

# Converting TempPrecip "date" column from string to datetime format
TempPrecip['DATE'] = pd.to_datetime(TempPrecip['DATE'])

# Reading in USGS water use data for all counties. It is natively a tab-delimited table
waterUse = pd.read_table('S:/Users/roushjac/Misc/Class/GLG 893/Homework 1/Data/usgs_water_use.txt')

# Reading in corn data from USDA NASS
nassCorn = pd.read_csv('S:/Users/roushjac/Misc/Class/GLG 893/Homework 1/Data/NASS_Corn.csv')

# Attempting to plot all discharge time series on top of each other

ax1=disAmer.loc[2:,].plot(x='VarUnits',y='DataType')
ax2=disDeer.loc[2:,].plot(x='VarUnits',y='DataType',ax=ax1)
ax3=disFrench.loc[2:,].plot(x='VarUnits',y='DataType',ax=ax2)
ax4=disNarrows.loc[2:,].plot(x='VarUnits',y='DataType',ax=ax3)
finalplot=disOyster.loc[2:,].plot(x='VarUnits',y='DataType',ax=ax4)
finalplot.legend(['Amer','Deer','French','Narrows','Oyster'])
finalplot.set_xlabel('Year')
finalplot.set_ylabel('Discharge (ft^3/sec)')

## Plotting all precipitation data


TempPrecip.set_index('DATE',inplace=True)
precipGroup=TempPrecip.groupby('NAME')['PRCP']
precipPlot=precipGroup.plot(legend=True)
#TempPrecip.groupby(by='NAME').plot(x='DATE',y='PRCP')

## Plotting all temperature data
# Using max temp for each day

tempGroup=TempPrecip.groupby('NAME')['TMAX']
tempPlot=tempGroup.plot(legend=True)

# Mapping watershed within counties
# Making watershed boundaries bold or stand out somehow

intersectMap=intersecWater.plot(edgecolor='Red')
intersectMap.set_xlabel('Longitude')
intersectMap.set_ylabel('Latitude')
intersectMap.set_title('San Francisco Bay Watershed, HUC 6')

# Changing waterUse dataframe to have a 0 at the beginning of counties with 4 digit FIPS, to match with the intersected layer
for a in range(0,len(waterUse.index)):
    waterUse.loc[a,'FIPS'] = '0' + str(waterUse.loc[a,'FIPS'])

# Changing datatype of intersection layer to be string to be consistent
for a in range(0,len(intersecWater.index)):
    intersecWater.loc[a,'GEOID'] = str(intersecWater.loc[a,'GEOID'])

intersecWaterUse = pd.merge(intersecWater, waterUse, left_on='GEOID', right_on='FIPS')

intersecWaterUse.plot(column='TO-WTotl',legend=True)

# Adding 0 into nassCorn dataframe as well to keep it consistent with intersection
for a in range(0,len(nassCorn.index)):
    nassCorn.loc[a,'GEOID'] = '0' + str(nassCorn.loc[a,'GEOID'])

#intersecWaterCorn = pd.merge(intersecWater, nassCorn[['GEOID','Value']], on='GEOID')
intersecWaterCorn = intersecWater.merge(nassCorn[['GEOID','Value']], on='GEOID')

intersecWaterCorn.loc[1:,].plot(column='Value',legend=True)


#%% This is not part of HW1
## Appending all discharge dataframes together after adding in column for each with only their station name

colList = ['VariableName','DataType','SiteName']

# Removing unnecessary columns
disAmer = disAmer[colList]
disDeer = disDeer[colList]
disFrench = disFrench[colList]
disNarrows = disNarrows[colList]
disOyster = disOyster[colList]

# Creating column with only station name - useful for appending/groupby functions

disAmer['SiteName'] = disAmer.loc[0,'SiteName']
disAmer = disAmer.loc[2:,]

disDeer['SiteName'] = disDeer.loc[0,'SiteName']
disDeer = disDeer.loc[2:,]

disFrench['SiteName'] = disFrench.loc[0,'SiteName']
disFrench = disFrench.loc[2:,]

disNarrows['SiteName'] = disNarrows.loc[0,'SiteName']
disNarrows = disNarrows.loc[2:,]

disOyster['SiteName'] = disOyster.loc[0,'SiteName']
disOyster = disOyster.loc[2:,]

disTotal = disAmer
disTotal = disTotal.append(disDeer).append(disFrench).append(disNarrows).append(disOyster)

# Fixing column names

disTotal.rename(columns={'VariableName': 'Time', 'DataType': 'Discharge'}, inplace=True)

# Loading finalized data into the HDF file
dataStore = pd.HDFStore('S:/Users/roushjac/Misc/Class/GLG 893/Homework 2/Data/data.h5')
dataStore['disTotal'] = disTotal
dataStore['TempPrecip'] = TempPrecip