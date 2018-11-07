import arcpy
import pandas
import numpy
import matplotlib.pyplot as plt


arcpy.MakeTableView_management(featureTempCountyManure, "CountyManureTableView")
arcpy.TableToTable_conversion("CountyManureTableView", scratchFilePath, "CountyManureTable")
tempArray = arcpy.da.TableToNumPyArray(scratchFilePath+'\\CountyManureTable', ['*'])
dfCountyManure = pd.DataFrame(tempArray)
arcpy.Delete_management(scratchFilePath+'\\CountyManureTable')
del tempArray
dfCountyManure[fieldAvgRate] =  dfCountyManure['kgP_year'] / dfCountyManure['Man_m2'] # This creates the field, then populates it - removes the need for AddField on the initial table
dfCountyManure['AvgRHect'] = dfCountyManure['AvgR']*10000
dfCountyManure.to_csv(tempCSVfile,index=False)
arcpy.TableToTable_conversion(tempCSVfile,outDir,'tempManureCountyTable')
arcpy.JoinField_management(featureTempCountyManure,fieldCountyJoin,'tempManureCountyTable',fieldCountyJoin, ['AvgR'])
arcpy.Delete_management('tempManureCountyTable')
arcpy.Delete_management(tempCSVfile)

dfCountyManure['AvgRHect'].plot.hist(bins=100)