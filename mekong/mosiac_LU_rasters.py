# This script mosiacs rasters together after they are created by Google Earth engine. 
# It is specific to Mekong Land Cover, but could be easily adapted for other purposes!

import arcpy
from arcpy import env

# Set paramaters
fieldLCName = 'Land_Cover_Class'
arcpy.CheckOutExtension('Spatial')

# Set overwrite
arcpy.env.overwriteOutput = 1

# Set environment
env.workspace = r'G:\My Drive\GEE_Outputs'
Dir = env.workspace

# Inputs
rasterBasin = ("S:\\Users\\roushjac\\NASA_Mekong\\GIS\\Basin_bound.gdb\\ras_bound")

# Produce a list of raster datasets
rasters = arcpy.ListRasters()
# Select all years based on raster names
years = []
for raster in rasters:
    year = raster[0:4] # First 4 digits of the file specify a year
    years.append(year)

# Getting unique years
years = list(set(years))

#%%
# Converting csv to arc table so we can join LC names
arcpy.TableToTable_conversion('S:\\Users\\roushjac\\NASA_Mekong\\Codes\\LU\\GEE_class_names.csv', 'S:\\Users\\roushjac\\NASA_Mekong\\Codes\\LU', 'GEE_classes_table')
#------------------------------------------------------------------------------
#%% Select 1 year - OPTIONAL - overwrites previous year list
year = ['2000']
#%%
#------------------------------------------------------------------------------


# Mosiacing all rasters together and joining values with LC classes
raslist = []
bandnumlist = []
for year in years:
    for raster in rasters:
        if raster[0:4]==str(year): # First 4 digits of raster name are a year
            raslist.append(raster)
            bandnumlist.append(arcpy.GetRasterProperties_management(raster, 'BANDCOUNT').getOutput(0))
    bandnum = max(bandnumlist)
    output = year + '_servir_landuse.tif'
    mask_output = year + '_servir_landuse_masked.tif'
    ';'.join(raslist)
    arcpy.MosaicToNewRaster_management(raslist,Dir, output, '', '', '', bandnum) # Stiching together
    arcpy.BuildRasterAttributeTable_management(in_raster=output ,overwrite="Overwrite") # Constructs attr table with classes and category counts
    # Remove masked values
    arcpy.gp.SetNull_sa(output, output, mask_output, """"Value" = 254""")
    # Populate attribute table
    # Joining correct land use class names by class number from GEE - manually generated table
    arcpy.JoinField_management(mask_output, 'Value', 'S:\\Users\\roushjac\\NASA_Mekong\\Codes\\LU\\GEE_classes_table.dbf', 'Field1')
    # Clipping LC to only Mekong basin and saving raster
    arcpy.Clip_management(in_raster=mask_output,rectangle="99.0216666673829 9.93416666645857 108.773333333567 22.7666666665335",out_raster="S:/Users/roushjac/NASA_Mekong/GIS/Basin_bound.gdb/LMRB_LC_"+year,in_template_dataset="S:\\Users\\roushjac\\NASA_Mekong\\GIS\\Basin_bound.gdb\\LMRB_rough_bound",nodata_value="#",clipping_geometry="ClippingGeometry",maintain_clipping_extent="NO_MAINTAIN_EXTENT")  
    
