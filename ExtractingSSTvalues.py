##################################################################################################################
# Extracting SST values from Aqua MODIS netCDF file
# Author: Denisse Fierro Arcos
# Version: 1
# Date: 2019-05-22
# Purpose: Extract SST values from netCDF files containing SST values from Aqua MODIS. This script will identify
# the months of interest, create a multiband raster, mask it using a shapefile defining the area of interest,
# extract SST values into a point feature and finally save the attribute table as a csv file.
##################################################################################################################

##################################################################################################################
#Upload relevant libraries
import arcpy
import os
import netCDF4 as nc
import numpy as np
import datetime as dt
import re
import pandas as pd

#Activate Spatial Analyst extension
arcpy.CheckOutExtension("Spatial")

#Allowing outputs to be overwritten.
arcpy.env.overwriteOutput = True

#Set working directory
wd = r"C:\Users\denisse.fierro\Documents\RESPALDO NAS DENISSE\SamplingCampaigns\MuestroEcuadorContinental\CapasSIG"
arcpy.env.workspace = wd #for arcpy
os.chdir(wd) #for Python
del wd
##################################################################################################################

#Upload netCDF file with netCDF4 package
SST = nc.Dataset("erdMBsstdmday_LonPM180_b102_bcb5_c347.nc", "r")
#Read variable 'Time' to identify layers
t = SST.variables["time"][:] #in seconds from 01/01/1970
#Change Time variable to date format
T = [dt.datetime.fromtimestamp(SST.variables["time"][i]).strftime("%d/%m/%Y") for i in range(0, len(t))]
del t

#Create variable with months of interest
MOI = ["05", "07"] #May and July
indT = [] #Create index variable
dates = [] #Create date variable
ind = []
#Find all occasions when MOI appear in Time variable
for m in MOI:
    regex = re.compile('.*'+ m +' *')
    x = filter(regex.search, T)
    #Find index for all items identified
    for i in x:
        indT.append("sst_" + str(T.index(i)))
        dates.append("time '"+ i +" 12:00:00'")
        ind.append(T.index(i))
del x, MOI

TimeLabs = [dt.datetime.fromtimestamp(SST.variables["time"][i]).strftime("%B %Y") for i in ind]
del ind, SST

#[Bands.append("sst/Band_{}".format(i)) for i in indAdj]

##################################################################################################################
#Upload only layers of interest using Arcpy
for i in range(0,len(dates)):
	arcpy.MakeNetCDFRasterLayer_md("erdMBsstdmday_LonPM180_b102_bcb5_c347.nc", "sst", "longitude", "latitude",
                                       indT[i], "", dates[i])

#Create a composite raster with all bands previously identified
SST = arcpy.CompositeBands_management(indT, "SST_compMOI.TIF")

#Upload shapefile of areas of interest - Machalilla & Galera San Francisco
aoi = r"RM_Machalilla_GSF_WGS84.shp"

#Mask out raster with AOI layer containing the Marine Reserve limits for Machalilla and Galera San Francisco
SST_RMs = arcpy.sa.ExtractByMask(SST, aoi)
SST_RMs.save("SST_RMs.TIF")

#Create point features from masked out raster
SST_Pts = arcpy.RasterToPoint_conversion(SST_RMs, "SST_Pts.shp", "Value")

#Extract values from multiple layers to point features
SST_RMVal = arcpy.gp.ExtractMultiValuesToPoints_sa(SST_Pts, "SST_RMs.tif SST_RMs", "NONE")

#Save attribute table of point features in excel format
arcpy.TableToExcel_conversion(SST_RMVal, "SST_RMValues.xls", "NAME", "CODE")

#Deactivate Spatial Analyst extension
arcpy.CheckInExtension("Spatial")

##################################################################################################################
#Read excel table
SST_RMPtVal = pd.ExcelFile("SST_RMValues.xls")
#SST_RMPtVal.sheet_names #Get the name of all sheets in the excel file
#Load relevant excel sheet
SST_sheet = SST_RMPtVal.parse('SST_RMValues')
#Create dictionary to change column names to months
NewColNames = dict(zip(SST_sheet.columns[3:13], TimeLabs))
#Change column names using dictionary
SST_sheet.rename(columns = NewColNames, inplace = True)
#Write file with corrected column names into a csv file for further analysis
SST_sheet.to_csv("SST_ValuesRMGSF_RMM.csv")
