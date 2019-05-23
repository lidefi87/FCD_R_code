########################################################################################################################################
# Extracting SST values from Aqua MODIS netCDF file
# Author: Denisse Fierro Arcos
# Version: 2
# Date: 2019-05-23
# Purpose: This scripts extracts SST values for a particular area and month(s) contained within nc files downloaded from NOAA's Aqua
# MODIS website.
# Inputs needed:
#   - Aqua MODIS nc file
#   - Month(s) of interest
#   - Name(s) of shapefile(s) with the limits of the area(s) of interest
#   - Name(s) of the area(s) of interest
# Outputs produced:
#   - Multiband raster based on nc files for the time and area of interest
#   - Point feature shapefile with SST values
#   - Excel and csv files with extracted SST values
########################################################################################################################################

########################################################################################################################################
#Upload relevant libraries
import arcpy
import os
import netCDF4 as nc
import datetime as dt
import re
import pandas as pd

#Set working directory
wd = r"C:\Users\denisse.fierro\Documents\RESPALDO NAS DENISSE\SamplingCampaigns\MuestroEcuadorContinental\CapasSIG"
os.chdir(wd)

########################################################################################################################################

#Upload netCDF file with netCDF4 package
SST = nc.Dataset("erdMBsstdmday_LonPM180_b102_bcb5_c347.nc", "r")
#Extract 'Time' variable (sec from 01/01/1970) and change to date format
T = [dt.datetime.fromtimestamp(SST.variables["time"][i]).strftime("%d/%m/%Y") for i in range(0, len(SST.variables["time"][:]))]

#Create variable with months of interest
MOI = ["05", "07"] #May and July
indT = [] #Create variable with layer name
dates = [] #Create date variable
ind = [] #Create index variable
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

########################################################################################################################################
#Set working directory for ArcPy
arcpy.env.workspace = wd
del wd

#Activate Spatial Analyst extension
arcpy.CheckOutExtension("Spatial")

#Allowing outputs to be overwritten
arcpy.env.overwriteOutput = True

#Upload only layers of interest using Arcpy
for i in range(0,len(dates)):
	arcpy.MakeNetCDFRasterLayer_md("erdMBsstdmday_LonPM180_b102_bcb5_c347.nc", "sst", "longitude", "latitude", indT[i], "", dates[i])

#Create a composite raster with all bands previously identified
SST = arcpy.CompositeBands_management(indT, "SST_compMOI.TIF")

#Names of shapefiles containing limits for Machalilla and GSF Marine Reserves
AOIs = [r"LimitesRMCM.shp", r"LimitesRMGSF.shp"]
#Names for output files in the same order as shapefiles
outname = ["RMCM", "RMGSF"]

def ExtractVals(nc, aoi, out):
    i = 0
    for j in aoi:
        #Mask out raster with Marine Reserve limits and save it
        mask = arcpy.sa.ExtractByMask(nc, j)
        mask.save("SST_"+out[i]+".TIF")
        #Create point features from masked out raster
        Pts = arcpy.RasterToPoint_conversion(mask, "SST_Pts"+out[i]+".shp", "Value")
        #Extract values from multiple layers to point features
        SSTVal = arcpy.gp.ExtractMultiValuesToPoints_sa(Pts, "SST_"+out[i]+".tif SST_"+out[i], "NONE")
        #Save attribute table of point features in excel format
        arcpy.TableToExcel_conversion(SSTVal, "SSTValues"+out[i]+".xls", "NAME", "CODE")
        #Read excel table
        SSTPtVal = pd.ExcelFile("SSTValues"+out[i]+".xls")
        #Load first excel sheet
        SST_sheet = SSTPtVal.parse(SSTPtVal.sheet_names[0])
        #Create dictionary to change column names to months
        NewColNames = dict(zip(SST_sheet.columns[3:14], TimeLabs))
        #Change column names using dictionary
        SST_sheet.rename(columns = NewColNames, inplace = True)
        #Write file with corrected column names into a csv file for further analysis
        SST_sheet.to_csv("SST_Values"+out[i]+".csv")
        i+=1

ExtractVals(SST, AOIs, outname)

#Deactivate Spatial Analyst extension
arcpy.CheckInExtension("Spatial")
