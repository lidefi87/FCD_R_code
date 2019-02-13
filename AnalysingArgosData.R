#Prepares Argos data to be used in SSM. Mode output then used for KDE and PVC calculation.
#Author: Denisse Fierro Arcos
#Version: 1
#Date last updated: 2019-02-13

rm(list = ls())

#Required packages
library(rgdal)
library(dplyr)
library(raster)
library(rgeos)
library(lubridate)
library(tidyr)

#Setting working directory - where your data is located in
setwd("C:/Users/Denisse/Documents/TigerSharksSatelliteDataKernels/ALL IN ONE 2016 TIGER TAGS/ALL IN ONE DATA PORTAL")

#Get a list of the directories within the folder - Keep files with tag numbers only
TagNumb = dir()[grepl("[0-9]", dir())]

#Uploading shapefiles needed for analysis - Galapagos Islands and GMR
GalIslWGS84 = shapefile("C:/Users/Denisse/Documents/TigerSharksSatelliteDataKernels/Layers/galapagos_island_wgs84.shp")
GMRWGS84 = shapefile("C:/Users/Denisse/Documents/TigerSharksSatelliteDataKernels/Layers/ReservaMarinaWGS84.shp")

################################### SATELLITE DATA PREPARATION ##########################################
#Cleaning data within each folder
for(i in seq_along(TagNumb)){
  #Set working directory to each tag folder
  setwd(TagNumb[i])
  #Find and upload file containing raw Argos data
  rawData = read.csv(list.files(pattern = "-RawArgos.csv$"), stringsAsFactors = F)
  #Cleaning raw data - Keeping only high quality data (Class = 0, 1, 2, 3, A, B)
  classInt = c(0:3, "A", "B")
  rawData = rawData[which(rawData$Class %in% classInt),]
  #Eliminate duplicate entries for the same date and time
  uniqueData = rawData[!c(duplicated(rawData$PassDate) & duplicated(rawData$PassTime)),]
  #Keep only variables of interest: Tag ID, date, ARGOS location class (lc), longitude, latitude
  varInt = c("PTT", "PassDate", "PassTime", "Class", "Longitude", "Latitude")
  uniqueData = uniqueData[,colnames(uniqueData) %in% varInt]
  #Change variable type of date column to date
  uniqueData$PassDate = parse_date_time(uniqueData$PassDate, orders = "dmy")
  #Unite date and time columns and convert them to date format
  uniqueData = unite(uniqueData, "PassDate", c("PassDate", "PassTime"), sep = " ")
  uniqueData$PassDate = as.POSIXct(uniqueData$PassDate)
  #Create one dataframe containing data for all tags
  if(i == 1){combinedData = uniqueData}else{combinedData = rbind(combinedData, uniqueData)}
  #Save data unique data as csv file for each tag
  write.csv(uniqueData, "unique.csv", row.names = F)
  #Go back one folder before loop starts over
  setwd("..")}
#Delete variables that are no longer needed
rm(uniqueData, rawData, varInt)

#Upload second dataset including satellite locations of tiger sharks 2014-2015
addData = read.csv("SatellitePositionsTigerSharks.csv", stringsAsFactors = F)
#Change date and time to correct format
addData = separate(addData, "date", c("date", "time"), sep = " ")
addData = mutate(addData, date = as.Date(date))
addData = unite(addData, "PassDate", c("date", "time"), sep = " ")
addData$PassDate = as.POSIXct(addData$PassDate)
#Filtering high quality data, classes 0 to 3
addData = addData[which(addData$Class %in% classInt),]
rm(classInt)

#Combining both datasets
combinedData = rbind(combinedData, addData)
rm(addData)


######################## CREATING POINT SHAPEFILE WITH CLEAN DATA FOR SSM ##############################
#Creating shapefile that only includes water points using the Galapagos layer
#Create a vector of unique datapoints with CRS: WGS84
combinedDataLyr = combinedData
coordinates(combinedDataLyr) = ~Longitude+Latitude
proj4string(combinedDataLyr) = CRS("++proj=longlat +datum=WGS84")
crs(combinedDataLyr) = crs(GalIslWGS84)

#Find points that fall on land using the island layer and delete them from the main database
x = over(combinedDataLyr, GalIslWGS84)
#Use previous data frame to find water only points (i.e., rows with NA values) in combined dataframe
combinedData = combinedData[which(is.na(x$id)),]
#Remove variables no longer needed
rm(x)
#Update point shapefile
combinedDataLyr = combinedData
coordinates(combinedDataLyr) = ~Longitude+Latitude
proj4string(combinedDataLyr) = CRS("++proj=longlat +datum=WGS84")
crs(combinedDataLyr) = crs(GalIslWGS84)

#Find points that fall within GMR borders and create a data frame
filteredPts = combinedDataLyr[GMRWGS84,]
rm(combinedDataLyr)

#Plotting maps to check if any points are located on land
plot(filteredPts)
plot(GalIslWGS84, add = T)
plot(GMRWGS84, add = T)
#Zoom option can also be used as follows:
# zoom(GalIslWGS84, ext = extent(combinedData))
# plot(combinedData, add = T)

# #Saving final output as shapefile
# writeOGR(filteredPts, dsn = "C:/Users/Denisse/Documents/TigerSharksSatelliteDataKernels/Layers",
#          layer = "cleanCombinedData", driver="ESRI Shapefile", overwrite_layer = T)



################################# BAYESIAN STATE-SPACE MODEL (SSM) #####################################
library(bsam)
library(plyr)

#Extracting filtered points to dataframe
combinedData = cbind(filteredPts@data, filteredPts@coords)
#Rename dataset columns to perform further analysis
colnames(combinedData) = c("id", "lc", "date", "lon", "lat")
rm(filteredPts)

#Get number of observations per tag number
Obs = ddply(combinedData,. (id), summarise, n = length(date))
combinedData = combinedData[-which(combinedData$id %in% (Obs$id[which(Obs$n < 80)])),]
rm(Obs)

#Fit state-space model
fit = fit_ssm(combinedData, model = "hDCRWS", tstep = 0.5, adapt = 10500)
#Checking fit of model
diag_ssm(fit)
map_ssm(fit)
plot_fit(fit)
dev.off()

#Extracting modelled values and creating a point shapefile
result = get_summary(fit)
coordinates(result) = ~lon+lat
proj4string(result) = CRS("++proj=longlat +datum=WGS84")
crs(result) = crs(GalIslWGS84)
# #Save result as a .csv file
# write.csv(result, "C:/Users/Denisse/Documents/TigerSharksSatelliteDataKernels/Layers/result.csv")


############################# KERNEL DENSITY ESTIMATION USING SSM DATA #################################
library(aspace)
library(spatialEco)

#Calculate standard distance (SDD) to estimate bandwidth (h)
coords = data.frame(lat = result$lat, lon = result$lon)
calc_sdd(points = coords) #Calculate SDD using coordinates
#Plot SDD with Galapagos map as background to visually check results
plot_sdd(plotnew = T)
plot(GalIslWGS84, add = T)
#Save SDD calculation as variable to be used in bandwidth estimation
SDDpts = r.SDD$SDD
#Using Silverman rule of thumb: h = 1.06*SD*n^-0.2 to calculate bandwith
h = 1.06*SDDpts*(nrow(coords)^(-0.2))
#Remove variables no longer in use
rm(coords, r.SDD, SDDpts, sddatt, sddloc)

#Unweighted KDE calculation
kde = sp.kde(x = result, bw = h, n = 10000)
#Remove any land areas from KDE raster
kdeClip = raster::mask(kde, GalIslWGS84, inverse = T)
rm(kde)

############################ CALCULATING PERCENTAGE VOLUME CONTOURS ###################################
#Create percentage volume contours (50%, 75%, 95%)
p95 = raster.vol(kdeClip, p = 0.95)
p75 = raster.vol(kdeClip, p = 0.75)
p50 = raster.vol(kdeClip, p = 0.50)
#Merge all contours together in one raster
PVC = p95+p75+p50
rm(p95, p75, p50)
#Change zero values into NA - Reclassify all other values
PVC[PVC == 0] = NA
PVC[PVC == 3] = 0.5
PVC[PVC == 2] = 0.75
PVC[PVC == 1] = 0.95
#Plot final raster with heat colour ramp
plot(PVC, col = heat.colors(3))
plot(GalIslWGS84, add = T)

#Save final raster as a TIF file
writeRaster(PVC, filename = "C:/Users/Denisse/Documents/TigerSharksSatelliteDataKernels/Layers/KDEresult.tif",
            format = "GTiff", overwrite = T)

#######################################################################################################

