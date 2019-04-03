#Prepares Argos data to be used in SSM. Mode output then used for KDE and PVC calculation.
#Author: Denisse Fierro Arcos
#Version: 2
#Date last updated: 2019-03-21

rm(list = ls())

#Required packages
library(rgdal)
library(dplyr)
library(raster)

#Setting working directory - where your data is located in
setwd("C:/Users/denisse.fierro/Documents/RESPALDO NAS DENISSE/TigerSharksSatelliteDataKernels/ALL IN ONE 2016 TIGER TAGS/ALL IN ONE DATA PORTAL")

#Uploading shapefiles needed for analysis
GalIslWGS84 = shapefile("C:/Users/denisse.fierro/Documents/RESPALDO NAS DENISSE/TigerSharksSatelliteDataKernels/Layers/galapagos_island_wgs84.shp")
GMRWGS84 = shapefile("C:/Users/denisse.fierro/Documents/RESPALDO NAS DENISSE/TigerSharksSatelliteDataKernels/Layers/ReservaMarinaWGS84.shp")

################################### UPLOADING DATABASES ##########################################
#Upload first clean dataset with locations for 2016
combinedData = read.csv("SatPositions2016.csv")
combinedData$PassDate = as.POSIXct(combinedData$PassDate)

#Upload second dataset including satellite locations of tiger sharks 2014-2015 - No duplicates included
addData = read.csv("SatellitePositionsTigerSharksEdited.csv", stringsAsFactors = F)
#Change date and time to correct format
addData$PassDate = as.POSIXct(addData$PassDate)

#Combining both datasets
combinedData = rbind(combinedData, addData)
rm(addData)

##See version 1 to check how data was cleaned

######################## CREATING POINT SHAPEFILE WITH CLEAN DATA FOR SSM ################################
library(rgeos)

#Creating shapefile that only includes water points using the Galapagos layer
#Create a vector of unique datapoints with Datum WGS84
combinedDataLyr = combinedData
coordinates(combinedDataLyr) = ~Longitude+Latitude
proj4string(combinedDataLyr) = crs(GalIslWGS84)

#Create shapefile based on an internal buffer of 5000 m within the islands to account for LC B margin of error
GalIsl_5kmIn = shapefile("C:/Users/denisse.fierro/Documents/RESPALDO NAS DENISSE/TigerSharksSatelliteDataKernels/Layers/GalIsl_5kmIn.shp")

#Internal buffer was created based on Galapagos layer with UTM zone 15S WGS84 projection
# x = buffer(GalIslUTM, width = -5000)
#Resulting layer was transformed to WGS84
# GalIsl_5kmIn = spTransform(x, crs(GalIslWGS84))

#Use internal buffer layer to identify locations that fall on land and delete them from the main database
combinedDataLyr = combinedDataLyr[-which(gIntersects(combinedDataLyr, GalIsl_5kmIn, byid = T)),]
rm(GalIsl_5kmIn)

################################# BAYESIAN STATE-SPACE MODEL (SSM) #####################################
library(bsam)
library(plyr)
library(ggplot2)
library(lubridate)

#Extracting filtered points to dataframe
combinedData = cbind(combinedDataLyr@data, combinedDataLyr@coords)

#Rename dataset columns to perform further analysis
colnames(combinedData) = c("id", "lc", "date", "lon", "lat")
combinedData = combinedData[order(combinedData$id, combinedData$date),]
# lapply(combinedData, class)

#Plot tracks with coordinates and varying colour based on date
ggplot(combinedData[38:43,], aes(lat, lon, col = date))+geom_path()+geom_point()

#Calculate number of days between observations initiating from the same tag
for(i in 2:nrow(combinedData)){
  if(combinedData$id[i] == combinedData$id[i-1]){
    combinedData$days[i] = difftime(combinedData$date[i], combinedData$date[i-1], units = "days")}else{
      combinedData$days[i] = NA}}

#Find gaps in observations of seven days or longer
which(combinedData$days >= 7)
#Find NAs to determine the start of next tag observations
which(is.na(combinedData$days))
#If week long gaps or more are found then split the track by adding _# to the unique ID
combinedData$id[xxxx:xxxx] = paste(combinedData$id[xxxx], xxx, sep = "_")
#Get a summary of total observations per unique tag ID. Use information to remove these observations
#from database
Obs = ddply(combinedData,. (id), summarise, n = length(date))
combinedData = combinedData[-which(combinedData$id %in% (Obs$id[Obs$n < 10])),]
rm(Obs)

#Check distribution of observations over time
ggplot(combinedData, aes(x = date, y = id))+geom_point()

#Calculate and visually cumulative distribution
combinedData = combinedData[order(combinedData$days),] #data ordered based on gap days
combinedData$count = 1
for(i in 2:nrow(combinedData)){
  combinedData$cum[1] = combinedData$count/sum(combinedData$count, na.rm = T)
  combinedData$cum[i] = sum(combinedData$count[1:i], na.rm = T)/sum(combinedData$count, na.rm = T)}
ggplot(combinedData, aes(x = days))+stat_ecdf(geom = "step")

#Delete any tracks with less than 10 observations
Obs = ddply(combinedData,. (id), summarise, n = length(date))
combinedData = combinedData[-which(combinedData$id %in% (Obs$id[Obs$n < 10])),]

#Split database per year because of large time gaps between years
Data2014 = combinedData[year(combinedData$date) == 2014,1:5]
Data2015 = combinedData[year(combinedData$date) == 2015,1:5]
Data2016 = combinedData[year(combinedData$date) == 2016,1:5]
Data2017 = combinedData[year(combinedData$date) == 2017,1:5]
rm(combinedData)

#Fit state-space model to yearly datasets with a 12 hour time step
fit2014 = fit_ssm(Data2014, model = "hDCRWS", tstep = 0.5)
fit2015 = fit_ssm(Data2015, model = "hDCRWS", tstep = 0.5)
fit2016 = fit_ssm(Data2016, model = "hDCRWS", tstep = 0.5)
fit2017 = fit_ssm(Data2017, model = "hDCRWS", tstep = 0.5)

# #Checking fit of model
# diag_ssm(fit)
# map_ssm(fit)
# plot_fit(fit)
# dev.off()

#Extracting results for each year modelled into one dataset and creating a point shapefile
result = rbind(get_summary(fit2014), get_summary(fit2015), get_summary(fit2016), 
               get_summary(fit2017))
coordinates(result) = ~lon+lat
crs(result) = crs(GalIslWGS84)

#Find points that fall on land using the island layer and delete them from shapefile
int = gIntersects(result, GalIslWGS84, byid = T)
clip = apply(int == F, MARGIN = 2, all)
result = result[which(clip),]
#Remove variables no longer needed
rm(int, clip)


############################# KERNEL DENSITY ESTIMATION USING SSM DATA #################################
library(aspace)
library(spatialEco)

#Project shapefile to CRS: UTM Zone 15S, Datum: WGS84 before any spatial analysis
result_UTM = spTransform(result, CRS("+init=epsg:32715"))

#Reproject GMR layer to UTM Zone 15S
GMR_UTM = spTransform(GMRWGS84, crs(result_UTM))
#Exclude any points outside the marine reserve before kernel analysis
resultGMR_UTM = result_UTM[GMR_UTM,]

#Extract coordinates from filtered SSM results
coordUTM = as.data.frame(resultGMR_UTM@coords)
#Calculate standard distance (SDD) to estimate bandwidth (h)
calc_sdd(points = coordUTM) #Calculate SDD using coordinates
#Plot SDD with Galapagos map as background to visually check results
plot_sdd(plotnew = T)
plot(GalIslUTM, add = T)
#Save SDD calculation as variable to be used in bandwidth estimation
SDDpts = r.SDD$SDD
#Using Silverman rule of thumb: h = 1.06*SD*n^-0.2 to calculate bandwith
h = 1.06*SDDpts*(nrow(coordUTM)^(-0.2))
#Remove variables no longer in use
rm(coordUTM, r.SDD, SDDpts, sddatt, sddloc)

#Unweighted KDE calculation
kde = sp.kde(x = resultGMR_UTM, bw = h, n = 10000)
#Remove any land areas from KDE raster
kdeClip = raster::mask(kde, GalIslUTM, inverse = T)
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
plot(GalIslUTM, add = T)

#Calculate area in m^2 covered by each contour
tapply(area(PVC), PVC[], sum)
#Could also calculate area by adding all pixels from each category and multiply
#this by the resolution of the raster. The area will be in m^2
res(PVC)[1] * res(PVC)[2] * table(PVC[])

#######################################################################################################

#Calculate proportion of SSM locations within 5 and 10 km from turtle nesting beaches
#Import locations of turtle nesting beaches
Tort = shapefile("C:/Users/Denisse/Documents/TigerSharksSatelliteDataKernels/Layers/PlayasTortugas.shp")
#Create buffers (5 and 10 km) using turtle nesting beaches (unit is in meters)
x = buffer(Tort, width = 5000)
y = buffer(Tort, width = 10000)
#Clip out land areas using Galapagos layer
Tort_5km = erase(x, GalIslWGS84)
Tort_10km = erase(y, GalIslWGS84)
#Remove unused variables
rm(Tort, x, y)

#Extract points within buffers
Pts_5km = intersect(cleanData, Tort_5km)
Pts_10km = intersect(cleanData, Tort_10km)

#Calculate proportion of points within each buffer area
prop5km = nrow(Pts_5km)/nrow(cleanData)
prop10km = nrow(Pts_10km)/nrow(cleanData)

# #Save all shapefiles created
# shapefile(Pts_5km, filename = "C:/Users/Denisse/Documents/TigerSharksSatelliteDataKernels/Layers/5kmPts.shp",
#           overwrite = T)

