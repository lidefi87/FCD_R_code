###########################################################################################################################
#Updating fish database for the sharks project
#If using the IUCN package (rredlist) for the first time, you must get an API key. Instructions can be found by typing
#rredlist::rl_use_iucn()
#Version: 1
#Date last updated: 2019-10-29
#Author: Denisse Fierro Arcos
###########################################################################################################################

#Clear workspace
rm(list = ls())


#Location of the main folder containing all data to be analysed
DD = "C:/Users/denisse.fierro/Documents/RESPALDO NAS DENISSE/Publications/D&WSeasonalPatterns/Aug2017_Cold/"

#Calling packages to be used
library(tidyr)
library(worrms)
library(rfishbase)

#Variable containing island names
ISL = c("Darwin", "Wolf")
#Set main directory
setwd(DD)
#Get fish database to be updated
FishDB = read.csv("../FishDB.csv", stringsAsFactors = F)
#Get month and year of campaign
x = unlist(strsplit(DD, "/"))
x = x[grepl("([0-9]{4})", x)]
MMYY = unlist(strsplit(x, "_"))[1]
Season = unlist(strsplit(x, "_"))[2]
rm(x)
#Obtaining a list of all folders inside the main directory
D = list.dirs()
#Find locations of MaxN files only 
DMaxN = D[grepl("MaxN$", D)]

#Create one big dataset using all data found inside folder
#Create empty data frame
MaxN = data.frame()
#Loop to access data and create final dataset
for(i in seq_along(DMaxN)){
  setwd(DMaxN[i])
  for(j in seq_along(ISL)){
    if(grepl(ISL[j], getwd()) == T){break}}
  x = read.delim(list.files(pattern = "Darwin|Wolf"), skip = 4)
  x$Island = ISL[j]
  x$Campaign = MMYY
  x$Season = Season
  MaxN = rbind(MaxN, x)
  rm(x, j)
  setwd(DD)}
#Creating new column uniting Genus and Species
MaxN = unite(MaxN, "SpeciesName", c("Genus", "Species"), sep = " ", remove = F)

#Extracting list of unique species to create fish database
DSp = unique(MaxN$SpeciesName)
#Remove any instances where no Genus is included
DSp = DSp[!grepl("^ ", DSp)]

#Create database with information about all species identified using WoRMS
M = wm_records_names(DSp, fuzzy = T)
#Unlist results from WoRMS query and create dataframe
M = do.call(rbind.data.frame, M)
#Keep only columns with information needed for further analysis
M = M[,c("scientificname", "valid_AphiaID", "valid_name", "family", "genus", "rank")]
#Keep only rows containing the scientific names used in EventMeasure outputs
M = M[M$scientificname %in% DSp,]
#Eliminate any duplicate entries
M = M[!duplicated(M),]

#Merge all fish databases into one
FishDB = merge(M, FishDB, by.x = "scientificname", by.y = "Species", all = T)
#Extract species for which data is missing
sp = FishDB$ValidName[is.na(FishDB$CommonName)]
#Get data from FishBase
x = species(sp)

DMaxN = merge(DMaxN, M, by.x = "SpeciesName", by.y = "scientificname", all.x = T)

#Check and update IUCN status of species included in the fish database
library(rredlist)
for(i in 1:nrow(FishDB)){
  x = rl_search(FishDB$ValidName[i])
  FishDB$IUCNStatus[i] = x$result$category}




