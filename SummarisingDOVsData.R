###################################################################################################################################
#Uses EventMeasure Dot Point Measurements text files (stereo-DOVs) to extract summary of species abundance
#Author: Denisse Fierro Arcos
#Version: 1
#Date last updated: 2019-08-01
###################################################################################################################################

#Clear all information from your environment
rm(list = ls())

#Set working directory - where your files are located, ideally all your files will be in the same folder.
#Ensure only forward slash is used "/"
setwd("C:/Users/denisse.fierro/Desktop")
#Name of input files - if not in the above folder, include complete address to location of file
#Ensure names/addresses are between quotes ("")
TextFileEM = "ArcoArenal270118_QltyChecked_Dot Point Measurements.txt" #Text file from DOVS analysis in EventMeasure
NonInterestList = "NonInterestFamilyList.csv" #CSV file containing a list of families of No Interest to the project
OutputFileName = "ArcoArenal270118_NonInterestAbundance.csv" #Name you would like to give to output file

######################################Do NOT CHANGE ANY CODE BELOW THIS LINE#######################################################
#Upload useful libraries
library(plyr)

#Upload files previously identified
InData = read.delim(TextFileEM, header = T, skip = 3)
FamList = read.csv2(NonInterestList)

#Delete any empty columns from InData
InData = InData[,-which(grepl(glob2rx("x*"), tolower(colnames(InData))))]

#Extract families of No Interest for the project
InData = InData[which(InData$Family %in% FamList$FamilyName),]

#Extract total number of individuals per species
NonInterest = ddply(InData,. (OpCode, Family, Genus, Species), summarise, TotInd = sum(Number))

#Output summary as a csv file
#Check that OutputFileName ends in .csv
if(grepl("*.csv$", OutputFileName) == F){
  OutputFileName = paste0(OutputFileName, ".csv")}
write.csv(NonInterest, file = OutputFileName, row.names = F)
