###############################################################################################################################
#Processes acoustic data and produces a plot of pings received over time.
#Author: Denisse Fierro Arcos
#Version: 1
#Date last updated: 2019-04-29
###############################################################################################################################

#Remove all variables from environment
rm(list = ls())

#Upload relevant libraries
library(lubridate)
library(tidyr)
library(stringr)

#Set working directory
setwd("C:/Users/denisse.fierro/Documents/RESPALDO NAS DENISSE/WorkingPublications/TigerSharksAcousticData")

###############################################################################################################################
#Upload databases
#Individual shark data
TigerData = read.csv2("TigerSharkTaggingDB2019clean.csv")
#Pooled shark acoustic detections
TigerAcoustic = read.delim2("TigerSharkAcousticData.txt", skipNul = T)

###############################################################################################################################
#Clean Tiger database
#Convert to DateTagged column date format
TigerData$DateTagged = parse_date_time(TigerData$DateTagged, orders = "dmy")
#Include only sharks tagged prior to August 2017
TigerData = TigerData[TigerData$Date < as.Date("2017-08-01"),]

###############################################################################################################################
#Clean TigerAcoustic database
#Fix name of the first column
colnames(TigerAcoustic)[1] = "Date"

#Merge Date and Time columns together and convert to date format
TigerAcoustic = unite(TigerAcoustic, "Date", c("Date", "Time"), sep = " ")
TigerAcoustic$Date = parse_date_time(TigerAcoustic$Date, orders = "dmy HMS")
#Correct time zone in Date column from GMT/UTC to Galapagos using Sys.timezone()
TigerAcoustic$Date = as.POSIXct(format(TigerAcoustic$Date, usetz = T, tz = Sys.timezone()))
#Include only data between October 2014 and October 2017
TigerAcoustic = TigerAcoustic[TigerAcoustic$Date < as.Date("2017-10-31"),]

#Include only data from Santa Cruz island: Bachas y Salinas sites
StaCruz = c("Bachas", "Salinas")
TigerAcoustic = TigerAcoustic[TigerAcoustic$ReceiverName %in% StaCruz,]
rm(StaCruz)

#Add new column VEMCOcode to TigerAcoustic to match data by TigerData
TigerAcoustic$VEMCOcode = as.integer(str_sub(TigerAcoustic$Transmitter, start = -5))
#Include only data for tagged sharks include in the TigerData DB
TigerAcoustic = TigerAcoustic[TigerAcoustic$VEMCOcode %in% TigerData$VEMCOcode,]

#Exclude tags with detections on a single day only
ExcTag = c(24893, 22978, 22954, 52980)
TigerAcoustic = TigerAcoustic[!TigerAcoustic$VEMCOcode %in% ExcTag,]
rm(ExcTag)

#Add date of tagging to TigerAcoustic DB before drawing plots
TigerAcoustic = merge(TigerAcoustic, TigerData[,5:6], by = "VEMCOcode", )

#Add symbol after shark ID
SymbID = c(24889, 52983, 24890)
for(i in 1:nrow(TigerAcoustic)){
  if(TigerAcoustic$VEMCOcode[i] %in% SymbID){
    TigerAcoustic$VEMCOcode[i] = paste0(TigerAcoustic$VEMCOcode[i],"^")}}
rm(SymbID, i)

#Set VEMCOcode column as factor in TigerAcoustic
TigerAcoustic$VEMCOcode = as.factor(TigerAcoustic$VEMCOcode)

#Order TigerAcoustic DB based on tagging date
TigerAcoustic$VEMCOcode = factor(TigerAcoustic$VEMCOcode, levels = unique(TigerAcoustic[order(TigerAcoustic$DateTagged), 
                                                                                        "VEMCOcode"]))

###############################################################################################################################
#Create variable with min and max dates found in TigerAcoustic DB to be used in x axis creation
# # Using base command plot
# y = as.POSIXct(range(TigerAcoustic$Date), "months")
# #Create a vector within the dates identified above
# yseq = format(seq(y[1], y[2], by = "month"), format = "%m-%Y")
# 
# #Plot filtered data
# par(mar = c(5,4.75,1,1))
# plot(TigerAcoustic$Date, TigerAcoustic$VEMCOcode, yaxt = "n", xaxt = "n", ylab = "", xlab = "")
# axis(2, at = TigerAcoustic$VEMCOcode, labels = paste(TigerAcoustic$VEMCOcode), 
#      las = 2, cex.axis = 0.75)
# axis(1, at = seq(y[1], y[2], by = "month"), labels = paste(yseq), las = 2, cex.axis = 0.75)
# mtext("Acoustic Tag ID", side = 2, line = 3.5, cex = 0.9)
# mtext("Date", side = 1, line = 3.5, cex = 0.8)
# dev.off()

#Using ggplot
library(ggplot2)
TigerAcoustic2 = TigerAcoustic
#Change Date column to Date format
TigerAcoustic2$Date = as.Date(TigerAcoustic2$Date)
#Create plot with Years as major breaks and Months as minor breaks
ggplot(TigerAcoustic2, mapping = aes(x = Date, y = VEMCOcode))+geom_point()+theme_bw()+labs(y = "Acoustic Tag ID")+
  scale_x_date(date_breaks = "year", date_minor_breaks = "month", date_labels = "%Y")+
  theme(text = element_text(size = 14, family = "sans"))
rm(TigerAcoustic2)
