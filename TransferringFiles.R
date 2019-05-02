################################# Copying information across hard drives ##########################################
# Author: Denisse Fierro Arcos
# Version: 3 - Ignores folders in HDs like the recycle bin and spotlight searches
# Date last update: 2019-05-02
# What does it do?: This code lets you copy the entire contents of a hard drive/hard disk/server/USB stick to 
# another hard drive/hard disk/server/USB stick. It can also be used to compare contents between hard drives/hard 
# disks/servers/USB sticks
###################################################################################################################

#Clear workspace
rm(list = ls())

#Upload relevant libraries
library(DescTools)

##################################################################################################################
############################ Update start and end directories below this line ####################################
##################################################################################################################
#Write directory where all files you want to copy are currently found (use forward slash / only)
startdir = "E:/"

#Write the end directory - where all your files will be transfered to
enddir = "Z:/Darwin and Wolf Stereo Sharks 2013-2018/DOVS 2014-2016"
##################################################################################################################
##################################################################################################################


##################################################################################################################
################################## DO NOT CHANGE THE CODE BELOW THIS LINE ########################################
##################################################################################################################
#List of folders to NOT be moved to the server
NoMoveList = c("%RECYCLE.BIN%", "%fseventsd%", "%Spotlight%", "%Trashes%", "%System Volume Information%", 
               "%TemporaryItems%")

#Set working directory to "from disk"
setwd(startdir)
#Create a list of directories and files for "from disk"
StartDir = list.dirs(recursive = T)
#Delete files in recycle bin
StartDir = StartDir[!StartDir %like any% NoMoveList]
StartFiles = list.files(recursive = T)
StartFiles = StartFiles[!StartFiles %like any% NoMoveList]

#Set working directory to "to disk"
setwd(enddir)
#Create a list of directories and files
EndDir = list.dirs(recursive = T)
EndFiles = list.files(recursive = T)

#Compare directory lists
MoveDir = StartDir[which(!StartDir %in% EndDir)]
#If there are any directories missing, then create a new folder in the "to disk"
if(length(MoveDir) > 0){
  for(i in seq_along(MoveDir)){
    dir.create(MoveDir[i])}}

#Compare file lists
MoveFiles = StartFiles[which(!StartFiles %in% EndFiles)]
#If there are any files missing, then copy files in the "to disk"
if(length(MoveFiles) > 0){
  for(i in seq_along(MoveFiles)){
    file.copy(paste(startdir, MoveFiles[i], sep = "/"), paste(enddir, MoveFiles[i], sep = "/"), recursive = F)}}
##################################################################################################################

