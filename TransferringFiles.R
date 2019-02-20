################################# Copying information across hard drives ##########################################
# Author: Denisse Fierro Arcos
# Version: 2 - allows code to be used when folders are contained within folders
# Date last update: 2019-02-20
# What does it do?: This code lets you copy the entire contents of a hard drive/hard disk/server/USB stick to 
# another hard drive/hard disk/server/USB stick. It can also be used to compare contents between hard drives/hard 
# disks/servers/USB sticks
###################################################################################################################

#Clear workspace
rm(list = ls())

##################################################################################################################
############################ Update start and end directories below this line ####################################
##################################################################################################################
#Write directory where all files you want to copy are currently found (use forward slash / only)
startdir = "E:"

#Write the end directory - where all your files will be transfered to
enddir = "Z:/ETP/Malpelo_2018"
##################################################################################################################
##################################################################################################################


##################################################################################################################
################################## DO NOT CHANGE THE CODE BELOW THIS LINE ########################################
##################################################################################################################
#Set working directory to "from disk"
setwd(startdir)
#Create a list of directories and files for "from disk"
StartDir = list.dirs(recursive = T)
StartFiles = list.files(recursive = T)

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
