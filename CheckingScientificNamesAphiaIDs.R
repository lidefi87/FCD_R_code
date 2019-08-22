##################################################################################################################################
# Checking validity of scientific names and retrieving Aphia IDs through WoRMS platform
# Author: Denisse Fierro Arcos
# Version: 1
# Date last updated: 2019-08-22
# Inputs: Path to folder containing data, name of file containing list of species to be checked (this file must contain three
# rows: Family, Genus, and Species), and a name for the output file.
# Outputs: 
#     Printed on screen: Lists of unmatchable taxa and families, and list of taxa and families that have been corrected.
#     Text file containing Family, Genus, Species and AphiaID for all taxa included in the input list to be used in EventMeasure
##################################################################################################################################

#Clear workspace
rm(list = ls())

################################################# CHANGE THE FOLLOWING PARAMETERS ################################################
#Set working directory - location of folder where your files are kept. Ensure you use / instead of \
WD = "Y:/Common Resources/Projects/SHARK SURVEYS"
#Name of the csv file containing the list of all scientific names to be checked. Write exact name including extension
FN = "SpeciesList_20190820_DFA.csv"
#Name of output file. This file will be saved on the folder given above, unless you specify a different path.
OP = "SpeciesList_20190822_AphiaID"

############################################# DO NOT CHANGE ANYTHING BELOW THIS LINE #############################################
#Upload relevant libraries
library(worms)
library(tidyr)

#Set working directory
setwd(WD)
rm(WD)

#Upload csv file containing names of species to be checked
Wdata = read.csv2(FN, stringsAsFactors = F)
rm(FN)
#Unite Genus and Species columns under a new column called Taxon
Wdata = unite(Wdata, "Taxon", c("Genus", "Species"), sep = " ")

#Extract unique Family names included in the dataset
FamData = unique(Wdata$Family)

#Check scientific names against WoRMS database and get Aphia IDs. Using fuzzy (partial) matching
M = wormsbynames(Wdata$Taxon, match = T)
#Print unmatchable taxa
print("Unmatchable taxa")
Wdata$Taxon[!Wdata$Taxon %in% M$scientificname]
#Print taxa with unaccepted names that have been matched with correct nomenclature
print("Taxa with unaccepted names  matched with correct nomenclature")
M[M$status != "accepted", c("name", "valid_name", "family")]

#Check Family names and get Aphia IDs. Using fuzzy (partial) matching
y = wormsbynames(FamData, marine_only = F, match = T)
#Print unmatchable taxa
print("Unmatchable Families")
FamData[!FamData %in% y$scientificname]
#Print taxa with unaccepted names that have been matched with correct nomenclature
print("Families with unaccepted names  matched with correct nomenclature")
y[y$status != "accepted", c("name", "valid_name")]

#Create new dataset containing Family, Genus, Species and Aphia ID
new = M[!is.na(M$name),c("family", "valid_name", "valid_AphiaID")]
new = separate(new, "valid_name", c("Genus", "Species"), sep = " ")
new = rbind.fill(new, y[!is.na(y$name),c("family", "valid_AphiaID")])
new = new[order(new$family, new$valid_AphiaID),]
colnames(new) = c("!FAMILY", "GENUS", "SPECIES", "APHIAID")
new = new[!duplicated(new),]

#Save the new data frame as a text file to be used in EventMeasure
write.table(new, paste0(OP, ".txt"), sep = "\t", row.names = F, quote = F, na = "")
