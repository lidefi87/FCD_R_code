# FCD_R_code
Code created for FCD - Shark Project

This folder contains code in R created while working as a Marine Ecologist at the Charles Darwin Foundation's Biomar Shark Team. A list of the code uploaded to this folder as well as a brief description of what was designed to do will be added to this page.

- AnalysingArgosData.R - Cleans satellite locations of tiger sharks downloaded from ARGOS. Creates point shapefile with water only points. Uses points in a Bayesian State-Space Model (SSM), performs Kernel Density Estimation (KDE) on modelled data and create Percentage Volume Countours (PVCs) - 50%, 75% and 95% as a raster (TIF file).

- CheckingScientificNamesAphiaIDs.R - Checks validity of scientific names and retrieves Aphia IDs through WoRMS platform

- ExtractingSSTvalues.py - This scripts extracts SST values for a particular area(s) and month(s) contained within nc files downloaded from NOAA's Aqua MODIS website.

- FishDBCreation - Update a database containing useful information about fish species identified in BRUV videos deployed around the Galapagos.

- SummarisingDOVsData.R - Uses EventMeasure Dot Point Measurements text files (stereo-DOVs) to extract summary of species abundance

- TigerSharkAcousticData.R - Clean acoustic data and create a point plot with acoustic detections over time.

- TransferringFiles.R - This code lets you copy the entire contents of a hard drive/hard disk/server/USB stick to another hard drive/hard disk/server/USB stick. It can also be used to compare contents between hard drives/hard disks/servers/USB sticks.
