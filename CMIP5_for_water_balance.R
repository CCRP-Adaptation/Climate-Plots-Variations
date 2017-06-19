# CMIP5_for_water_balance.R

# Provide a vector of coordinates for sites to be used in water balance model
# Each run generates output tables for a selected GCM and RCP 
# Generates projected time series of monthly mean Tmin, Tmax, and Ppt from desired GCMs
# Loops through sites and generates an output table, formatted to be input into water balance model PRISM_batch script. Can be combined with historical PRISM series.

library(ncdf4)

rm(list=ls())

#############################  Initials  ###################################################
####Input to select grid cell, year of interest, and range over which to compute statistics

SiteID <- "HAFE"  # identifier.  Use "" if not desired 

# use center coordinate ONLY: .0625, .1875, .3125, .4375, .5625, .6875, .8125, .9375
SiteCoords = list(c(39.3125, -77.6875))
SiteNames = c("HAFE")

#Select GCMs
GCMs = c("access1-0.1.rcp85", "bcc-csm1-1.1.rcp85", "miroc-esm-chem.1.rcp85", "csiro-mk3-6-0.1.rcp45")  #Do not include rcp 

HistBeginYr = 1950
HistEndYr =  1999	 

PrjBeginYr = 2000
PrjEndYr = 2099

# CMIP5 data root directory
WD = "~/RSS Plots/DETO/CMIP5"

# where to write figures, tables, data
OutDir <- "~/WaterBalance/DETO/CMIP5/" #Include '/' at end

####################################################################################
############# PARSE Historical ################    

ncDir <- paste(WD, "/1_8obs/", sep="")

#### PRECIP #####
Extraction_Pr = nc_open(paste(ncDir, "Extraction_pr.nc", sep=""))
t = ncvar_get(Extraction_Pr, "time")
tUnits = ncatt_get(Extraction_Pr, "time", "units")

######   TMAX ######
Extraction_Tmax = nc_open(paste(ncDir, "Extraction_tasmax.nc", sep=""))
t = ncvar_get(Extraction_Tmax, "time")
tUnits = ncatt_get(Extraction_Tmax, "time", "units")

######   TMIN ######
Extraction_Tmin = nc_open(paste(ncDir, "Extraction_tasmin.nc", sep=""))
t = ncvar_get(Extraction_Tmin, "time")
tUnits = ncatt_get(Extraction_Tmin, "time", "units")

All_lat = data.frame(Extraction_Pr$dim$lat$vals)    #    All potential coordinates
All_lon = data.frame(Extraction_Pr$dim$lon$vals)

Months = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
Years = HistBeginYr:HistEndYr
YrMon = c()
for(Year in Years){
  for(Month in Months){
    YrMon <- c(YrMon, paste(Year, Month, sep=""))
  }
}

#Output tables
Historic_ppt = data.frame(YrMon = YrMon)
Historic_tmax = data.frame(YrMon = YrMon)
Historic_tmin = data.frame(YrMon = YrMon)

for(i in 1:length(SiteNames)){
  Lat = SiteCoords[[i]][1]
  Lon = SiteCoords[[i]][2]
  SiteName = SiteNames[i]
  print(SiteName)
  cLon <- Lon
  if(Lon < 0){if(min(All_lon[1]) > 0 )cLon <- 360 + Lon}
  Lat_index = as.numeric(which(All_lat$Extraction_Pr.dim.lat.vals == Lat))   # Get desired grid point coordinates
  Lon_index = as.numeric(which(All_lon$Extraction_Pr.dim.lon.vals == cLon))

  All_Precip_Hist = ncvar_get(Extraction_Pr, Extraction_Pr$var[[1]])    # Array with all precip data for all cells
  #    Extract precip values for desired grid cell, Add date and customary unit columns
  Precip_Hist = data.frame(Date = as.Date(t, origin = "1950-1-1"), Precip = All_Precip_Hist[Lon_index,Lat_index, ])

  All_Tmax_Hist = ncvar_get(Extraction_Tmax, Extraction_Tmax$var[[1]])
  Tmax_Hist = data.frame(Date = as.Date(t, origin = "1950-1-1"), Tmax = All_Tmax_Hist[Lon_index,Lat_index, ])

  All_Tmin_Hist = ncvar_get(Extraction_Tmin, Extraction_Tmin$var[[1]])   #  array with all precip data for all cells
  Tmin_Hist = data.frame(Date = as.Date(t, origin = "1950-1-1"), Tmin = All_Tmin_Hist[Lon_index,Lat_index, ])

  #     Create Master Historical Data Frame.
  Historical_all = merge(Precip_Hist, Tmax_Hist, by = c("Date"))
  Historical_all = merge(Historical_all, Tmin_Hist, by = c("Date"))

  ####Order by date
  Historical_all = Historical_all[order(Historical_all$Date), ]
  #Add YrMon column
  Historical_all$YrMon = paste(strftime(Historical_all$Date, "%Y"), strftime(Historical_all$Date, "%m"), sep="")

  ##Mean monthly values
  Monthly_historic_ppt = aggregate(Precip ~ YrMon, Historical_all, FUN = sum)
  Monthly_historic_tmax = aggregate(Tmax ~ YrMon, Historical_all, FUN = mean)
  Monthly_historic_tmin = aggregate(Tmin ~ YrMon, Historical_all, FUN = mean)

  Historic_ppt = cbind(Historic_ppt, Monthly_historic_ppt$Precip)
  Historic_tmax = cbind(Historic_tmax, Monthly_historic_tmax$Tmax)
  Historic_tmin = cbind(Historic_tmin, Monthly_historic_tmin$Tmin)
}

colnames(Historic_ppt) <- c("YrMon", SiteNames)
colnames(Historic_tmax) <- c("YrMon", SiteNames)
colnames(Historic_tmin) <- c("YrMon", SiteNames)


####################################################################################
############# PARSE GCMs ################    

WD2 <- paste(WD, "/bcca5", sep='')
setwd(WD2)

getProjs <- function(ncObject){     # return vector of projection names as characters. Not generic.
  ps <- ncatt_get(ncObject, varid=0, attname='Projections')
  projs <- data.frame((strsplit(ps[[2]], ", ")))
  projs <- as.character(projs[,1])
  return(projs)
}

#  Format YrMon values for data range
Months = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
Years = PrjBeginYr:PrjEndYr
YrMon = c()
for(Year in Years){
  for(Month in Months){
    YrMon <- c(YrMon, paste(Year, Month, sep=""))
  }
}

# Open first .ncdf file
Extraction_Tmin <- nc_open("Extraction_tasmin.nc")
#  Get list of all GCMs in CMIP5 ensemble
allGCMs <- getProjs(Extraction_Tmin)
#  Get vector of dates from projections (daily)
dateVals <- Extraction_Tmin$dim$time$vals   # days from 1950-01-01. Convert: as.POSIXlt(DateVal*24*60*60, origin="1950-01-01")                 
Dates <- as.Date(dateVals, origin = "1950-1-1")
#  Get vectors of lat and lon for CMIP5 dataset
All_lat = data.frame(Extraction_Tmin$dim$lat$vals)    # get grid coordinates
All_lon = data.frame(Extraction_Tmin$dim$lon$vals) 

# Tmin already opened
Extraction_Tmax <- nc_open("Extraction_tasmax.nc")
Extraction_Pr <- nc_open("Extraction_pr.nc")

if(dir.exists(OutDir) == FALSE){
  dir.create(OutDir)
}
setwd(OutDir)

#  Create data frame to convert dates to YrMon format
DailyDF = data.frame(Date = Dates)
DailyDF$YrMon = as.numeric(paste(strftime(DailyDF$Date, "%Y"), strftime(DailyDF$Date, "%m"), sep=""))

# Loop through selected GCMs
for(i in 1:length(GCMs)){
  GCM <- GCMs[i]
  ModelIndex <- which(allGCMs == GCM)
  
  #Monthly output table
  Future_tmin = data.frame(YrMon = YrMon)
  Future_tmax = data.frame(YrMon = YrMon)
  Future_ppt = data.frame(YrMon = YrMon)
  
  # Extract data for each site  
  for(i in 1:length(SiteNames)){
    # Select the desired grid cell
    Lat = SiteCoords[[i]][1]
    Lon = SiteCoords[[i]][2]
    SiteName = SiteNames[i]
    cLon <- Lon
    if(Lon < 0){if(min(All_lon[1]) > 0 )cLon <- 360 + Lon}
    Lat_index = as.numeric(which(All_lat$Extraction_Tmin.dim.lat.vals == Lat))   # Get desired grid point coordinates
    Lon_index = as.numeric(which(All_lon$Extraction_Tmin.dim.lon.vals == cLon))
    
    # Extract data for GCM and create data frame
    Tmin_vals = ncvar_get(Extraction_Tmin, start=c(Lon_index,Lat_index,1,ModelIndex), count=c(1,1,-1,1))   # -1 = all 
    Future_tmin_daily = cbind(DailyDF, Tmin_vals)
    Tmax_vals = ncvar_get(Extraction_Tmax, start=c(Lon_index,Lat_index,1,ModelIndex), count=c(1,1,-1,1)) 
    Future_tmax_daily = cbind(DailyDF, Tmax_vals)
    Ppt_vals = ncvar_get(Extraction_Pr, start=c(Lon_index,Lat_index,1,ModelIndex), count=c(1,1,-1,1))
    Future_ppt_daily = cbind(DailyDF, Ppt_vals)
    
    # Convert to monthly values
    Future_tmin_monthly = aggregate(Tmin_vals ~ YrMon, Future_tmin_daily, FUN = mean)
    Future_tmax_monthly = aggregate(Tmax_vals ~ YrMon, Future_tmax_daily, FUN = mean)
    Future_ppt_monthly = aggregate(Ppt_vals ~ YrMon, Future_ppt_daily, FUN = sum)
    
    # Add data series to output table
    Future_tmin = merge(Future_tmin, Future_tmin_monthly, by="YrMon")
    Future_tmax = merge(Future_tmax, Future_tmax_monthly, by="YrMon")
    Future_ppt = merge(Future_ppt, Future_ppt_monthly, by="YrMon")
    names(Future_tmin)[i+1] = SiteName
    names(Future_tmax)[i+1] = SiteName
    names(Future_ppt)[i+1] = SiteName

    # Combine with historic data
    Tmin = rbind(Historic_tmin, Future_tmin)
    Tmax = rbind(Historic_tmax, Future_tmax)
    Ppt = rbind(Historic_ppt, Future_ppt)
    
    # Save output files
    dir.create(GCM)
    setwd(GCM)
    write.csv(Tmin, "tmin.csv", row.names=FALSE)
    write.csv(Tmax, "tmax.csv", row.names=FALSE)
    write.csv(Ppt, "ppt.csv", row.names=FALSE)
    setwd("..")
  }
}

##  EOF  ##
#   v02 - cleaned up code.  Added loop for RCPs.  2015_10_08