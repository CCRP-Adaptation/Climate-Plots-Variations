# CMIP5_GetFutureValues.R

# Provide a vector of coordinates for sites to be used in water balance model. 
# Loops through sites and generates an output table, formatted to be input into water balance model PRISM_batch script. Can be combined with historical PRISM series.

library(RColorBrewer)
library(ggplot2)
library(ggmap)
library(matrixStats)
library(reshape)
library(plyr)
library(WriteXLS)
library(ncdf4)

rm(list=ls())

#############################  Initials  ###################################################
####Input to select grid cell, year of interest, and range over which to compute statistics

SiteID <- "ROMO"  # identifier.  Use "" if not desired 

# use center coordinate ONLY: .0625, .1875, .3125, .4375, .5625, .6875, .8125, .9375
SiteCoords = list(c(40.4375, -105.6875), c(40.4375, -105.5625), c(40.1875, -105.6875), c(40.4375, -105.8125), c(40.1875, -105.6875))
SiteNames <- c("X1", "X2", "X3", "X4", "X5")

StartYr = 2015
EndYr = 2099

# Data root directory
WD = paste("~/RSS Plots/", SiteID, "/CMIP5", sep="")

# where to write figures, tables, datA
OutDir <- "C:/Users/arcarlson/Documents/WaterBalance/ROMO/PRISM_CMIP5_merged"


####################################################################################
############# END OF HISTORICAL DATA EXTRACTION / BEGIN GCM PARSING ################    


getProjs <- function(ncObject){     # return vector of projection names as characters. Not generic.
  ps <- ncatt_get(ncObject, varid=0, attname='Projections')
  projs <- data.frame((strsplit(ps[[2]], ", ")))
  projs <- as.character(projs[,1])
  return(projs)}

WD2 <- paste(WD, "/bcca5", sep='')
setwd(WD2)

#  Create vector with names of GCMs
Extraction_Tmin <- nc_open("Extraction_tasmin.nc")
GCMs <- getProjs(Extraction_Tmin)
dateVals <- Extraction_Tmin$dim$time$vals   # days from 1950-01-01. Convert: as.POSIXlt(DateVal*24*60*60, origin="1950-01-01")                 

# Tmin already opened
Extraction_Tmax <- nc_open("Extraction_tasmax.nc")
Extraction_Pr <- nc_open("Extraction_pr.nc")

##  Add CheckLongitudeRange here. Historical files lon is 0-360 but projection files are -180 to +180

All_lat = data.frame(Extraction_Tmin$dim$lat$vals)    # get grid coordinates
All_lon = data.frame(Extraction_Tmin$dim$lon$vals) 

Months = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
Years = StartYr:EndYr
YrMon = c()
for(Year in Years){
  for(Month in Months){
    YrMon <- c(YrMon, paste(Year, Month, sep=""))
  }
}

#Output tables
Future_ppt_RCP4.5 = data.frame(Date = YrMon)
Future_tmax_RCP4.5 = data.frame(Date = YrMon)
Future_tmin_RCP4.5 = data.frame(Date = YrMon)
Future_ppt_RCP8.5 = data.frame(Date = YrMon)
Future_tmax_RCP8.5 = data.frame(Date = YrMon)
Future_tmin_RCP8.5 = data.frame(Date = YrMon)

for(i in 1:length(SiteNames)){
  Lat = SiteCoords[[i]][1]
  Lon = SiteCoords[[i]][2]
  SiteName = SiteNames[i]
  print(SiteName)
  cLon <- Lon
  if(Lon < 0){if(min(All_lon[1]) > 0 )cLon <- 360 + Lon}
  Lat_index = as.numeric(which(All_lat$Extraction_Tmin.dim.lat.vals == Lat))   # Get desired grid point coordinates
  Lon_index = as.numeric(which(All_lon$Extraction_Tmin.dim.lon.vals == cLon))
  
  # loop through projections, extracting data and creating data frame
  for (i in 1:length(GCMs)){ 
    Tmin_sub = ncvar_get(Extraction_Tmin, start=c(Lon_index,Lat_index,1,i), count=c(1,1,-1,1))   # -1 = all 
    Tmax_sub = ncvar_get(Extraction_Tmax, start=c(Lon_index,Lat_index,1,i), count=c(1,1,-1,1)) 
    Pr_sub   = ncvar_get(Extraction_Pr,   start=c(Lon_index,Lat_index,1,i), count=c(1,1,-1,1))  
    
    if (i==1){        # reading first GCM (level in nc file) for this lat/lon & set up data frames
      Precip <- data.frame(Date=as.Date(dateVals, origin = "1950-1-1"), Pr_sub)   # precip in mm/day 
      names(Precip)[1]="Date"
      names(Precip)[2]=GCMs[i]
      
      Tmin<- data.frame(Date=as.Date(dateVals, origin = "1950-1-1"), Tmin_sub)
      names(Tmin)[1]="Date"
      names(Tmin)[2]=GCMs[i]
      
      Tmax<- data.frame(Date=as.Date(dateVals, origin = "1950-1-1"), Tmax_sub)
      names(Tmax)[1]="Date"
      names(Tmax)[2]=GCMs[i]
      
      rm(Tmin_sub, Pr_sub, Tmax_sub)
      
      #Wind=initial[c(1,5)]  #Wind in m/s
    }    # end if (i==1)
    else{  
      Tmin    <- cbind(Tmin, Tmin_sub)
      Tmax    <- cbind(Tmax, Tmax_sub)
      Precip  <- cbind(Precip,Pr_sub)
      
      names(Tmin)[i+1]<- GCMs[i]
      names(Tmax)[i+1]<- GCMs[i]
      names(Precip)[i+1]<- GCMs[i] 
    }  # end else
  }    # end for loop on GCMs
  
  setwd(WD)
  
  Tmin$Date =strptime(Tmin$Date, "%Y-%m-%d")
  Tmax$Date =strptime(Tmax$Date, "%Y-%m-%d")
  Precip$Date =strptime(Precip$Date, "%Y-%m-%d")
  
  months=factor(c("January","February","March","April","May","June","July","August","September","October","November","December"),levels = month.name)
  
  #subset climate variables for selected future year range, centered on the selected year
  YearRange=(StartYr:EndYr) 
  Precip_future=subset(Precip,(Date$year+1900) %in% YearRange) 
  Tmax_future=subset(Tmax,(Date$year+1900) %in% YearRange) 
  Tmin_future=subset(Tmin,(Date$year+1900) %in% YearRange) 
  # Wind_future=subset(Wind,(Date$year+1900) %in% YearRange) 
  
  Precip_future_melt = melt(Precip_future, id="Date")
  Tmax_future_melt = melt(Tmax_future, id="Date")
  Tmin_future_melt = melt(Tmin_future, id="Date")
  #Wind_future_melt = melt(Wind_future, id="Date")
  
  Future_all = merge(Precip_future_melt, Tmax_future_melt, by=c("Date","variable"))
  Future_all = merge(Future_all, Tmin_future_melt, by=c("Date","variable"))
  # Future_all = merge(Future_all, Wind_future_melt, by=c("Date","variable"))
  names(Future_all) = c("Date", "GCM", "Precip", "Tmax", "Tmin")  #  , "Wind")
  
  # rm(Precip_future_melt, Tmax_future_melt, Tmin_future_melt)   #, Wind_future_melt)
  rm(Precip, Tmax, Tmin)  # Wind_baseline_melt, Wind)
  
  ####Order by date
  Future_all = Future_all[order(Future_all$Date), ]
  #Add YrMon column
  Future_all$YrMon = paste(strftime(Future_all$Date, "%Y"), strftime(Future_all$Date, "%m"), sep="")
  
  ##Separate values by RCP
  #     Add column with emissions scenario for each GCM run
  Future_all$emissions[grep("rcp85",Future_all$GCM)] = "RCP 8.5"
  Future_all$emissions[grep("rcp45",Future_all$GCM)] = "RCP 4.5"
  Future_all_RCP4.5 = subset(Future_all, Future_all$emissions == "RCP 4.5")
  Future_all_RCP8.5 = subset(Future_all, Future_all$emissions == "RCP 8.5")
  
  ##Mean monthly values
  Monthly_future_ppt_RCP4.5 = aggregate(Precip ~ YrMon, Future_all_RCP4.5, FUN = summarize())
  Monthly_future_tmax_RCP4.5 = aggregate(Tmax ~ YrMon, Future_all_RCP4.5, FUN = mean)
  Monthly_future_tmin_RCP4.5 = aggregate(Tmin ~ YrMon, Future_all_RCP4.5, FUN = mean)
  
  Monthly_future_ppt_RCP8.5 = aggregate(Precip ~ YrMon, Future_all_RCP8.5, FUN = sum)
  Monthly_future_tmax_RCP8.5 = aggregate(Tmax ~ YrMon, Future_all_RCP8.5, FUN = mean)
  Monthly_future_tmin_RCP8.5 = aggregate(Tmin ~ YrMon, Future_all_RCP8.5, FUN = mean)
  
  Future_ppt_RCP4.5 = cbind(Future_ppt_RCP4.5, Monthly_future_ppt_RCP4.5$Precip)
  Future_tmax_RCP4.5 = cbind(Future_tmax_RCP4.5, Monthly_future_tmax_RCP4.5$Tmax)
  Future_tmin_RCP4.5 = cbind(Future_tmin_RCP4.5, Monthly_future_tmin_RCP4.5$Tmin)
  Future_ppt_RCP8.5 = cbind(Future_ppt_RCP8.5, Monthly_future_ppt_RCP8.5$Precip)
  Future_tmax_RCP8.5 = cbind(Future_tmax_RCP8.5, Monthly_future_tmax_RCP8.5$Tmax)
  Future_tmin_RCP8.5 = cbind(Future_tmin_RCP8.5, Monthly_future_tmin_RCP8.5$Tmin)
}

colnames(Future_ppt_RCP4.5) <- c("Date", SiteNames)
colnames(Future_tmax_RCP4.5) <- c("Date", SiteNames)
colnames(Future_tmin_RCP4.5) <- c("Date", SiteNames)
colnames(Future_ppt_RCP8.5) <- c("Date", SiteNames)
colnames(Future_tmax_RCP8.5) <- c("Date", SiteNames)
colnames(Future_tmin_RCP8.5) <- c("Date", SiteNames)

setwd(OutDir)
write.csv(Future_ppt_RCP4.5, "CMIP5_ppt_45.csv")
write.csv(Future_tmax_RCP4.5, "CMIP5_tmax_45.csv")
write.csv(Future_tmin_RCP4.5, "CMIP5_tmin_45.csv")
write.csv(Future_ppt_RCP8.5, "CMIP5_ppt_85.csv")
write.csv(Future_tmax_RCP8.5, "CMIP5_tmax_85.csv")
write.csv(Future_tmin_RCP8.5, "CMIP5_tmin_85.csv")


##  EOF  ##
#   v02 - cleaned up code.  Added loop for RCPs.  2015_10_08