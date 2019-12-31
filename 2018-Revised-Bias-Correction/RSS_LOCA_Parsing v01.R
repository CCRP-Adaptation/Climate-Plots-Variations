#Download LOCA data from BOR website - pr, tmin, and tmax variables needs to be downloaded 
  #separately due to large file size. Store ncdf files for each variable in folders named 'pr', 
  #'tmin', and 'tmax'.
#Select Jan1950-Dec2099
#Compare 1/16 historical observations to future projections

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

# use center coordinate ONLY: .03125, .09375, .15625, .21875, .28125, .34375, .40625, .46875, .53125, .59375, .65625, .71875, .78125, .84375, .90625, .96875 
Lat=   40.40625
Lon = -105.71875 

Year= 2040 
Range= 30       # statistics will be computed in the range (Year-Range/2,Year+Range/2)    
HistYears = 56  # 1950-2005


HotTemp = 95    # deg F. Default should be about 100 deg F
ColdTemp = 32   # deg F
PrecipThreshold = 0.05    # inches per day. Precip Threshold (used to measure Drought duration). For many GCMs shoud not 
#  be 0 because models "drizzle". Some investigation necessary.
CFLow = 0.25     # Threshold percentages for defining Climate futures. Default low/high:  0.25, 0.75
CFHigh = 0.75

# Data root directory
WD = paste("~/RSS Plots/", SiteID, "/LOCA", sep="")

# where to write figures, tables, datA
WDP <- paste("~/RSS Plots/", SiteID, "/Figs LOCA", sep="")

dir.create(WDP)         # create if necessary; see warning if it exists
WD_plots <- WDP         #select from WDP list

#################    End of Intials   ############################################
########################### HISTORICAL DATA EXTRACTION  ##########################  

#### PRECIP #####

prDir <- paste(WD, "/pr/1_16obs/", sep="")

Extraction_Pr = nc_open(paste(prDir, "Extraction_pr.nc", sep=""))
t = ncvar_get(Extraction_Pr, "time")
tUnits = ncatt_get(Extraction_Pr, "time", "units")

All_lat = data.frame(Extraction_Pr$dim$lat$vals)    #    All potential coordinates
All_lon = data.frame(Extraction_Pr$dim$lon$vals)

Lat_index = as.numeric(which(All_lat$Extraction_Pr.dim.lat.vals == Lat))   # Get desired grid point indices
Lon_index = as.numeric(which(All_lon$Extraction_Pr.dim.lon.vals == Lon))

All_Precip_Hist = (ncvar_get(Extraction_Pr, Extraction_Pr$var[[1]]))    # Array with all precip data for all cells

#    Extract precip values for desired grid cell, Add date and customary unit columns
Precip_Hist = data.frame(Date = as.Date(t, origin = "1900-1-1"), Precip = All_Precip_Hist[Lon_index,Lat_index, ])
Precip_Hist$PrecipCustom = (Precip_Hist$Precip)*0.03937
Avg_Hist_Precip = (mean(Precip_Hist$PrecipCustom))   #    Calculate mean for entire historical range

######   TMAX ######

tmaxDir <- paste(WD, "/tmax/1_16obs/", sep="")

Extraction_Tmax = nc_open(paste(tmaxDir, "Extraction_tasmax.nc", sep=""))
t = ncvar_get(Extraction_Tmax, "time")
tUnits = ncatt_get(Extraction_Tmax, "time", "units")

# All potential coordinates
All_lat = data.frame(Extraction_Tmax$dim$lat$vals)
All_lon = data.frame(Extraction_Tmax$dim$lon$vals)

Lat_index = as.numeric(which(All_lat$Extraction_Tmax.dim.lat.vals == Lat))     # Get desired grid point indices
Lon_index = as.numeric(which(All_lon$Extraction_Tmax.dim.lon.vals == Lon))

All_Tmax_Hist = (ncvar_get(Extraction_Tmax, Extraction_Tmax$var[[1]]))
Tmax_Hist = data.frame(Date = as.Date(t, origin = "1900-1-1"), Tmax = All_Tmax_Hist[Lon_index,Lat_index, ])
Tmax_Hist$TmaxCustom = (((Tmax_Hist$Tmax)*(9/5) +32))
Avg_Hist_Tmax = (mean(Tmax_Hist$TmaxCustom))    #     Calculate mean for entire historical range

######   TMIN ######

tminDir <- paste(WD, "/tmin/1_16obs/", sep="")

Extraction_Tmin = nc_open(paste(tminDir, "Extraction_tasmin.nc", sep=""))
t = ncvar_get(Extraction_Tmin, "time")
tUnits = ncatt_get(Extraction_Tmin, "time", "units")     # days since 1950-01-01

All_lat = data.frame(Extraction_Tmin$dim$lat$vals)
All_lon = data.frame(Extraction_Tmin$dim$lon$vals)

Lat_index = as.numeric(which(All_lat$Extraction_Tmin.dim.lat.vals == Lat))
Lon_index = as.numeric(which(All_lon$Extraction_Tmin.dim.lon.vals == Lon))

All_Tmin_Hist = (ncvar_get(Extraction_Tmin, Extraction_Tmin$var[[1]]))   #  array with all precip data for all cells
Tmin_Hist = data.frame(Date = as.Date(t, origin = "1900-1-1"), Tmin = All_Tmin_Hist[Lon_index,Lat_index, ])
Tmin_Hist$TminCustom = (((Tmin_Hist$Tmin)*1.8000) +32)
Avg_Hist_Tmin = (mean(Tmin_Hist$TminCustom))   #    Calculate mean for entire historical range

#     Create Master Historical Data Frame.
Historical_all = merge(Precip_Hist, Tmax_Hist, by = c("Date"))
Historical_all = merge(Historical_all, Tmin_Hist, by = c("Date"))

# clean up
rm(Precip_Hist, Tmax_Hist, Tmin_Hist, Lat_index, Lon_index)
rm(Extraction_Pr, Extraction_Tmax, Extraction_Tmin)
rm(All_Precip_Hist, All_Tmin_Hist, All_Tmax_Hist)

####################################################################################
############# END OF HISTORICAL DATA EXTRACTION / BEGIN GCM PARSING ################    

getProjs <- function(ncObject){     # return vector of projection names as characters. Not generic.
  ps <- ncatt_get(ncObject, varid=0, attname='Projections')
  projs <- data.frame((strsplit(ps[[2]], ", ")))
  projs <- as.character(projs[,1])
  return(projs)}


######   TMIN ######

tminWD <- paste(WD, "/tmin/loca5/", sep='')

#  Create vector with names of GCMs
Extraction_Tmin <- nc_open(paste(tminWD, "Extraction_tasmin.nc", sep=''))
GCMs <- getProjs(Extraction_Tmin)
dateVals <- Extraction_Tmin$dim$time$vals   # days from 1900-01-01. Convert: as.POSIXlt(DateVal*24*60*60, origin="1950-01-01")                 

##  Add CheckLongitudeRange here. Historical files lon is 0-360 but projection files are -180 to +180

All_lat = data.frame(Extraction_Tmin$dim$lat$vals)    # get grid coordinates
All_lon = data.frame(Extraction_Tmin$dim$lon$vals) 
cLon <- Lon
if(Lon < 0){if(min(All_lon[1]) > 0 )cLon <- 360 + Lon}
Lat_index = as.numeric(which(All_lat$Extraction_Tmin.dim.lat.vals == Lat))   # Get desired grid point coordinates
Lon_index = as.numeric(which(All_lon$Extraction_Tmin.dim.lon.vals == cLon))

# Tmin already opened
tmaxWD <- paste(WD, "/tmax/loca5/", sep='')
Extraction_Tmax <- nc_open(paste(tmaxWD, "Extraction_tasmax.nc", sep=''))
prWD <- paste(WD, "/pr/loca5/", sep='')                          
Extraction_Pr <- nc_open(paste(prWD, "Extraction_pr.nc", sep=''))

# loop through projections, extracting data and creating data frame
for (i in 1:length(GCMs)){ 
  Tmin_sub = ncvar_get(Extraction_Tmin, start=c(Lon_index,Lat_index,1,i), count=c(1,1,-1,1))   # -1 = all 
  Tmax_sub = ncvar_get(Extraction_Tmax, start=c(Lon_index,Lat_index,1,i), count=c(1,1,-1,1)) 
  Pr_sub   = ncvar_get(Extraction_Pr,   start=c(Lon_index,Lat_index,1,i), count=c(1,1,-1,1))  
  
  if (i==1){        # reading first GCM (level in nc file) for this lat/lon & set up data frames
    Precip <- data.frame(Date=as.Date(dateVals, origin = "1900-1-1"), Pr_sub)   # precip in mm/day 
    names(Precip)[1]="Date"
    names(Precip)[2]=GCMs[i]
    
    Tmin<- data.frame(Date=as.Date(dateVals, origin = "1900-1-1"), Tmin_sub)
    names(Tmin)[1]="Date"
    names(Tmin)[2]=GCMs[i]
    
    Tmax<- data.frame(Date=as.Date(dateVals, origin = "1900-1-1"), Tmax_sub)
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

Historical_all$Date=strptime(Historical_all$Date, "%Y-%m-%d")
months=factor(c("January","February","March","April","May","June","July","August","September","October","November","December"),levels = month.name)

#subset climate variables for selected future year range, centered on the selected year
YearRange=((Year-(Range/2)):(Year+(Range/2))) 
Precip_future=subset(Precip,(Date$year+1900) %in% YearRange) 
Tmax_future=subset(Tmax,(Date$year+1900) %in% YearRange) 
Tmin_future=subset(Tmin,(Date$year+1900) %in% YearRange) 
# Wind_future=subset(Wind,(Date$year+1900) %in% YearRange) 

#subset baseline 20th century climate variables, averaged over 1950-1999
BaselineYearRange=(1950:2005)
Precip_baseline=subset(Precip,(Date$year+1900) %in% BaselineYearRange) 
Tmax_baseline=subset(Tmax,(Date$year+1900) %in% BaselineYearRange)
Tmin_baseline=subset(Tmin,(Date$year+1900) %in% BaselineYearRange) 
# Wind_baseline=subset(Wind,(Date$year+1900) %in% BaselineYearRange) 

Precip_future_melt = melt(Precip_future, id="Date")
Tmax_future_melt = melt(Tmax_future, id="Date")
Tmin_future_melt = melt(Tmin_future, id="Date")
#Wind_future_melt = melt(Wind_future, id="Date")

Future_all = merge(Precip_future_melt, Tmax_future_melt, by=c("Date","variable"))
Future_all = merge(Future_all, Tmin_future_melt, by=c("Date","variable"))
# Future_all = merge(Future_all, Wind_future_melt, by=c("Date","variable"))
names(Future_all) = c("Date", "GCM", "Precip", "Tmax", "Tmin")  #  , "Wind")
Future_all$PrecipCustom = Future_all$Precip*0.03937
Future_all$TmaxCustom = (Future_all$Tmax*(9/5))+32
Future_all$TminCustom = (Future_all$Tmin*(9/5))+32

# rm(Precip_future_melt, Tmax_future_melt, Tmin_future_melt)   #, Wind_future_melt)

Precip_baseline_melt = melt(Precip_baseline, id="Date")
Tmax_baseline_melt = melt(Tmax_baseline, id="Date")
Tmin_baseline_melt = melt(Tmin_baseline, id="Date")
# Wind_baseline_melt = melt(Wind_baseline, id="Date")

Baseline_all = merge(Precip_baseline_melt, Tmax_baseline_melt, by=c("Date","variable"))
Baseline_all = merge(Baseline_all, Tmin_baseline_melt, by=c("Date","variable"))
#Baseline_all = merge(Baseline_all, Wind_baseline_melt, by=c("Date","variable"))
names(Baseline_all) = c("Date", "GCM", "Precip", "Tmax", "Tmin")   #  , "Wind")
Baseline_all$PrecipCustom = Baseline_all$Precip*0.03937
Baseline_all$TmaxCustom = (Baseline_all$Tmax*(9/5))+32
Baseline_all$TminCustom = (Baseline_all$Tmin*(9/5))+32

rm(Precip_baseline_melt, Tmax_baseline_melt, Tmin_baseline_melt, Precip, Tmax, Tmin)  # Wind_baseline_melt, Wind)

####Set Average values for all four weather variables, using all baseline years and all climate models
BaseMeanPr = mean(Baseline_all$PrecipCustom)
BaseMeanTmx = mean(Baseline_all$TmaxCustom)
BaseMeanTmn = mean(Baseline_all$TminCustom)
# BaseMeanWind = mean(Baseline_all$Wind)

####Create Future/Baseline means data tables, with averages for all four weather variables, organized by GCM
Future_Means = data.frame(aggregate(cbind(Future_all$PrecipCustom, Future_all$TmaxCustom, Future_all$TminCustom)
                                    ~ Future_all$GCM, Future_all, mean))   # , Future_all$Wind
names(Future_Means) = c("GCM", "PrecipCustom", "TmaxCustom", "TminCustom")    # , "Wind"
Future_Means$TavgCustom = (Future_Means$TmaxCustom + Future_Means$TminCustom)/2

Baseline_Means = data.frame(aggregate(cbind(Baseline_all$PrecipCustom, Baseline_all$TmaxCustom, Baseline_all$TminCustom)
                                      ~ Baseline_all$GCM, Baseline_all, mean))    #  ,Baseline_all$Wind)
names(Baseline_Means) = c("GCM", "PrecipCustom", "TmaxCustom", "TminCustom")  #  , "Wind")
Baseline_Means$TavgCustom = (Baseline_Means$TmaxCustom + Baseline_Means$TminCustom)/2

#### add delta columns in order to classify CFs
Future_Means$DeltaPr = Future_Means$PrecipCustom - Baseline_Means$PrecipCustom
Future_Means$DeltaTmx = Future_Means$TmaxCustom - Baseline_Means$TmaxCustom
Future_Means$DeltaTmn = Future_Means$TminCustom - Baseline_Means$TminCustom
# Future_Means$DeltaWind = Future_Means$Wind - Baseline_Means$Wind
Future_Means$DeltaTavg = Future_Means$TavgCustom - Baseline_Means$TavgCustom


#### Set limits for CF classification
Pr25 = as.numeric(quantile(Future_Means$DeltaPr, CFLow))
PrAvg = as.numeric(mean(Future_Means$DeltaPr))
Pr75 = as.numeric(quantile(Future_Means$DeltaPr, CFHigh))
Tavg25 = as.numeric(quantile(Future_Means$DeltaTavg, CFLow)) 
Tavg = as.numeric(mean(Future_Means$DeltaTavg))
Tavg75 = as.numeric(quantile(Future_Means$DeltaTavg, CFHigh))
# Wind25 = as.numeric(quantile(Future_Means$DeltaWind, CFLow))
# WindAvg = as.numeric(mean(Future_Means$DeltaWind))
# Wind75 = as.numeric(quantile(Future_Means$DeltaWind, CFHigh))

#### Designate Climate Future
Future_Means$CF1 = as.numeric((Future_Means$DeltaTavg<Tavg & Future_Means$DeltaPr>Pr75) | Future_Means$DeltaTavg<Tavg25 & Future_Means$DeltaPr>PrAvg)
Future_Means$CF2 = as.numeric((Future_Means$DeltaTavg>Tavg & Future_Means$DeltaPr>Pr75) | Future_Means$DeltaTavg>Tavg75 & Future_Means$DeltaPr>PrAvg)
Future_Means$CF3 = as.numeric((Future_Means$DeltaTavg>Tavg25 & Future_Means$DeltaTavg<Tavg75) & (Future_Means$DeltaPr>Pr25 & Future_Means$DeltaPr<Pr75))
Future_Means$CF4 = as.numeric((Future_Means$DeltaTavg<Tavg & Future_Means$DeltaPr<Pr25) | Future_Means$DeltaTavg<Tavg25 & Future_Means$DeltaPr<PrAvg)
Future_Means$CF5 = as.numeric((Future_Means$DeltaTavg>Tavg & Future_Means$DeltaPr<Pr25) | Future_Means$DeltaTavg>Tavg75 & Future_Means$DeltaPr<PrAvg)

#Assign full name of climate future to new variable CF
Future_Means$CF[Future_Means$CF1==1]="Warm Wet"
Future_Means$CF[Future_Means$CF2==1]="Hot Wet"
Future_Means$CF[Future_Means$CF3==1]="Central"
Future_Means$CF[Future_Means$CF4==1]="Warm Dry"
Future_Means$CF[Future_Means$CF5==1]="Hot Dry"
Future_Means$CF=as.factor(Future_Means$CF)
Future_Means$CF = factor(Future_Means$CF,ordered=TRUE,levels=c("Warm Wet","Hot Wet","Central","Warm Dry","Hot Dry"))

#     Remove extraneous Climate Future columns
Future_Means$CF1 = NULL
Future_Means$CF2 = NULL
Future_Means$CF3 = NULL
Future_Means$CF4 = NULL
Future_Means$CF5 = NULL

#     Add column with emissions scenario for each GCM run
Future_Means$emissions[grep("rcp85",Future_Means$GCM)] = "RCP 8.5"
Future_Means$emissions[grep("rcp45",Future_Means$GCM)] = "RCP 4.5"
# Future_Means$emissions[grep("sresb1",Future_Means$GCM)] = "B1"

####Order by date and GCM
Future_all = Future_all[order(Future_all$GCM, Future_all$Date), ]
Baseline_all = Baseline_all[order(Baseline_all$GCM, Baseline_all$Date),]

####Add column with CF classification to Future_all/Baseline_all
CF_GCM = data.frame(GCM = Future_Means$GCM, CF = Future_Means$CF)

Future_all = merge(Future_all, CF_GCM, by="GCM")
Baseline_all = merge(Baseline_all, CF_GCM, by="GCM")

##  EOF  ##
#   v02 - cleaned up code.  Added loop for RCPs.  2015_10_08