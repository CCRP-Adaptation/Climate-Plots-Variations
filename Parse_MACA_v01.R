#######################################################################################
# Script for extracting historic and future projection data from daily CONUS MACAv2 dataset
# Inputs: lat/lon coordinates, directory of .ncdf4 MACA data files, selected GCMs, and historical/future start and end years
# Outputs: List of data frames containing all daily values for each scenario and variable, with data columns separated by GCM 
#   Converts mm to inches and Kelvins to degrees F
#######################################################################################

#v01 - Running - Possible embellishment to include multiple grid cell parsing

library(ncdf4)
library(reshape2)

rm(list=ls())

############################# User-defined initials #################################

####Location for data extraction
SiteID = ""
# Lat and lon coordinates - Cell resolution is 0.041666 degrees, reference coordinates are cell centers
Lat = 37.5
Lon = -106.0
cLon <- Lon + 360 #Adjusts negative lon. values 
SiteID

#Cell resolution is 0.041666 degrees, reference coordinates are cell centers
Lat = 37.5
Lon = -106.0
cLon = Lon + 360 #Adjusts negative lon. values 

###Parameters for extracting data from NetCDF files
#Specify top-level directory where MACA data is stored (all scenarios and variables)
MACADir = "F:/ClimateData/MACAv2Metdata" #no '/' at the end

#Variable and scenario names corresponding to MACA data directory structure
vars = c("pr", "tasmax", "tasmin")
scens = c("historical", "rcp45", "rcp85")

#GCMs to be extracted
GCMs = c('bcc-csm1-1','bcc-csm1-1-m','BNU-ESM','CanESM2','CCSM4','CNRM-CM5','CSIRO-Mk3-6-0',
           'GFDL-ESM2G','GFDL-ESM2M','HadGEM2-CC365','HadGEM2-ES365',
           'inmcm4','IPSL-CM5A-MR','IPSL-CM5A-LR','IPSL-CM5B-LR',
           'MIROC5','MIROC-ESM','MIROC-ESM-CHEM','MRI-CGCM3','NorESM1-M')

#Date ranges to be extracted
Future_StartYear = 2025   #2006-2099
Future_EndYear = 2055     #2006-2099
Hist_StartYear = 1950     #1950-2005
Hist_EndYear = 2005       #1950-2005


######################### End user-defined initials ##############################


################### Extract daily data series from NetCDF files ##################

#Date ranges
Hist_StartDate <- as.Date(paste(Hist_StartYear, "01", "01", sep="-"))
Hist_EndDate <- as.Date(paste(Hist_EndYear, "12", "31", sep="-"))
Hist_DateRange <- seq.Date(Hist_StartDate, Hist_EndDate, by="1 day")
Future_StartDate <- as.Date(paste(Future_StartYear, "01", "01", sep="-"))
Future_EndDate <- as.Date(paste(Future_EndYear, "01", "01", sep="-"))
Future_DateRange <- seq.Date(Future_StartDate, Future_EndDate, by="1 day")

#Will store data frames of time series extracted for each scenario and variable. 
#.ncdf files separated by time period will be combined into a single series for each scenario, variable, and GCM.
DataExtraction <- list(0)
j <-1 #index for output list

for(scen in scens){
  scenDir <- paste(MACADir, scen, sep="/")
  scenIndex <- which(scens == scen)
  if(scen == "historical"){
    StartYr <- Hist_StartYear
    EndYr <- Hist_EndYear
    startDate <- Hist_StartDate
    endDate <- Hist_EndDate
    DateRange <- Hist_DateRange
  }
  else{
    StartYr <- Future_StartYear
    EndYr <- Future_EndYear
    StartDate <- Future_StartDate
    EndDate <- Future_EndDate
    DateRange <- Future_DateRange
  }
  
  for(var in vars){
    writeLines("")
    print(paste("Extracting", scen, var, "data"))
    writeLines("")
    ScenVarDF <- data.frame(Dates=DateRange)
    varDir <- paste(scenDir, var, sep="/")
    nc.files <- c()
    for(GCM in GCMs){
      GCM.sub <- list.files(varDir, pattern = paste("_", GCM, "_", sep=""))
      GCM.sub <- GCM.sub[grepl("\\.nc$", GCM.sub)]
      for(file in GCM.sub){
        year1 <- as.numeric(strsplit(file, "_")[[1]][6])
        year2 <- as.numeric(strsplit(file, "_")[[1]][7])
        if((year1 >= StartYr & year1 <= EndYr) | (year2 >= StartYr & year2 <= EndYr)){
          nc.files <- c(nc.files, file)
        }
      }
    }
    
    #Get information from first file, to be used for all extractions
    nc1 <- nc_open(paste(varDir, nc.files[1], sep="/"))
    varName <- names(nc1$var)
    varUnits <- ncatt_get(nc1, varName, "units")$value
    All_lat <- data.frame(nc1$dim$lat$vals)
    All_lon <- data.frame(nc1$dim$lon$vals)
    Lat_index = as.numeric(which.min(abs(All_lat$nc1.dim.lat.vals - Lat)))
    Lon_index = as.numeric(which.min(abs(All_lon$nc1.dim.lon.vals - cLon)))
    GCM <- strsplit(nc.files[1], "_")[[1]][3]

    #Loop through data and separate by GCM 
    Dates <- as.Date(nc1$dim$time$vals, origin = "1900-01-01")
    Extr <- ncvar_get(nc1, varName, c(Lon_index, Lat_index, 1), count=c(1,1,-1))   
    GCM_data <- data.frame(Dates, Extr)
    nc_close(nc1)
    for(i in 2:length(nc.files)){
      GCM.file <- strsplit(nc.files[i], "_")[[1]][3]
      #Extract data
      nc <- nc_open(paste(varDir, nc.files[i], sep="/"))
      Dates <- as.Date(nc$dim$time$vals, origin="1900-01-01")
      Extr <- ncvar_get(nc, varName, c(Lon_index, Lat_index, 1), count=c(1,1,-1))   
      Extr_DF <- data.frame(Dates, Extr)
      #Add each data series to separate GCM column within scen/var data frame
      if(GCM.file == GCM){
        ##Extract data array and append to data frame
        GCM_data <- rbind(GCM_data, Extr_DF)       
      }
      else{
        print(paste(GCM, "data extracted"))
        #Append extracted data series to data frame
        ScenVarDF <- merge(ScenVarDF, GCM_data, by="Dates")
        if(varUnits == "mm"){
          ScenVarDF$Extr <- ScenVarDF$Extr/25.4
        }
        else if(varUnits == "K"){
          ScenVarDF$Extr <- (ScenVarDF$Extr * (9/5)) - 459.67
        }
        #Rename last column
        colnames(ScenVarDF)[ncol(ScenVarDF)] <- GCM
        #Begin new data series 
        GCM_data <- data.frame(Dates, Extr)
        GCM <- GCM.file
      }
      nc_close(nc)
    }
    ScenVarDF <- merge(ScenVarDF, GCM_data, by="Dates")
    print(paste(GCM, "data extracted"))
    if(varUnits == "mm"){
      ScenVarDF$Extr <- ScenVarDF$Extr/25.4
      Units <- "In"
    }
    else if(varUnits == "K"){
      ScenVarDF$Extr <- (ScenVarDF$Extr * (9/5)) - 459.67
      Units <- "degF"
    }
    else{
      ScenVarDF$Extr <- ScenVarDF$Extr
      Units <- varUnits
    }
    colnames(ScenVarDF)[ncol(ScenVarDF)] <- GCM
    
    ScenVarName <- paste(scen, var, sep=".")
    DataExtraction[[j]] <- ScenVarDF
    names(DataExtraction)[j] <- ScenVarName
    attr(DataExtraction[[j]], "units") <- Units
    j <- j+1
    rm(ScenVarDF, All_lat, All_lon)
  }
} 

### End data extraction ###

##################################################################################

### Summarize daily data ###

#Historical
Hist_Data <- DataExtraction[grep("historical", names(DataExtraction))]
df <- Hist_Data[[1]]
Hist_all <- melt(df, id="Dates")
colnames(Hist_all)[1:2] <- c("Date", "GCM")
colnames(Hist_all)[3] <- paste(vars[1], attr(Hist_Data[[1]], "units"), sep=".")

for(i in 2:length(vars)){
  df <- Hist_Data[[i]]
  df.melt <- melt(df, id="Dates")
  colnames(df.melt) <- c("Date", "GCM", paste(vars[i], attr(Hist_Data[[i]], "units"), sep="."))
  Hist_all <- merge(Hist_all, df.melt, by=c("Date", "GCM"))
}

Historical_all <- data.frame(aggregate(Hist_all[3:ncol(Hist_all)] ~ Date, Hist_all, mean))
Baseline_all <- data.frame(aggregate(Hist_all[3:ncol(Hist_all)] ~ GCM, Hist_all, mean))

#Future
RCP45_Data <- DataExtraction[grep("rcp45", names(DataExtraction))]
RCP85_Data <- DataExtraction[grep("rcp85", names(DataExtraction))]

df1 <- RCP45_Data[[1]]
df2 <- RCP85_Data[[1]]
df1.melt <- melt(df1, id="Dates")
df1.melt$variable <- paste(df1.melt$variable, "rcp45", sep=".")
df2.melt <- melt(df2, id="Dates")
df2.melt$variable <- paste(df2.melt$variable, "rcp85", sep=".")
Future_all <- rbind(df1.melt, df2.melt)
colnames(Future_all)[1:2] <- c("Date", "GCM")
colnames(Future_all)[3] <- paste(vars[1], attr(RCP45_Data[[1]], "units"), sep=".")

for(i in 2:length(vars)){
  df1 <- RCP45_Data[[i]]
  df2 <- RCP85_Data[[i]]
  df1.melt <- melt(df1, id="Dates")
  df1.melt$variable <- paste(df1.melt$variable, "rcp45", sep=".")
  df2.melt <- melt(df2, id="Dates")
  df2.melt$variable <- paste(df2.melt$variable, "rcp85", sep=".")
  df <- rbind(df1.melt, df2.melt)
  colnames(df) <- c("Date", "GCM", paste(vars[i], attr(Future_Data[[i]], "units"), sep="."))
  Future_all <- merge(Future_all, df, by=c("Date", "GCM"))
}


RCP45.sub <- subset(RCP45_Data[[1]], Date$year %in% Fut_YearRange)
RCP45 <- melt(RCP45.sub, id="Date")
colnames(RCP45) <- c("Date", "GCM", strsplit(names(DataExtraction)[[1]], ".")[[1]][2])
for(i in 2:length(RCP45_Data)){
  df <- subset(RCP45_Data[[i]], Date %in% Fut_Dates) 
  df.melt <- melt(df, id="Date")
  RCP45 <- cbind(RCP45, df.melt$var)
  colnames(RCP45)[ncol(RCP45)] <- strsplit(names(df), ".")[[1]][2]
}
RCP45$GCM <- paste(RCP45$GCM, "rcp45", ".")

RCP85.sub <- subset(RCP85_Data[[1]], Date$year %in% Fut_YearRange)
RCP85 <- melt(RCP85.sub, id="Date")
colnames(Future_all) <- c("Date", "GCM", strsplit(names(DataExtraction)[[1]], ".")[[1]][2])
for(i in 2:length(RCP85_Data)){
  df <- subset(RCP85_Data[[i]], Date %in% Fut_Dates) 
  df.melt <- melt(df, id="Date")
  RCP85 <- cbind(RCP85, df.melt$var)
  colnames(RCP85)[ncol(RCP85)] <- strsplit(names(df), ".")[[1]][2]
}
RCP85$GCM <- paste(RCP85$GCM, "rcp85", ".")

Future_all <- rbind(RCP45, RCP85)


################################### END SCRIPT ###################################