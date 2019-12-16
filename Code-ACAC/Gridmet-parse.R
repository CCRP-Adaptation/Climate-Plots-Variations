# Extract GridMET NetCDF files
# Convert to imperial units (inches,Fahrenheit)
# Write to csv

library(ncdf4)
library(reshape2)

rm(list=ls())
####################  Initial input data ######################

#Location of data
DataDir <- "E:/ClimateData/gridmet/"
#Location of output files
OutDir <- "C:/Users/achildress/Documents/RSS/Working/FRSP/Gridmet/"

SiteID = "FRSP" #Park code
Lat = 38.30364
Lon = -77.73877

setwd(DataDir)

####################  Extract daily data from NetCDF files ######################
vars = c("precip", "tmax", "tmin") #folder names

for(var in vars){
  writeLines("")
  print(paste("Extracting", var, "data"))
  writeLines("")
  varDir <- paste(DataDir, var, sep="/")
  nc.files <- c()
    files <- list.files(varDir)
    d=setNames(data.frame(matrix(ncol=2,nrow=0)),c("Dates","Extr"))
    
    for(file in files){
      if(file==files[1]){
        Data1=d
      }
      nc<-nc_open(paste(varDir,file,sep="/"))
      varName <- names(nc$var)
      varUnits <- ncatt_get(nc, varName, "units")$value
      All_lat <- data.frame(nc$dim$lat$vals)
      All_lon <- data.frame(nc$dim$lon$vals)
      Lat_index = as.numeric(which.min(abs(All_lat$nc.dim.lat.vals - Lat)))
      Lon_index = as.numeric(which.min(abs(All_lon$nc.dim.lon.vals - Lon)))
      
      #Loop through data and separate by GCM 
      Date <- as.Date(nc$dim$day$vals, origin = "1900-01-01")
      Extr <- ncvar_get(nc, varName, c(Lon_index, Lat_index, 1), count=c(1,1,-1))
      if(varUnits == "mm"){
        Extr <- Extr/25.4
      }
      else if(varUnits == "K"){
        Extr <- (Extr * (9/5)) - 459.67
      }
      Data <- data.frame(Date, Extr)
      nc_close(nc)
      Data1<-rbind(Data1,Data)
    }
    colnames(Data1)[2]<-var
    Data2<-Data1[order(Date),]
    if (var == vars[1]){GridMet<-Data1} else {GridMet<-cbind(GridMet,Data1[2])}
    assign(var,Data1)
    }

head(GridMet)

####################  Save as ouptut files ######################
setwd(OutDir)
write.csv(GridMet,"GridMet.csv",row.names=F)




