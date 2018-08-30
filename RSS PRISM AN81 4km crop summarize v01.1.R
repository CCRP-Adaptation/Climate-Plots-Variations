#   PRISM_AN81_4km_crop_summarize vxx.R
#   John Gross
#   30 Oct 2015

#   Input is single point, same as for extracting projection data.
#   This code buffers the point. Default is one cell buffer (e.g. averaged over 3x3 grid)

#  v01.1 - minor changes to variable names, cleaned up namespace.  16 Nov. Runs without error.
#  v01 - Robust and running

# Run time: iMac ~ 15 min w/ USB3 external drive; office PC: ~ 20 min w/ USB 2.0 (same drive)

library(rgdal)
library(raster)
library(fields)   # for image.plot
library(ggplot2)
library(WriteXLS)
rm(list = ls()) 

# to recover earlier session set WD and
# load("FileName.RData")

#############  Initialize   ##############

    #  for output file names
SiteID <- "BIBE"  # identifier.  Use "" if not desired

    # Coordinates for cell center. Cell size is 0.04166 for 4-km dataset, .008333 for 800-m dataset. 
Lat = 29.1875
Lon = -103.5625

Buffer <- 0.06    # dec degree.  

BeginYr = 1895
EndYr =  2017      
EndMo = 12
Day = 15          # in output files, day of month in Date variable (req by strptime)

          #  Root Out File Dir MUST exist - can only create final subdirectory (not root of this)   
WinDataDir <- "F:/ClimateData/PRISM/PRISM_LT81m_800m"  #Drobo drive containing 800-m resolution 2017 data
WinOFDir <- "~/RSS Plots/BIBE/Figs PRISM"

OPath1 <-"/Volumes/Seagate1_Blue2TB/COLM RSS/Figs PRISM/"
OPath2 <- "/Volumes/Seagate1_Blue2TB/Projects/RSS Climate/HOBE"
MacOFDir <- OPath1

DPath1 <- "/Volumes/Seagate2_4TB/PRISM4k_AN81M2"    
DPath2 <- "/Volumes/Seagate1_Blue2TB/PRISM"        
MacDataDir <- DPath2
rm(OPath1, OPath2, DPath1, DPath2)

if(.Platform$OS.type=="windows"){   
  DataDir <- WinDataDir
  OFDir <- WinOFDir}

if(.Platform$OS.type=="unix"){     # does not distinguish MacOS from others
  DataDir <- MacDataDir
  OFDir <- MacOFDir}

rm(WinDataDir, WinOFDir,MacDataDir,MacOFDir)
  #  Seagate Blue
#PptDir <- paste(DataDir, "/ppt/bil/", sep = "")
#TminDir <- paste(DataDir, "/tmin/bil/", sep = "")
#TmaxDir <- paste(DataDir, "/tmax/bil/", sep = "")

    #   Seagate 4TB/Drobo
PptDir <- paste(DataDir, "/ppt/", sep = "")
TminDir <- paste(DataDir, "/tmin/", sep = "")
TmaxDir <- paste(DataDir, "/tmax/", sep = "")

############################################     
###########  End of Initials  ##############
  
AoaExt <- extent(Lon-Buffer, Lon+Buffer, Lat-Buffer, Lat+Buffer)

GetFileNames <- function(DataDirectoryName){  
  files <- list.files(DataDirectoryName)
  
  afiles <- files[nchar(files) > 26]  
  bFiles <- afiles[grep("^[_a-zA-Z0-9]+\\.bil$", afiles)]   # files ending with.bil 
  return(bFiles)
}
 
GetMonMeans <- function(DataDirName, AOAExt){
    DataFileNames <- GetFileNames(DataDirName)
    DataFile = paste(DataDirName, DataFileNames[1], sep="")
    R <- raster(DataFile)
    RCrp <- crop(R, AOAExt)
    Means = data.frame(File = as.character(DataFile), Mean = cellStats(RCrp, stat='mean', na.rm=TRUE))
    
    for(NFiles in 2:length(DataFileNames)){           
      DataFile = paste(DataDirName, DataFileNames[NFiles], sep="")
      R <- raster(DataFile)
      RCrp <- crop(R, AOAExt)
      M = data.frame(File = DataFile, Mean = cellStats(RCrp, stat='mean', na.rm=TRUE))
      Means <- rbind(Means, M)
    }   # next NFiles
return(Means)}
  
## For Debugging variables
NYr = BeginYr
#EndYr = BeginYr + 2

PptMeans = TminMeans = TmaxMeans = data.frame
for(NYr in BeginYr:EndYr){
  print(NYr); flush.console()
 
  PptYrDir <- paste(PptDir, NYr, "/", sep="")
  TminYrDir <- paste(TminDir, NYr, "/", sep="")
  TmaxYrDir <- paste(TmaxDir, NYr, "/", sep="")
  
  if(NYr == BeginYr){
    PptMeans <- GetMonMeans(PptYrDir, AoaExt)
    TminMeans <- GetMonMeans(TminYrDir, AoaExt)
    TmaxMeans <- GetMonMeans(TmaxYrDir, AoaExt)
  }   # end if(NYr == BeginYr)
  
  if(NYr > BeginYr){
    PptMeans <- rbind(PptMeans, GetMonMeans(PptYrDir, AoaExt))
    TminMeans <- rbind(TminMeans, GetMonMeans(TminYrDir, AoaExt))
    TmaxMeans <- rbind(TmaxMeans, GetMonMeans(TmaxYrDir, AoaExt))
  }   # end else
}	# next NYr 

      #  Deal with dates and seasons
GetSeason <- function(DateVec){
  seas <- as.character(rep(NA, length(DateVec)))
  seas[which(format(DateVec,'%B') %in% c("December", "January", "February"))]<- "Winter"
  seas[which(format(DateVec,'%B') %in% c("March", "April", "May"))]<- "Spring"
  seas[which(format(DateVec,'%B') %in% c("June", "July", "August"))]<- "Summer"
  seas[which(format(DateVec,'%B') %in% c("September", "October", "November"))]<- "Fall"
  return(seas)
}

names(PptMeans)[2] <- "Ppt"
PptMeans$File <- as.character(PptMeans$File)
PptMeans$YearMon <- substring(PptMeans$File[], nchar(PptMeans$File[])-9,nchar(PptMeans$File[])-4)
PptMeans$Date <- strptime(paste(PptMeans$YearMon, Day, sep=""), "%Y%m%d")
Season <- GetSeason(PptMeans$Date)
PptMeans<- cbind(PptMeans, Season)

names(TminMeans)[2] <- "Tmin"
TminMeans$File <- as.character(TminMeans$File)
TminMeans$YearMon <- substring(TminMeans$File[], nchar(TminMeans$File[])-9,nchar(TminMeans$File[])-4)
TminMeans$Date <- strptime(paste(TminMeans$YearMon, Day, sep=""), "%Y%m%d")
Season <- GetSeason(TminMeans$Date)
TminMeans<- cbind(TminMeans, Season)
                           
names(TmaxMeans)[2] <- "Tmax"
TmaxMeans$File <- as.character(TmaxMeans$File)
TmaxMeans$YearMon <- substring(TmaxMeans$File[], nchar(TmaxMeans$File[])-9,nchar(TmaxMeans$File[])-4)
TmaxMeans$Date <- strptime(paste(TmaxMeans$YearMon, Day, sep=""), "%Y%m%d")
Season <- GetSeason(TmaxMeans$Date)
TmaxMeans<- cbind(TmaxMeans, Season)

#  Data conversions

PptMeans$PptIn <- PptMeans$Ppt/25.4     # mm to in
TminMeans$TminF <- TminMeans$Tmin * 9/5 + 32
TmaxMeans$TmaxF <- TmaxMeans$Tmax * 9/5 + 32

      # clean up before writing file
rm(PptDir, PptYrDir, TminDir, TminYrDir, TmaxDir, TmaxYrDir, NYr)

dir.create(OFDir)
setwd(OFDir)
save.image(sprintf("%s_%s_%s_PRISM_PptTminTmax_IntermediateFiles.RData", SiteID, Lat, Lon))
     
      # reality check. Need print & flush to generate output to screen when full script run
print(qplot(PptMeans$Date, PptMeans$PptIn, data=PptMeans)); flush.console()
print(qplot(TmaxMeans$Date, TmaxMeans$TmaxF, data=TmaxMeans)); flush.console()
print(qplot(TminMeans$Date, TminMeans$TminF, data=TminMeans)); flush.console()

#### Create .xslx workbook with all data tables
WriteXLS(c("PptMeans", "TmaxMeans", "TminMeans"), paste(OFDir, "/",SiteID, "_", Lat,"_", Lon,"_PRISM.xlsx", sep=""), BoldHeaderRow = TRUE)


## EOF ##
