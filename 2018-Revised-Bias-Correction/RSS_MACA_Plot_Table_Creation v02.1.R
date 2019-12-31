# RSS_Plot_Table_Creation vxx.R
# v02.1 - added Future_Means to xlsx output.
# v02 STABLE - 22 Oct 2015 - days below ColdTemp added and debugged.

rm(list=ls())

################################################## INITIALS ##################################################

SiteID = "CHCU"

#Directory and RData file where daily data series is stored
DataDir = "~/RSS/Parks/CHCU/Figs MACA"
DataFile = "CHCU_36.02_-107.898_MACA_Daily_Data_Extraction.RData"

#Year range for summarizing future climate (Year - Range/2) to (Year + Range/2)
Year = 2040 #Central year
Range = 30  #Number of years to summarize (should be at least 30)

# Threshold percentages for defining Climate futures. Default low/high:  0.25, 0.75
CFLow = 0.25     
CFHigh = 0.75
CFs = c("Warm Wet", "Hot Wet", "Central", "Warm Dry", "Hot Dry") #Use spaces and characters only

#Temperature/precip threshold values
HotTemp = 95    # deg F. Default should be about 100 deg F
ColdTemp = 32    # deg F
PrecipThreshold = 0.05    # inches per day. Precip Threshold (used to measure Drought duration). For many GCMs shoud not 
#  be 0 because models "drizzle". Some investigation necessary.
QuantileLow = 0.05   #Quantiles for temperature threshold calculations
QuantileHigh = 0.95

#Month and season names 
months=factor(c("January","February","March","April","May","June","July","August","September","October","November","December"),levels = month.name)
seasons=factor(c("Winter", "Spring", "Summer", "Fall"))
levels(seasons)=seasons

################################################## END INITIALS ###############################################

setwd(DataDir)
load(DataFile)

################################################### FUNCTION DEFINITIONS ########################################

#### FUNCTION TO CALCULATE SEASON FROM 'DATE' ####

getSeason <- function(DATES) {
  WS <- as.Date("2012-12-21", format = "%Y-%m-%d") # Winter Solstice
  SE <- as.Date("2012-3-21",  format = "%Y-%m-%d") # Spring Equinox
  SS <- as.Date("2012-6-21",  format = "%Y-%m-%d") # Summer Solstice
  FE <- as.Date("2012-9-21",  format = "%Y-%m-%d") # Fall Equinox
  
  # Convert dates from any year to 2012 dates
  d <- as.Date(strftime(DATES, format="2012-%m-%d"))
  
  ifelse (d >= WS | d < SE, "Winter",
          ifelse (d >= SE & d < SS, "Spring",
                  ifelse (d >= SS & d < FE, "Summer", "Fall")))
}

#### END FUNCTION ####


#### FUNCTION TO CALCULATE AVERAGE OF TOTAL DAYS/YEAR FOR A VARIABLE ####

MeanAnnualTotals = function(DF, VarName){
  Years = length(unique(strftime(DF$Date, "%Y")))
  VarIndex = which(colnames(DF) == VarName)
  MeanAnnualTotal = aggregate(DF[,VarIndex] ~ DF$GCM, FUN=function(x){sum(x)/Years}) 
  names(MeanAnnualTotal) = c("GCM", "MeanAnnualTotals")
  return(MeanAnnualTotal)
}

#### END FUNCTION ####


#### FUNCTION TO CALCULATE AVERAGE OF MAXIMUM ANNUAL DAYS/YEAR FOR A VARIABLE ####
MeanAnnualMax = function(DF, VarName){
  VarIndex = which(colnames(DF) == VarName)
  YearlyMax = aggregate(DF[,VarIndex], by=list(GCM=DF$GCM, Year=DF$Date$year), FUN=max)
  MeanAnnualMax = aggregate(YearlyMax[,3] ~ YearlyMax$GCM, FUN=mean)
  names(MeanAnnualMax) = c("GCM", "MeanAnnualMax")
  return(MeanAnnualMax)
}

#### END FUNCTION ####


#### FUNCTION TO COMPARE BASELINE TO FUTURE MEANS AND CALCULATE DELTAS ####

GetAnnualMeanDeltas = function(BaseMeans, FutureMeans){
  TotalMeans = merge(BaseMeans, FutureMeans, by="GCM")
  TotalMeans$Delta = unlist(FutureMeans[2] - BaseMeans[2])
  return(TotalMeans)
}

#### END FUNCTION ####


#### FUNCTION TO CALCULATE AVERAGE OF MAXIMUM SEASONAL DAYS/YEAR FOR A VARIABLE ####
MeanSeasonalMax = function(DF, VarName){
  VarIndex = which(colnames(DF) == VarName)
  YearlyMax = aggregate(DF[,VarIndex], by=list(GCM=DF$GCM, Year=DF$Date$year, Season=DF$season), FUN=max)
  MeanAnnualMax = aggregate(YearlyMax[,4], by=list(GCM=YearlyMax$GCM, Season=YearlyMax$Season), FUN=mean)
  names(MeanAnnualMax) = c("GCM", "Season", "MeanAnnualMax")
  return(MeanAnnualMax)
}

#### END FUNCTION ####


#### FUNCTION TO COMPARE BASELINE TO FUTURE MEANS AND CALCULATE DELTAS ####

GetSeasonalMeanDeltas = function(BaseMeans, FutureMeans){
  TotalMeans = merge(BaseMeans, FutureMeans, by=c("GCM", "Season"))
  TotalMeans$Delta = unlist(FutureMeans[3] - BaseMeans[3])
  return(TotalMeans)
}

#### END FUNCTION ####

################################ END FUNCTION DEFINITIONS #########################################


################################# SUMMARIZE CHANGE IN FUTURE TEMP/PRECIP MEANS BY GCM ####################

####Set Average values for all four weather variables, using all baseline years and all climate models
BaseMeanPr = mean(Baseline_all$PrecipCustom)
BaseMeanTmx = mean(Baseline_all$TmaxCustom)
BaseMeanTmn = mean(Baseline_all$TminCustom)
# BaseMeanWind = mean(Baseline_all$Wind)

####Create Future/Baseline means data tables, with averages for all four weather variables, organized by GCM
Future_Means = data.frame(aggregate(cbind(Future_all$PrecipCustom, Future_all$TmaxCustom, Future_all$TminCustom)
                                    ~ Future_all$GCM, Future_all, mean)) 
names(Future_Means) = c("GCM", "PrecipCustom", "TmaxCustom", "TminCustom")
Future_Means$TavgCustom = (Future_Means$TmaxCustom + Future_Means$TminCustom)/2

Baseline_Means = data.frame(aggregate(cbind(Baseline_all$PrecipCustom, Baseline_all$TmaxCustom, Baseline_all$TminCustom)
                                      ~ Baseline_all$GCM, Baseline_all, mean))   
names(Baseline_Means) = c("GCM", "PrecipCustom", "TmaxCustom", "TminCustom")  
Baseline_Means$TavgCustom = (Baseline_Means$TmaxCustom + Baseline_Means$TminCustom)/2

#### add delta columns in order to classify CFs
Future_Means$DeltaPr = Future_Means$PrecipCustom - Baseline_Means$PrecipCustom
Future_Means$DeltaTmx = Future_Means$TmaxCustom - Baseline_Means$TmaxCustom
Future_Means$DeltaTmn = Future_Means$TminCustom - Baseline_Means$TminCustom
# Future_Means$DeltaWind = Future_Means$Wind - Baseline_Means$Wind
Future_Means$DeltaTavg = Future_Means$TavgCustom - Baseline_Means$TavgCustom


#### Set limits for CF classification
Pr0 = as.numeric(quantile(Future_Means$DeltaPr, 0))
Pr25 = as.numeric(quantile(Future_Means$DeltaPr, CFLow))
PrAvg = as.numeric(mean(Future_Means$DeltaPr))
Pr75 = as.numeric(quantile(Future_Means$DeltaPr, CFHigh))
Pr100 = as.numeric(quantile(Future_Means$DeltaPr, 1))
Tavg0 = as.numeric(quantile(Future_Means$DeltaTavg, 0))
Tavg25 = as.numeric(quantile(Future_Means$DeltaTavg, CFLow)) 
Tavg = as.numeric(mean(Future_Means$DeltaTavg))
Tavg75 = as.numeric(quantile(Future_Means$DeltaTavg, CFHigh))
Tavg100 = as.numeric(quantile(Future_Means$DeltaTavg, 1))
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
Future_Means$CF[Future_Means$CF1==1]=CFs[1]
Future_Means$CF[Future_Means$CF2==1]=CFs[2]
Future_Means$CF[Future_Means$CF3==1]=CFs[3]
Future_Means$CF[Future_Means$CF4==1]=CFs[4]
Future_Means$CF[Future_Means$CF5==1]=CFs[5]
Future_Means$CF=as.factor(Future_Means$CF)
Future_Means$CF = factor(Future_Means$CF,ordered=TRUE,levels=CFs)

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

####Add column with CF classification to Future_all/Baseline_all
CF_GCM = data.frame(GCM = Future_Means$GCM, CF = Future_Means$CF)
Future_all = merge(Future_all, CF_GCM[1:2], by="GCM")
Baseline_all$CF = "Historical"

################################# END FUTURE MEANS SUMMARY ################################################


################################ SUMMARIZE TEMPERATURE AND PRECIP BY MONTH & SEASON #######################

#Add season variable for all data in Future_all and Baseline_all
Future_all$season=getSeason(Future_all$Date)
Baseline_all$season=getSeason(Baseline_all$Date)

#### Create tables with monthly tmax/tmin delta by CF
Monthly_Tmax_Baseline = with(Baseline_all, tapply(TmaxCustom, list(Date$mon), mean))
Monthly_Tmax_Future = melt(with(Future_all, tapply(TmaxCustom, list(Date$mon, CF), mean, na.rm=TRUE)))
Monthly_Tmax_delta = data.frame(months, CF=Monthly_Tmax_Future$X2, Tmax_delta=Monthly_Tmax_Future$value - rep(Monthly_Tmax_Baseline, length(CFs)))
names(Monthly_Tmax_delta) = c("month","CF","Tmax") 
Monthly_Tmax_delta$CF = factor(Monthly_Tmax_delta$CF, 
                               levels = unique(Monthly_Tmax_delta$CF), 
                               labels=CFs)

Monthly_Tmin_Baseline = with(Baseline_all, tapply(TminCustom, list(Date$mon), mean))
Monthly_Tmin_Future = melt(with(Future_all, tapply(TminCustom, list(Date$mon, CF), mean, na.rm=TRUE)))
Monthly_Tmin_delta = data.frame(months, CF=Monthly_Tmin_Future$X2, Tmin_delta=Monthly_Tmin_Future$value - rep(Monthly_Tmin_Baseline, length(CFs)))
names(Monthly_Tmin_delta) = c("month","CF","Tmin") 
Monthly_Tmin_delta$CF = factor(Monthly_Tmin_delta$CF, 
                               levels = unique(Monthly_Tmin_delta$CF), 
                               labels=CFs)


#### Create table with monthly precip delta by CF
Monthly_Precip_Baseline = with(Baseline_all, tapply(PrecipCustom, list(Date$mon), mean))
Monthly_Precip_Future = melt(with(Future_all, tapply(PrecipCustom, list(Date$mon, CF), mean, na.rm=TRUE)))
Monthly_Precip_delta = data.frame(months, CF=Monthly_Precip_Future$X2, Precip_delta=Monthly_Precip_Future$value - rep(Monthly_Precip_Baseline, length(CFs)))
names(Monthly_Precip_delta) = c("month","CF","Precip")
Monthly_Precip_delta$CF = factor(Monthly_Precip_delta$CF, 
                                 levels = unique(Monthly_Precip_delta$CF), 
                                 labels=CFs)

### Create table with seasonal precip delta by CF
Seasonal_Precip_Baseline = with(Baseline_all, tapply(PrecipCustom, list(season), mean))
Seasonal_Precip_Future = melt(with(Future_all, tapply(PrecipCustom, list(season, CF), mean, na.rm=TRUE)))
Seasonal_Precip_delta = data.frame(seasons, CF=Seasonal_Precip_Future$X2, Precip_delta=Seasonal_Precip_Future$value - rep(Seasonal_Precip_Baseline, length(CFs)))
names(Seasonal_Precip_delta) = c("Season","CF","Precip")
Seasonal_Precip_delta$CF = factor(Seasonal_Precip_delta$CF, 
                                  levels = unique(Seasonal_Precip_delta$CF), 
                                  labels=CFs)

########################################## END MONTH & SEASON SUMMARY ##########################################


######################################## CALCULATE ANNUAL DAYS ABOVE/BELOW TEMP & PRECIP THRESHOLDS ##########################

###### TOTAL & CONSECUTIVE DAYS OVER/UNDER THRESHOLD TEMPS ######

HistTmaxHigh = quantile(Baseline_all$TmaxCustom, QuantileHigh)
HistTminLow = quantile(Baseline_all$TminCustom, QuantileLow)

Baseline_all$TavgCustom = (Baseline_all$TmaxCustom + Baseline_all$TminCustom)/2
Baseline_all$OverHotTemp = Baseline_all$TmaxCustom > HotTemp
Baseline_all$OverHighQ = Baseline_all$TmaxCustom > HistTmaxHigh
Baseline_all$UnderColdTemp = Baseline_all$TminCustom < ColdTemp
Baseline_all$UnderLowQ = Baseline_all$TminCustom < HistTminLow
Baseline_all$HeatConsecutive=(Baseline_all$OverHotTemp)*unlist(lapply(rle(Baseline_all$OverHotTemp)$lengths, seq_len))
Baseline_all$ColdConsecutive=(Baseline_all$UnderColdTemp)*unlist(lapply(rle(Baseline_all$UnderColdTemp)$lengths, seq_len))
Baseline_all$NoPrecip = Baseline_all$PrecipCustom < PrecipThreshold
Baseline_all$DroughtLength = (Baseline_all$NoPrecip)*unlist(lapply(rle(Baseline_all$NoPrecip)$lengths, seq_len)) 

Future_all$TavgCustom = (Future_all$TmaxCustom + Future_all$TminCustom)/2
Future_all$OverHotTemp = Future_all$TmaxCustom > HotTemp
Future_all$OverHighQ = Future_all$TmaxCustom > HistTmaxHigh
Future_all$UnderColdTemp = Future_all$TminCustom < ColdTemp
Future_all$UnderLowQ = Future_all$TminCustom < HistTminLow
Future_all$HeatConsecutive=(Future_all$OverHotTemp)*unlist(lapply(rle(Future_all$OverHotTemp)$lengths, seq_len))
Future_all$ColdConsecutive=(Future_all$UnderColdTemp)*unlist(lapply(rle(Future_all$UnderColdTemp)$lengths, seq_len))
Future_all$NoPrecip = Future_all$PrecipCustom < PrecipThreshold
Future_all$DroughtLength = (Future_all$NoPrecip)*unlist(lapply(rle(Future_all$NoPrecip)$lengths, seq_len))


#### Total Days Over Hot Threshold/Under Cold Threshold #####

#Hot threshold

BaseOverHotTemp = MeanAnnualTotals(Baseline_all, "OverHotTemp")
FutureOverHotTemp = MeanAnnualTotals(Future_all, "OverHotTemp")

BaseOverHotTemp$CF = "Historical"
BaseOverHotTemp$timeframe = "Historical"
Hist_Total_HotTemp_Days = mean(BaseOverHotTemp$MeanAnnualTotals)

FutureOverHotTemp = merge(FutureOverHotTemp, CF_GCM, by="GCM")
FutureOverHotTemp$timeframe = "Future"

TotalOverHotTemp = rbind(BaseOverHotTemp, FutureOverHotTemp)
names(TotalOverHotTemp) = c("GCM", "HotDays", "CF", "timeframe")
TotalOverHotTemp$CF = factor(TotalOverHotTemp$CF, levels = CFs)

#Cold threshold
BaseUnderColdTemp = MeanAnnualTotals(Baseline_all, "UnderColdTemp")
FutureUnderColdTemp = MeanAnnualTotals(Future_all, "UnderColdTemp")

BaseUnderColdTemp$CF = "Historical"
BaseUnderColdTemp$timeframe = "Historical"
Hist_Total_ColdTemp_Days = mean(BaseUnderColdTemp$MeanAnnualTotals)

FutureUnderColdTemp = merge(FutureUnderColdTemp, CF_GCM, by="GCM")
FutureUnderColdTemp$timeframe = "Future"

TotalUnderColdTemp = rbind(BaseUnderColdTemp, FutureUnderColdTemp)
names(TotalUnderColdTemp) = c("GCM", "ColdDays", "CF", "timeframe")
TotalUnderColdTemp$CF = factor(TotalUnderColdTemp$CF, levels = CFs)

#############################################################

#### Total Days Over High Percentile Tmax/Under Low Percentile Tmin ####

#Over 95th
BaseOverHighQ = MeanAnnualTotals(Baseline_all, "OverHighQ")
FutureOverHighQ = MeanAnnualTotals(Future_all, "OverHighQ")

BaseOverHighQ$CF = "Historical"
BaseOverHighQ$timeframe = "Historical"
Hist_Total_HighQ_Days = mean(BaseOverHighQ$MeanAnnualTotals)

FutureOverHighQ = merge(FutureOverHighQ, CF_GCM, by="GCM")
FutureOverHighQ$timeframe = "Future"

TotalOverHighQ = rbind(BaseOverHighQ, FutureOverHighQ)
names(TotalOverHighQ) = c("GCM", "HighQDays", "CF", "timeframe")
TotalOverHighQ$CF = factor(TotalOverHighQ$CF, levels = CFs)

#Under 5th
BaseUnderLowQ = MeanAnnualTotals(Baseline_all, "UnderLowQ")
FutureUnderLowQ = MeanAnnualTotals(Future_all, "UnderLowQ")

BaseUnderLowQ$CF = "Historical"
BaseUnderLowQ$timeframe = "Historical"
Hist_Total_LowQ_Days = mean(BaseUnderLowQ$MeanAnnualTotals)

FutureUnderLowQ = merge(FutureUnderLowQ, CF_GCM, by="GCM")
FutureUnderLowQ$timeframe = "Future"

TotalUnderLowQ = rbind(BaseUnderLowQ, FutureUnderLowQ)
names(TotalUnderLowQ) = c("GCM", "LowQDays", "CF", "timeframe")
TotalUnderLowQ$CF = factor(TotalUnderLowQ$CF, levels = CFs)

########################################################################

#### Maximum consecutive days over hot threshold ####
BaseHeatConsecutive = MeanAnnualMax(Baseline_all, "HeatConsecutive")
FutureHeatConsecutive = MeanAnnualMax(Future_all, "HeatConsecutive")

BaseHeatConsecutive$CF = "Historical"
BaseHeatConsecutive$timeframe = "Historical"
Hist_Cons_HotTemp_Days = mean(BaseHeatConsecutive$MeanAnnualMax)

FutureHeatConsecutive = merge(FutureHeatConsecutive, CF_GCM, by="GCM")
FutureHeatConsecutive$timeframe = "Future"

TotalHeatConsecutive = rbind(BaseHeatConsecutive, FutureHeatConsecutive)
names(TotalHeatConsecutive) = c("GCM", "ConsHotDays", "CF", "timeframe")
TotalHeatConsecutive$CF = factor(TotalHeatConsecutive$CF, levels = CFs)

#####################################################

#### Maximum consecutive days under cold threshold ####

BaseColdConsecutive = MeanAnnualMax(Baseline_all, "ColdConsecutive")
FutureColdConsecutive = MeanAnnualMax(Future_all, "ColdConsecutive")

BaseColdConsecutive$CF = "Historical"
BaseColdConsecutive$timeframe = "Historical"
Hist_Cons_ColdTemp_Days = mean(BaseColdConsecutive$MeanAnnualMax)

FutureColdConsecutive = merge(FutureColdConsecutive, CF_GCM, by="GCM")
FutureColdConsecutive$timeframe = "Future"

TotalColdConsecutive = rbind(BaseColdConsecutive, FutureColdConsecutive)
names(TotalColdConsecutive) = c("GCM", "ConsColdDays", "CF", "timeframe")
TotalColdConsecutive$CF = factor(TotalColdConsecutive$CF, levels = CFs)

#######################################################

#### Average annual drought length ####

BaseDroughtLength = MeanAnnualMax(Baseline_all, "DroughtLength")
FutureDroughtLength = MeanAnnualMax(Future_all, "DroughtLength")

BaseDroughtLength$CF = "Historical"
BaseDroughtLength$timeframe = "Historical"
Hist_DroughtLength = mean(BaseDroughtLength$MeanAnnualMax)

FutureDroughtLength = merge(FutureDroughtLength, CF_GCM, by="GCM")
FutureDroughtLength$timeframe = "Future"

DroughtLength = rbind(BaseDroughtLength, FutureDroughtLength)
names(DroughtLength) = c("GCM", "DroughtLength", "CF", "timeframe")
DroughtLength$CF = factor(DroughtLength$CF, levels = CFs)

#######################################################

#### Average seasonal drougth length

BaseSeasonalDroughtLength = MeanSeasonalMax(Baseline_all, "DroughtLength")
FutureSeasonalDroughtLength = MeanSeasonalMax(Future_all, "DroughtLength")

BaseSeasonalDroughtLength$CF = "Historical"
BaseSeasonalDroughtLength$timeframe = "Historical"
Hist_SeasonalDrought = aggregate(MeanAnnualMax ~ Season, data=BaseSeasonalDroughtLength, FUN=mean)
names(Hist_SeasonalDrought) = c("Season", "DroughtLength")

FutureSeasonalDroughtLength = merge(FutureSeasonalDroughtLength, CF_GCM, by="GCM")
FutureSeasonalDroughtLength$timeframe = "Future"

SeasonalDroughtLength = rbind(BaseSeasonalDroughtLength, FutureSeasonalDroughtLength)
names(SeasonalDroughtLength) = c("GCM", "Season", "DroughtLength", "CF", "timeframe")
SeasonalDroughtLength$CF = factor(SeasonalDroughtLength$CF, levels = CFs)

#######################################################

#### Annual maximum 1-day precipitation #####

BasePrecipMax = MeanAnnualMax(Baseline_all, "PrecipCustom")
FuturePrecipMax = MeanAnnualMax(Future_all, "PrecipCustom")

BasePrecipMax$CF = "Historical"
BasePrecipMax$timeframe = "Historical"
Hist_Precip_Max = mean(BasePrecipMax$MeanAnnualMax)

FuturePrecipMax = merge(FuturePrecipMax, CF_GCM, by="GCM")
FuturePrecipMax$timeframe = "Future"

PrecipMax = rbind(BasePrecipMax, FuturePrecipMax)
names(PrecipMax) = c("GCM", "PrecipMax", "CF", "timeframe")
PrecipMax$CF = factor(PrecipMax$CF, levels = CFs)

##############################################

######################################## END TEMP & PRECIP THRESHOLD CALCULATIONS ##############################


#### Create .xslx workbook with all data tables
setwd(paste(OutDir, "Figs MACA", sep="/"))
WriteXLS(c("Monthly_Precip_delta", "Monthly_Tmax_delta", "Monthly_Tmin_delta", "PrecipMax", "TotalHeatConsecutive", "TotalColdConsecutive", "DroughtLength", "TotalOverHotTemp", "TotalOverHighQ", "TotalUnderColdTemp", "TotalUnderLowQ", "Future_Means"), 
         sprintf("%s_%s_%s_CCSP_Plot_data.xlsx", SiteID, Lat, Lon), BoldHeaderRow = TRUE)

##### Save Current workspace environment
save.image(sprintf("%s_%s_%s_Final_Environment.RData",SiteID, Lat, Lon))

#  EOF
