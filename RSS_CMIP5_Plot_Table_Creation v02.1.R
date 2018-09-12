# RSS_Plot_Table_Creation vxx.R
# v02.1 - added Future_Means to xlsx output.
# v02 STABLE - 22 Oct 2015 - days below ColdTemp added and debugged.

################################################### 'GET SEASON' FUNCTION ########################################

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
###################################################################################################################

#Add season variable for all data in Future_all and Baseline_all
Future_all$season=getSeason(Future_all$Date)
Baseline_all$season=getSeason(Baseline_all$Date)
Historical_all$season=getSeason(Historical_all$Date)

#### Create tables with monthly tmax/tmin delta by CF
Monthly_Tmax_delta = data.frame(months,Tmax_delta=with(Future_all, tapply(TmaxCustom, list(Date$mon, CF), mean))-
                                  with(Baseline_all, tapply(TmaxCustom, list(Date$mon, CF), mean)))
Monthly_Tmax_delta = melt(Monthly_Tmax_delta, id="months")
names(Monthly_Tmax_delta) = c("month","CF","Tmax") 
Monthly_Tmax_delta$CF = factor(Monthly_Tmax_delta$CF, 
                               levels = unique(Monthly_Tmax_delta$CF), 
                               labels=CFs)

Monthly_Tmin_delta = data.frame(months,Tmin_delta=with(Future_all, tapply(TminCustom, list(Date$mon, CF), mean))-
                                  with(Baseline_all, tapply(TminCustom, list(Date$mon, CF), mean)))
Monthly_Tmin_delta = melt(Monthly_Tmin_delta, id="months")
names(Monthly_Tmin_delta) = c("month","CF","Tmin") 
Monthly_Tmin_delta$CF = factor(Monthly_Tmin_delta$CF, 
                               levels = unique(Monthly_Tmin_delta$CF), 
                               labels=CFs)


#### Create table with monthly precip delta by CF
Monthly_Precip_delta = data.frame(months,Precip_delta=with(Future_all, tapply(PrecipCustom, list(Date$mon, CF), mean))-
                                    with(Baseline_all, tapply(PrecipCustom, list(Date$mon, CF), mean)))
Monthly_Precip_delta = melt(Monthly_Precip_delta, id="months")
names(Monthly_Precip_delta) = c("month","CF","Precip")
Monthly_Precip_delta$CF = factor(Monthly_Precip_delta$CF, 
                                 levels = unique(Monthly_Precip_delta$CF), 
                                 labels=CFs)

### Create table with seasonal precip delta by CF
Seasonal_Precip_delta = data.frame(Precip_delta=with(Future_all, tapply(PrecipCustom, list(season, CF), mean))-
                                    with(Baseline_all, tapply(PrecipCustom, list(season, CF), mean)))
Seasonal_Precip_delta = Seasonal_Precip_delta[match(seasons, row.names(Seasonal_Precip_delta)),]
Seasonal_Precip_delta$season = seasons
Seasonal_Precip_delta = melt(Seasonal_Precip_delta, id="season")
names(Seasonal_Precip_delta) = c("Season","CF","Precip")
Seasonal_Precip_delta$CF = factor(Seasonal_Precip_delta$CF, 
                                 levels = unique(Seasonal_Precip_delta$CF), 
                                 labels=CFs)


###### TOTAL & CONSECUTIVE DAYS OVER/UNDER THRESHOLD TEMPS ######

HistTmax95 = quantile(Historical_all$TmaxCustom, .95)
HistTmin05 = quantile(Historical_all$TminCustom, .05)

Baseline_all$OverHotTemp = Baseline_all$TmaxCustom > HotTemp
Baseline_all$Over95th = Baseline_all$TmaxCustom > HistTmax95
Baseline_all$UnderColdTemp = Baseline_all$TminCustom < ColdTemp
Baseline_all$Under5th = Baseline_all$TminCustom < HistTmin05
Baseline_all$HeatConsecutive=(Baseline_all$OverHotTemp)*unlist(lapply(rle(Baseline_all$OverHotTemp)$lengths, seq_len))
Baseline_all$ColdConsecutive=(Baseline_all$UnderColdTemp)*unlist(lapply(rle(Baseline_all$UnderColdTemp)$lengths, seq_len))
Baseline_all$NoPrecip = Baseline_all$PrecipCustom < PrecipThreshold
Baseline_all$DroughtLength = (Baseline_all$NoPrecip)*unlist(lapply(rle(Baseline_all$NoPrecip)$lengths, seq_len)) 

Future_all$OverHotTemp = Future_all$TmaxCustom > HotTemp
Future_all$Over95th = Future_all$TmaxCustom > HistTmax95
Future_all$UnderColdTemp = Future_all$TminCustom < ColdTemp
Future_all$Under5th = Future_all$TminCustom < HistTmin05
Future_all$HeatConsecutive=(Future_all$OverHotTemp)*unlist(lapply(rle(Future_all$OverHotTemp)$lengths, seq_len))
Future_all$ColdConsecutive=(Future_all$UnderColdTemp)*unlist(lapply(rle(Future_all$UnderColdTemp)$lengths, seq_len))
Future_all$NoPrecip = Future_all$PrecipCustom < PrecipThreshold
Future_all$DroughtLength = (Future_all$NoPrecip)*unlist(lapply(rle(Future_all$NoPrecip)$lengths, seq_len))


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

#### Total Days Over Hot Threshold/Under Cold Threshold #####

#Hot days
Hist_Mean_HD = sum(Historical_all$TmaxCustom > HotTemp)/HistYears

BaseOverHotTemp = MeanAnnualTotals(Baseline_all, "OverHotTemp")
FutureOverHotTemp = MeanAnnualTotals(Future_all, "OverHotTemp")

TotalOverHotTemp = GetAnnualMeanDeltas(BaseOverHotTemp, FutureOverHotTemp)
names(TotalOverHotTemp) = c("GCM", "BaseOverHotTemp", "FutureOverHotTemp", "DeltaOverHotTemp")

#Cold days
Hist_Mean_CD = sum(Historical_all$TminCustom < ColdTemp)/HistYears

BaseUnderColdTemp = MeanAnnualTotals(Baseline_all, "UnderColdTemp")
FutureUnderColdTemp = MeanAnnualTotals(Future_all, "UnderColdTemp")

TotalUnderColdTemp = GetAnnualMeanDeltas(BaseUnderColdTemp, FutureUnderColdTemp)
names(TotalUnderColdTemp) = c("GCM", "BaseUnderColdTemp", "FutureUnderColdTemp", "DeltaUnderColdTemp")

#############################################################

#### Total Days Over 95th percentile Tmax/Under 5th percentile Tmin ####
 
#Over 95th
BaseOver95th = MeanAnnualTotals(Baseline_all, "Over95th")
FutureOver95th = MeanAnnualTotals(Future_all, "Over95th")

TotalOver95th = GetAnnualMeanDeltas(BaseOver95th, FutureOver95th)
names(TotalOver95th) = c("GCM", "BaseOver95th", "FutureOver95th", "DeltaOver95th")

#Under 5th
BaseUnder5th = MeanAnnualTotals(Baseline_all, "Under5th")
FutureUnder5th = MeanAnnualTotals(Future_all, "Under5th")

TotalUnder5th = GetAnnualMeanDeltas(BaseUnder5th, FutureUnder5th)
names(TotalUnder5th) = c("GCM", "BaseUnder5th", "FutureUnder5th", "DeltaUnder5th")

########################################################################


#### Maximum consecutive days over hot threshold ####

BaseHeatConsecutive = MeanAnnualMax(Baseline_all, "HeatConsecutive")
FutureHeatConsecutive = MeanAnnualMax(Future_all, "HeatConsecutive")

TotalHeatConsecutive = GetAnnualMeanDeltas(BaseHeatConsecutive, FutureHeatConsecutive)
names(TotalHeatConsecutive) = c("GCM", "BaseHeatConsecutive", "FutureHeatConsecutive", "DeltaHeatConsecutive")


#####################################################


#### Maximum consecutive days under cold threshold ####

BaseColdConsecutive = MeanAnnualMax(Baseline_all, "ColdConsecutive")
FutureColdConsecutive = MeanAnnualMax(Future_all, "ColdConsecutive")

TotalColdConsecutive = GetAnnualMeanDeltas(BaseColdConsecutive, FutureColdConsecutive)
names(TotalColdConsecutive) = c("GCM", "BaseColdConsecutive", "FutureColdConsecutive", "DeltaColdConsecutive")

#######################################################


#### Average annual drought length ####

BaseDroughtLength = MeanAnnualMax(Baseline_all, "DroughtLength")
FutureDroughtLength = MeanAnnualMax(Future_all, "DroughtLength")

TotalDroughtLength = GetAnnualMeanDeltas(BaseDroughtLength, FutureDroughtLength)
names(TotalDroughtLength) = c("GCM", "BaseDroughtLength", "FutureDroughtLength", "DeltaDroughtLength")

#######################################################


#### Average seasonal drougth length

BaseSeasonalDroughtLength = MeanSeasonalMax(Baseline_all, "DroughtLength")
FutureSeasonalDroughtLength = MeanSeasonalMax(Future_all, "DroughtLength")

TotalSeasonalDroughtLength = GetSeasonalMeanDeltas(BaseSeasonalDroughtLength, FutureSeasonalDroughtLength)
names(TotalSeasonalDroughtLength) = c("GCM", "Season", "BaseSeasonalDroughtLength", "FutureSeasonalDroughtLength", "DeltaSeasonalDroughtLength")


#######################################################


#### Annual maximum 1-day precipitation #####

BasePrecipMax = MeanAnnualMax(Baseline_all, "PrecipCustom")
FuturePrecipMax = MeanAnnualMax(Future_all, "PrecipCustom")

TotalPrecipMax = GetAnnualMeanDeltas(BasePrecipMax, FuturePrecipMax)
names(TotalPrecipMax) = c("GCM", "BasePrecipMax", "FuturePrecipMax", "DeltaPrecipMax")

#######################################


#### Create .xslx workbook with all data tables
setwd(WD_plots)
WriteXLS(c("Monthly_Precip_delta", "Monthly_Tmax_delta", "Monthly_Tmin_delta", "PrecipMax", "HeatMax", "ColdMax", "DroughtMax", "TotalOverHotTemp", "TotalUnderColdTemp", "Future_Means"), 
         sprintf("%s_%s_%s_CCSP_Plot_data.xlsx", SiteID, Lat, Lon), BoldHeaderRow = TRUE)

##### Save Current workspace environment
save.image(sprintf("%s_%s_%s_Final_Environment.RData",SiteID, Lat, Lon))

#  EOF