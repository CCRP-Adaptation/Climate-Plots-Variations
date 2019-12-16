# RSS_Plot_Table_Creation vxx.R
# v02.1 - added Future_Means to xlsx output.
# v02 STABLE - 22 Oct 2015 - days below ColdTemp added and debugged.

# AC - 07/03/2018 - Altered delta formulas to calculate difference from a standard averaged historical value, rather than by comparing CFs from Baseline_all data frame. 
#   This is needed to make script compatible with MACA 

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
                                  rep(with(Historical_all, tapply(TmaxCustom, list(Date$mon, CF), mean)), length(CFs)))
Monthly_Tmax_delta = melt(Monthly_Tmax_delta, id="months")
names(Monthly_Tmax_delta) = c("month","CF","Tmax") 
Monthly_Tmax_delta$CF = factor(Monthly_Tmax_delta$CF, 
                               levels = unique(Monthly_Tmax_delta$CF), 
                               labels=CFs)

Monthly_Tmin_delta = data.frame(months,Tmin_delta=with(Future_all, tapply(TminCustom, list(Date$mon, CF), mean))-
                                  rep(with(Historical_all, tapply(TminCustom, list(Date$mon, CF), mean)), length(CFs)))
Monthly_Tmin_delta = melt(Monthly_Tmin_delta, id="months")
names(Monthly_Tmin_delta) = c("month","CF","Tmin") 
Monthly_Tmin_delta$CF = factor(Monthly_Tmin_delta$CF, 
                               levels = unique(Monthly_Tmin_delta$CF), 
                               labels=CFs)


#### Create table with monthly precip delta by CF
Monthly_Precip_delta = data.frame(months,Precip_delta=with(Future_all, tapply(PrecipCustom, list(Date$mon, CF), mean))-
                                    rep(with(Baseline_all, tapply(PrecipCustom, list(Date$mon, CF), mean)), length(CFs)))
Monthly_Precip_delta = melt(Monthly_Precip_delta, id="months")
names(Monthly_Precip_delta) = c("month","CF","Precip")
Monthly_Precip_delta$CF = factor(Monthly_Precip_delta$CF, 
                                 levels = unique(Monthly_Precip_delta$CF), 
                                 labels=CFs)

### Create table with seasonal precip delta by CF
Seasonal_Precip_delta = data.frame(Precip_delta=with(Future_all, tapply(PrecipCustom, list(season, CF), mean))-
                                    rep(with(Baseline_all, tapply(PrecipCustom, list(season, CF), mean)), length(CFs)))
Seasonal_Precip_delta = Seasonal_Precip_delta[match(seasons, row.names(Seasonal_Precip_delta)),]
Seasonal_Precip_delta$season = seasons
Seasonal_Precip_delta = melt(Seasonal_Precip_delta, id="season")
names(Seasonal_Precip_delta) = c("Season","CF","Precip")
Seasonal_Precip_delta$CF = factor(Seasonal_Precip_delta$CF, 
                                 levels = unique(Seasonal_Precip_delta$CF), 
                                 labels=CFs)


###### TOTAL & CONSECUTIVE DAYS OVER/UNDER THRESHOLD TEMPs ######

HistYears = Hist_EndYear - Hist_StartYear + 1

#### Historical
Historical_all$Over_HotTemp = Historical_all$TmaxCustom > HotTemp
Hist_Total_HotTemp_Days = as.numeric(sum(Historical_all$Over_HotTemp)/HistYears)

Historical_all$HeatConsecutive=(Historical_all$Over_HotTemp)*unlist(lapply(rle(Historical_all$Over_HotTemp)$lengths, seq_len))
HeatMaxYearly_Hist = data.frame(HeatMax=tapply(Historical_all$HeatConsecutive, Historical_all$Date$year, max))
HeatMaxHist = as.numeric(mean(HeatMaxYearly_Hist$HeatMax))

Historical_all$Under_ColdTemp = Historical_all$TminCustom < ColdTemp
Hist_Total_ColdTemp_Days = as.numeric(sum(Historical_all$Under_ColdTemp)/HistYears)

Historical_all$ColdConsecutive=(Historical_all$Under_ColdTemp)*unlist(lapply(rle(Historical_all$Under_ColdTemp)$lengths, seq_len))
ColdMaxYearly_Hist = data.frame(ColdMax=tapply(Historical_all$ColdConsecutive, Historical_all$Date$year, max))
ColdMaxHist = as.numeric(mean(ColdMaxYearly_Hist$ColdMax))

HistTmax95 = quantile(Historical_all$TmaxCustom, .95)
Hist_Total_95th_Days = (.05*nrow(Historical_all))/HistYears

HistTmin05 = quantile(Historical_all$TminCustom, .05)
Hist_Total_5th_Days = (.05*nrow(Historical_all))/HistYears

#### Baseline

### Hot days 
## HotTemp threshold 
Baseline_all$OverHotTemp=Baseline_all$TmaxCustom > HotTemp

BaseOverHotTemp = data.frame(aggregate(Baseline_all$OverHotTemp ~ Baseline_all$GCM,Future_all,sum))
names(BaseOverHotTemp) = c("GCM", "Total_HotTemp_Days")
BaseOverHotTemp$CF= "Historical"
BaseOverHotTemp$timeframe = "Historical"

BaseOverHotTemp$OverHotTempComp = ifelse(BaseOverHotTemp$Total_HotTemp_Days != 0, (Hist_Total_HotTemp_Days)/(BaseOverHotTemp$Total_HotTemp_Days), 1)
BaseOverHotTemp$Adjusted = ifelse((BaseOverHotTemp$OverHotTempComp == 0), Hist_Total_HotTemp_Days, (BaseOverHotTemp$Total_HotTemp_Days * BaseOverHotTemp$OverHotTempComp))

##95th percentile temp.
Baseline_all$Over95th=Baseline_all$TmaxCustom > HistTmax95

BaseOver95th = data.frame(aggregate(Baseline_all$Over95th ~ Baseline_all$GCM,Future_all,sum))
names(BaseOver95th) = c("GCM", "Total_Over95th_Days")
BaseOver95th$CF = "Historical"
BaseOver95th$timeframe = "Historical"

BaseOver95th$Over95thComp = ifelse(BaseOver95th$Total_Over95th_Days != 0, (Hist_Total_95th_Days)/(BaseOver95th$Total_Over95th_Days), 1)
BaseOver95th$Adjusted = ifelse((BaseOver95th$Over95thComp == 0), Hist_Total_95th_Days, (BaseOver95th$Total_Over95th_Days * BaseOver95th$Over95thComp))

### Cold days 
## ColdTemp threshold 
Baseline_all$UnderColdTemp=Baseline_all$TminCustom < ColdTemp

BaseUnderColdTemp = data.frame(aggregate(Baseline_all$UnderColdTemp ~ Baseline_all$GCM,Future_all,sum))  # sum by GCM
names(BaseUnderColdTemp) = c("GCM", "Total_ColdTemp_Days")
BaseUnderColdTemp$CF = "Historical"
BaseUnderColdTemp$timeframe = "Historical"

BaseUnderColdTemp$UnderColdTempComp = ifelse(BaseUnderColdTemp$Total_ColdTemp_Days != 0, 
                                             (Hist_Total_ColdTemp_Days)/(BaseUnderColdTemp$Total_ColdTemp_Days), 1)
BaseUnderColdTemp$Adjusted = ifelse((BaseUnderColdTemp$UnderColdTempComp == 0), 
                                    Hist_Total_ColdTemp_Days, (BaseUnderColdTemp$Total_ColdTemp_Days * BaseUnderColdTemp$UnderColdTempComp))

##5th percentile temp.
Baseline_all$Under5th=Baseline_all$TminCustom < HistTmin05

BaseUnder5th = data.frame(aggregate(Baseline_all$Under5th ~ Baseline_all$GCM,Future_all,sum))
names(BaseUnder5th) = c("GCM", "Total_Under5th_Days")
BaseUnder5th$CF = "Historical"
BaseUnder5th$timeframe = "Historical"

BaseUnder5th$Under5thComp = ifelse(BaseUnder5th$Total_Under5th_Days != 0, (Hist_Total_5th_Days)/(BaseUnder5th$Total_Under5th_Days), 1)
BaseUnder5th$Adjusted = ifelse((BaseUnder5th$Under5thComp == 0), Hist_Total_5th_Days, (BaseUnder5th$Total_Under5th_Days * BaseUnder5th$Under5thComp))


#### Future

### Hot days
## HotTemp threshold
Future_all$OverHotTemp=Future_all$TmaxCustom > HotTemp

FutureOverHotTemp = data.frame(aggregate(Future_all$OverHotTemp ~ Future_all$GCM,Future_all,sum))
names(FutureOverHotTemp) = c("GCM", "Total_HotTemp_Days")
FutureOverHotTemp = merge(FutureOverHotTemp, CF_GCM, by ="GCM")
FutureOverHotTemp$timeframe = "Future"

FutureOverHotTemp$OverHotTempComp = ifelse(FutureOverHotTemp$Total_HotTemp_Days != 0, (Hist_Total_HotTemp_Days)/(FutureOverHotTemp$Total_HotTemp_Days), 1)
FutureOverHotTemp$Adjusted = ifelse((BaseOverHotTemp$OverHotTempComp == 0), Hist_Total_HotTemp_Days, (FutureOverHotTemp$Total_HotTemp_Days * BaseOverHotTemp$OverHotTempComp))

TotalOverHotTemp = data.frame(rbind(FutureOverHotTemp, BaseOverHotTemp))

## Historic 95th percentile 
Future_all$Over95th=Future_all$TmaxCustom > HistTmax95

FutureOver95th = data.frame(aggregate(Future_all$Over95th ~ Future_all$GCM,Future_all,sum))
names(FutureOver95th) = c("GCM", "Total_Over95th_Days")
FutureOver95th = merge(FutureOver95th, CF_GCM, by ="GCM")
FutureOver95th$timeframe = "Future"

FutureOver95th$Over95thComp = ifelse(FutureOver95th$Total_Over95th_Days != 0, (Hist_Total_95th_Days)/(FutureOver95th$Total_Over95th_Days), 1)
FutureOver95th$Adjusted = ifelse((BaseOver95th$Over95thComp == 0), Hist_Total_95th_Days, (FutureOver95th$Total_Over95th_Days * BaseOver95th$Over95thComp))

TotalOver95th = data.frame(rbind(FutureOver95th, BaseOver95th))

### Cold days
## ColdTemp threshold
Future_all$UnderColdTemp=Future_all$TminCustom < ColdTemp

FutureUnderColdTemp = data.frame(aggregate(Future_all$UnderColdTemp ~ Future_all$GCM,Future_all,sum))  # sum by GCM
names(FutureUnderColdTemp) = c("GCM", "Total_ColdTemp_Days")
FutureUnderColdTemp = merge(FutureUnderColdTemp, CF_GCM, by ="GCM")
FutureUnderColdTemp$timeframe = "Future"

FutureUnderColdTemp$UnderColdTempComp = ifelse(FutureUnderColdTemp$Total_ColdTemp_Days != 0, 
                                               (Hist_Total_ColdTemp_Days)/(FutureUnderColdTemp$Total_ColdTemp_Days), 1)
FutureUnderColdTemp$Adjusted = ifelse((BaseUnderColdTemp$UnderColdTempComp == 0), 
                                      Hist_Total_ColdTemp_Days, (FutureUnderColdTemp$Total_ColdTemp_Days * BaseUnderColdTemp$UnderColdTempComp))

TotalUnderColdTemp = data.frame(rbind(FutureUnderColdTemp, BaseUnderColdTemp))

## Historic 5th percentile
Future_all$Under5th=Future_all$TminCustom < HistTmin05

FutureUnder5th = data.frame(aggregate(Future_all$Under5th ~ Future_all$GCM,Future_all,sum))  # sum by GCM
names(FutureUnder5th) = c("GCM", "Total_Under5th_Days")
FutureUnder5th = merge(FutureUnder5th, CF_GCM, by ="GCM")
FutureUnder5th$timeframe = "Future"

FutureUnder5th$Under5thComp = ifelse(FutureUnder5th$Total_Under5th_Days != 0, 
                                               (Hist_Total_5th_Days)/(FutureUnder5th$Total_Under5th_Days), 1)
FutureUnder5th$Adjusted = ifelse((BaseUnder5th$Under5thComp == 0), 
                                      Hist_Total_5th_Days, (FutureUnder5th$Total_Under5th_Days * BaseUnder5th$Under5thComp))

TotalUnder5th = data.frame(rbind(FutureUnder5th, BaseUnder5th))


#Baseline max consecutive days over HotTemp per year by GCM and CF in 1950-1999 (HistYears = 1950 thru 1999 inclusive = 50)
Baseline_all$HeatConsecutive=(Baseline_all$OverHotTemp)*unlist(lapply(rle(Baseline_all$OverHotTemp)$lengths, seq_len))
HeatMax_baseline=data.frame(with(Baseline_all, tapply(HeatConsecutive, list(GCM, Date$year), max)))
HeatMax_baseline=data.frame(CF="Historical", HeatMaxDays=rowMeans(HeatMax_baseline)) #max values averaged over 30 years for each GCM
HeatMax_baseline$CF = factor(HeatMax_baseline$CF, ordered=TRUE, 
                                 levels=unique(HeatMax_baseline$CF)) #correctly order the CFs
HeatMax_baseline$timeframe = "Historical"

HeatMax_baseline$HeatConsecutiveComp = ifelse(HeatMax_baseline$HeatMaxDays != 0, (HeatMaxHist)/(HeatMax_baseline$HeatMaxDays), 1)
HeatMax_baseline$Adjusted = ifelse((HeatMax_baseline$HeatConsecutiveComp == 0), HeatMaxHist, (HeatMax_baseline$HeatMaxDays * HeatMax_baseline$HeatConsecutiveComp))

       # Historical Cold days
Baseline_all$ColdConsecutive=(Baseline_all$UnderColdTemp)*unlist(lapply(rle(Baseline_all$UnderColdTemp)$lengths, seq_len))
ColdMax_baseline=data.frame(with(Baseline_all, tapply(ColdConsecutive, list(GCM, Date$year), max)))
ColdMax_baseline=data.frame(CF="Historical", ColdMaxDays=rowMeans(ColdMax_baseline)) #max values averaged over 30 years for each GCM
ColdMax_baseline$CF = factor(ColdMax_baseline$CF, ordered=TRUE, 
                             levels=unique(ColdMax_baseline$CF)) #correctly order the CFs
ColdMax_baseline$timeframe = "Historical"

ColdMax_baseline$ColdConsecutiveComp = ifelse(ColdMax_baseline$ColdMaxDays != 0, (ColdMaxHist)/(ColdMax_baseline$ColdMaxDays), 1)
ColdMax_baseline$Adjusted = ifelse((ColdMax_baseline$ColdConsecutiveComp == 0), ColdMaxHist, (ColdMax_baseline$ColdMaxDays * ColdMax_baseline$ColdConsecutiveComp))


#Future max consecutive days over HotTempF per year by GCM and CF in 2040 
Future_all$HeatConsecutive=(Future_all$OverHotTemp)*unlist(lapply(rle(Future_all$OverHotTemp)$lengths, seq_len))
HeatMax_future=data.frame(with(Future_all, tapply(HeatConsecutive, list(GCM, Date$year), max)))
HeatMax_future=data.frame(CF=CF_GCM$CF, HeatMaxDays=rowMeans(HeatMax_future)) #max values averaged over 30 years for each GCM
HeatMax_future$CF = factor(HeatMax_future$CF, ordered=TRUE, 
                               levels=unique(HeatMax_future$CF)) #correctly order the CFs
HeatMax_future$timeframe = "Future"

   # Future max consecutive cold days by GCM & CF
Future_all$ColdConsecutive=(Future_all$UnderColdTemp)*unlist(lapply(rle(Future_all$UnderColdTemp)$lengths, seq_len))
ColdMax_future=data.frame(with(Future_all, tapply(ColdConsecutive, list(GCM, Date$year), max)))
ColdMax_future=data.frame(CF=CF_GCM$CF, ColdMaxDays=rowMeans(ColdMax_future)) #max values averaged over 30 years for each GCM
ColdMax_future$CF = factor(ColdMax_future$CF, ordered=TRUE, 
                           levels=unique(ColdMax_future$CF)) #correctly order the CFs
ColdMax_future$timeframe = "Future"

#max consecutive HotTemp days
HeatMax_delta = HeatMax_future
HeatMax_delta$HeatMaxDays = HeatMax_delta$HeatMaxDays - HeatMax_baseline$HeatMaxDays

HeatMax_future$HeatConsecutiveComp = ifelse(HeatMax_baseline$HeatMaxDays != 0, (HeatMaxHist)/(HeatMax_baseline$HeatMaxDays), 1)
HeatMax_future$Adjusted = ifelse((HeatMax_future$HeatConsecutiveComp == 0), HeatMax_future$HeatMaxDays, (HeatMax_future$HeatMaxDays * HeatMax_baseline$HeatConsecutiveComp))

    # max consec cold days
ColdMax_delta = ColdMax_future
ColdMax_delta$ColdMaxDays = ColdMax_delta$ColdMaxDays - ColdMax_baseline$ColdMaxDays

ColdMax_future$ColdConsecutiveComp = ifelse(ColdMax_baseline$ColdMaxDays != 0, (ColdMaxHist)/(ColdMax_baseline$ColdMaxDays), 1)
ColdMax_future$Adjusted = ifelse((ColdMax_future$ColdConsecutiveComp == 0), ColdMax_future$ColdMaxDays, (ColdMax_future$ColdMaxDays * ColdMax_baseline$ColdConsecutiveComp))


#Combine baseline and future heat wave (consec days) dfs for boxplotting
HeatMax=data.frame(rbind(HeatMax_baseline,HeatMax_future))
ColdMax=data.frame(rbind(ColdMax_baseline,ColdMax_future))

####Remove extra data frames
rm(HeatMax_baseline, HeatMax_delta, HeatMax_future)
rm(ColdMax_baseline, ColdMax_delta, ColdMax_future)
rm(FutureOverHotTemp, BaseOverHotTemp, FutureUnderColdTemp, BaseUnderColdTemp)

###########Drought duration (aggregated over whole year)

####### HISTORICAL DROUGHT LENGTH ########

### Average Yearly Historical Drought Length
Historical_all$NoPrecip = Historical_all$PrecipCustom == 0
Historical_all$DroughtConsecutive=(Historical_all$NoPrecip)*unlist(lapply(rle(Historical_all$NoPrecip)$lengths, seq_len)) 
DroughtYearly_Hist = data.frame(MaxDroughtLength=tapply(Historical_all$DroughtConsecutive, Historical_all$Date$year, max))
DroughtMax_Hist = as.numeric(mean(DroughtYearly_Hist$MaxDroughtLength))

### Average Seasonal Historical Drought Length

DroughtSeasons_Hist=data.frame((with(Historical_all, tapply(DroughtConsecutive, list(Date$year, season), max))))
DroughtMaxSeasons_Hist = data.frame(Winter = mean(DroughtSeasons_Hist$Winter), 
                                    Spring = mean(DroughtSeasons_Hist$Spring), 
                                    Summer = mean(DroughtSeasons_Hist$Summer), 
                                    Fall = mean(DroughtSeasons_Hist$Fall))

DroughtMaxSeasons_Hist_melt = melt(DroughtMaxSeasons_Hist, id=NULL)
DroughtMaxSeasons_Hist_melt$Season<-DroughtMaxSeasons_Hist_melt$variable

##### Future/Baseline No Precip
Future_all$NoPrecip=Future_all$Precip <= PrecipThreshold
Baseline_all$NoPrecip=Baseline_all$Precip <= PrecipThreshold
Future_all$DroughtConsecutive=(Future_all$NoPrecip)*unlist(lapply(rle(Future_all$NoPrecip)$lengths, seq_len)) #count consecutive days over HotTemp.
Baseline_all$DroughtConsecutive=(Baseline_all$NoPrecip)*unlist(lapply(rle(Baseline_all$NoPrecip)$lengths, seq_len)) #count consecutive days over HotTemp.

#Baseline max drought duration (days with 0 precip)
DroughtMaxGCMs_baseline=data.frame(with(Baseline_all, tapply(DroughtConsecutive, list(GCM, Date$year), max)))
DroughtMaxGCMs_baseline=data.frame(CF="Historical", DroughtMaxDays=rowMeans(DroughtMaxGCMs_baseline)) #max values averaged over 30 years for each GCM
DroughtMaxGCMs_baseline$CF = factor(DroughtMaxGCMs_baseline$CF,ordered=TRUE,
                                    levels=unique(DroughtMaxGCMs_baseline$CF)) #correctly order the CFs
DroughtMaxGCMs_baseline$timeframe = "Historical"

##### Adjust baseline to match Historical
DroughtMaxGCMs_baseline$DroughtLengthComp = ((DroughtMax_Hist)/DroughtMaxGCMs_baseline$DroughtMaxDays)
DroughtMaxGCMs_baseline$Adjusted = with(DroughtMaxGCMs_baseline, DroughtMaxDays * DroughtLengthComp)

#Future max drought duration (days with 0 precip)
DroughtMaxGCMs_future=data.frame(with(Future_all, tapply(DroughtConsecutive, list(GCM, Date$year), max)))
DroughtMaxGCMs_future=data.frame(CF=CF_GCM$CF, DroughtMaxDays=rowMeans(DroughtMaxGCMs_future)) #max values averaged over 30 years for each GCM
DroughtMaxGCMs_future$CF = factor(DroughtMaxGCMs_future$CF,ordered=TRUE,
                                  levels=unique(DroughtMaxGCMs_future$CF)) #correctly order the CFs
DroughtMaxGCMs_future$timeframe = "Future"

#### Apply historical adjustment to future values
DroughtMaxGCMs_future$DroughtLengthComp = ((DroughtMax_Hist)/DroughtMaxGCMs_baseline$DroughtMaxDays)
DroughtMaxGCMs_future$Adjusted = with(DroughtMaxGCMs_future, DroughtMaxDays * DroughtLengthComp)

# Combine baseline and future drought (max consec days) dfs for boxplotting
DroughtMax=data.frame(rbind(DroughtMaxGCMs_baseline,DroughtMaxGCMs_future))

##### Remove extra data frames
rm(DroughtMaxGCMs_baseline, DroughtMaxGCMs_future)


#### Seasonal Drought Length

# Baseline max drought duration by season
DroughtSeasons_baseline=data.frame(CF="Historical",(with(Baseline_all, tapply(DroughtConsecutive, list(GCM, Date$year, season), max))))
DroughtSeasons_baseline=data.frame(CF="Historical", 
                                   Winter=rowMeans(DroughtSeasons_baseline[,grep("Winter", colnames(DroughtSeasons_baseline))]), 
                                   Spring=rowMeans(DroughtSeasons_baseline[,grep("Spring", colnames(DroughtSeasons_baseline))]), 
                                   Summer=rowMeans(DroughtSeasons_baseline[,grep("Summer", colnames(DroughtSeasons_baseline))]), 
                                   Fall=rowMeans(DroughtSeasons_baseline[,grep("Fall", colnames(DroughtSeasons_baseline))])) #max values averaged over 30 years for each GCM

DroughtSeasons_baseline$WinterComp = (DroughtMaxSeasons_Hist$Winter)/(DroughtSeasons_baseline$Winter)
DroughtSeasons_baseline$SpringComp = (DroughtMaxSeasons_Hist$Spring)/(DroughtSeasons_baseline$Spring)
DroughtSeasons_baseline$SummerComp = (DroughtMaxSeasons_Hist$Summer)/(DroughtSeasons_baseline$Summer)
DroughtSeasons_baseline$FallComp = (DroughtMaxSeasons_Hist$Fall)/(DroughtSeasons_baseline$Fall)

DroughtSeasons_baseline$WinterAdj = with(DroughtSeasons_baseline, Winter * WinterComp)
DroughtSeasons_baseline$SpringAdj = with(DroughtSeasons_baseline, Spring * SpringComp)
DroughtSeasons_baseline$SummerAdj = with(DroughtSeasons_baseline, Summer * SummerComp)
DroughtSeasons_baseline$FallAdj = with(DroughtSeasons_baseline, Fall * FallComp)


DroughtSeasons_baseline$CF = factor(DroughtSeasons_baseline$CF,ordered=TRUE,
                                    levels=unique(DroughtSeasons_baseline$CF)) #correctly order the CFs
DroughtSeasons_baseline$timeframe = "Historical"


# Future max drought duration by season
DroughtSeasons_future=data.frame(CF=CF_GCM$CF,(with(Future_all, tapply(DroughtConsecutive, list(GCM, Date$year, season), max))))
DroughtSeasons_future=data.frame(CF=CF_GCM$CF, 
                                 Winter=rowMeans(DroughtSeasons_future[,grep("Winter", colnames(DroughtSeasons_future))]), 
                                 Spring=rowMeans(DroughtSeasons_future[,grep("Spring", colnames(DroughtSeasons_future))]), 
                                 Summer=rowMeans(DroughtSeasons_future[,grep("Summer", colnames(DroughtSeasons_future))]), 
                                 Fall=rowMeans(DroughtSeasons_future[,grep("Fall", colnames(DroughtSeasons_future))]))

DroughtSeasons_future$WinterComp = (DroughtMaxSeasons_Hist$Winter)/(DroughtSeasons_baseline$Winter)
DroughtSeasons_future$SpringComp = (DroughtMaxSeasons_Hist$Spring)/(DroughtSeasons_baseline$Spring)
DroughtSeasons_future$SummerComp = (DroughtMaxSeasons_Hist$Summer)/(DroughtSeasons_baseline$Summer)
DroughtSeasons_future$FallComp = (DroughtMaxSeasons_Hist$Fall)/(DroughtSeasons_baseline$Fall)

DroughtSeasons_future$WinterAdj = with(DroughtSeasons_future, Winter * WinterComp)
DroughtSeasons_future$SpringAdj = with(DroughtSeasons_future, Spring * SpringComp)
DroughtSeasons_future$SummerAdj = with(DroughtSeasons_future, Summer * SummerComp)
DroughtSeasons_future$FallAdj = with(DroughtSeasons_future, Fall * FallComp)


DroughtSeasons_future$CF = factor(DroughtSeasons_future$CF,ordered=TRUE,
                                  levels=unique(DroughtSeasons_future$CF)) #correctly order the CFs
DroughtSeasons_future$timeframe = "Future"

keeps = c("CF", "WinterAdj", "SpringAdj", "SummerAdj", "FallAdj", "timeframe")

DroughtSeasons_baseline = DroughtSeasons_baseline[keeps]
DroughtSeasons_future = DroughtSeasons_future[keeps]

# Combine baseline and future drought (max consec zero precip days) dfs for boxplotting
DroughtSeasons=data.frame(rbind(DroughtSeasons_baseline,DroughtSeasons_future))
names(DroughtSeasons) = c("CF", "Winter", "Spring", "Summer", "Fall", "timeframe")
DroughtSeasons=melt(DroughtSeasons, id=c("CF","timeframe"))
names(DroughtSeasons)=c("CF","Timeframe","Season","DroughtMax")

#### Remove extra data frames
rm(DroughtSeasons_baseline, DroughtSeasons_future)


###### HISTORICAL MAX PRECIP ##########
PrecipMax_Hist = mean(tapply(Historical_all$PrecipCustom, Historical_all$Date$year, max))

##### Max Precip by CF
PrecipMax_baseline=data.frame(with(Baseline_all, tapply(PrecipCustom, list(GCM, Date$year), max)))
PrecipMax_baseline=data.frame(CF="Historical", PrecipMax=rowMeans(PrecipMax_baseline)) #max values averaged over 30 years for each GCM
PrecipMax_baseline$CF = factor(PrecipMax_baseline$CF, ordered=TRUE, 
                               levels=unique(PrecipMax_baseline$CF)) #correctly order the CFs
PrecipMax_baseline$timeframe = "Historical"

PrecipMax_baseline$PrecipComp = ((PrecipMax_Hist)/(PrecipMax_baseline$PrecipMax))
PrecipMax_baseline$AdjustedMaxPrecip = with(PrecipMax_baseline, PrecipMax * PrecipComp)

PrecipMax_future=data.frame(with(Future_all, tapply(PrecipCustom, list(GCM, Date$year), max)))
PrecipMax_future=data.frame(CF=CF_GCM$CF, PrecipMax=rowMeans(PrecipMax_future)) #max values averaged over 30 years for each GCM
PrecipMax_future$CF = factor(PrecipMax_future$CF, ordered=TRUE, 
                             levels=unique(PrecipMax_future$CF)) #correctly order the CFs
PrecipMax_future$timeframe = "Future"

PrecipMax_future$PrecipComp = ((PrecipMax_Hist)/(PrecipMax_baseline$PrecipMax))
PrecipMax_future$AdjustedMaxPrecip = with(PrecipMax_future, PrecipMax * PrecipComp)

PrecipMax=data.frame(rbind(PrecipMax_future, PrecipMax_baseline))

### #Remove extra data frames
rm(PrecipMax_baseline, PrecipMax_future)

#######################################################################################################
# Additional variables from DETO 
#  days over 1 inch, 95, and 99 percent precip, growing season, wet-frost, freeze-thaw
# create var for over 1 inch precip
Historical_all$OverPr1<-0
Historical_all$OverPr1[which(Historical_all$PrecipCustom >= 1)] <- 1
Future_all$OverPr1 <-0
Future_all$OverPr1[which(Future_all$PrecipCustom >= 1)] <- 1

# create var for over .5 inch precip
Historical_all$OverPr.5<-0
Historical_all$OverPr.5[which(Historical_all$PrecipCustom >= .5)] <- 1
Future_all$OverPr.5 <-0
Future_all$OverPr.5[which(Future_all$PrecipCustom >= .5)] <- 1
 
# create var for over 99% precip threshold
highpr99<-as.numeric(quantile(Historical_all$PrecipCustom, .99))
Historical_all$OverPr99<-0
Historical_all$OverPr99[which(Historical_all$PrecipCustom >= highpr99)] <- 1
Future_all$OverPr99 <-0
Future_all$OverPr99[which(Future_all$PrecipCustom >= highpr99)] <- 1
 
# create var for over 95% precip threshold
highpr95<-as.numeric(quantile(Historical_all$PrecipCustom, .95))
Historical_all$OverPr95<-0
Historical_all$OverPr95[which(Historical_all$PrecipCustom >= highpr95)] <- 1
Future_all$OverPr95 <-0
Future_all$OverPr95[which(Future_all$PrecipCustom >= highpr95)] <- 1

# create var for tmean
Historical_all$TmeanCustom<-(Historical_all$TmaxCustom + Historical_all$TminCustom) / 2
Future_all$TmeanCustom<-(Future_all$TmaxCustom + Future_all$TminCustom) / 2
 
# create var for freeze-thaw count 32
Historical_all$FT32<-0
Historical_all$FT32[which(Historical_all$TmaxCustom > 32 & Historical_all$TminCustom < 32)] <- 1
Future_all$FT32 <-0
Future_all$FT32[which(Future_all$TmaxCustom > 32 & Future_all$TminCustom < 32)] <- 1
 
# create var for freeze-thaw with buffer around freezing
Historical_all$FTbuff<-0
Historical_all$FTbuff[which(Historical_all$TmaxCustom > 34 & Historical_all$TminCustom < 28)] <- 1
Future_all$FTbuff <-0
Future_all$FTbuff[which(Future_all$TmaxCustom > 34 & Future_all$TminCustom < 28)] <- 1
 
# create var for wet-frost
# fun lag() from dplyr pkg. 

Historical_all$wet_frost<-0
Historical_all$wet_frost[which(Historical_all$TminCustom < 30 & lag(Historical_all$TmaxCustom > 32 & Historical_all$PrecipCustom > .08))] <- 1
Future_all$wet_frost<-0
Future_all$wet_frost[which(Future_all$TminCustom < 30 & lag(Future_all$TmaxCustom > 32 & Future_all$PrecipCustom > .08))] <- 1

# Start calculating green-up -- last date after 6 days with tmean > 5C
#Julian day
Historical_all$Julian<-yday(Historical_all$Date)
Future_all$Julian<-yday(Future_all$Date)

# tmean_C
Historical_all$Tmean_C<-(Historical_all$Tmax_C+Historical_all$Tmin_C)/2
Future_all$Tmean_C<-(Future_all$Tmax_C+Future_all$Tmin_C)/2

# recalc GDD as tmean_C > 5
Historical_all$GDD<-0
Historical_all$GDD[which(Historical_all$Tmean_C > 5)] <- 1
Future_all$GDD<-0
Future_all$GDD[which(Future_all$Tmean_C > 5)] <- 1

# count consecutive GDD
Historical_all$GDD_count<-0
Historical_all$GDD_count<- (Historical_all$GDD) * unlist(lapply(rle(Historical_all$GDD)$lengths, seq_len))
Future_all$GDD_count<-0
Future_all$GDD_count<- (Future_all$GDD) * unlist(lapply(rle(Future_all$GDD)$lengths, seq_len))

# count consecutive non-GDD
Historical_all$N_GDD_count<- 0
Historical_all$N_GDD_count<- (Historical_all$GDD == 0) * unlist(lapply(rle(Historical_all$GDD)$lengths, seq_len))
Future_all$N_GDD_count<- 0
Future_all$N_GDD_count<- (Future_all$GDD == 0) * unlist(lapply(rle(Future_all$GDD)$lengths, seq_len))

# return Julian date of first 7 each year, for each GCM
Historical_GS <- as.data.table(subset(Historical_all,select=c(Year,Julian,GDD_count,N_GDD_count)))
Historical_GU<-Historical_GS[GDD_count==7,.SD[1],by=.(Year)] 

Future_GS <- as.data.table(subset(Future_all,select=c(Year,GCM,Julian,GDD_count,N_GDD_count)))
Future_GU<-Future_GS[GDD_count==7,.SD[1],by=.(Year,GCM)] 

# return Julian date of end growing season
# Historical_SE<-Historical_GS[GDD_count==6,.SD[c(.N)],by=.(Year,GCM)]
Historical_SE<-Historical_GS[N_GDD_count==6,.SD[c(.N)],by=.(Year)]
Historical_SE$adjusted<-Historical_SE$Julian - 6
Future_SE<-Future_GS[N_GDD_count==6,.SD[c(.N)],by=.(Year,GCM)]
Future_SE$adjusted<-Future_SE$Julian - 6

# Create annual dataframe to hold annually aggregated variables
Hist_t2_annual<-aggregate(cbind(Julian)~Year,data=Historical_GU,mean,na.rm=TRUE)
colnames(Hist_t2_annual)[2] <- "BegGrow"
Hist_t2_annual<-cbind(Hist_t2_annual,Historical_SE$adjusted)
colnames(Hist_t2_annual)[3] <- "EndGrow"
Hist_t2_annual$GrowLen<- Hist_t2_annual$EndGrow - Hist_t2_annual$BegGrow

Fut_t2_annual<-aggregate(cbind(Julian)~GCM+Year,data=Future_GU,mean,na.rm=TRUE)
colnames(Fut_t2_annual)[3] <- "BegGrow"
Fut_t2_annual<-merge(Fut_t2_annual,Future_SE[,c("GCM","Year","adjusted")], by=c("Year","GCM"))
colnames(Fut_t2_annual)[4] <- "EndGrow"
Fut_t2_annual$GrowLen<- Fut_t2_annual$EndGrow - Fut_t2_annual$BegGrow

rm(Historical_GS, Historical_GU, Historical_SE, Future_GS, Future_GU, Future_SE)

F_other<-aggregate(cbind(OverPr1, OverPr.5, OverPr99, OverPr95,FT32, FTbuff, wet_frost)~GCM+Year, 
                   data=Future_all, sum, na.rm=TRUE)
H_other<-aggregate(cbind(OverPr1, OverPr.5, OverPr99, OverPr95,FT32, FTbuff, wet_frost)~Year, 
                   data=Historical_all, sum, na.rm=TRUE)

Fut_t2_annual<-merge(Fut_t2_annual,F_other,by=c("Year","GCM"))
Fut_t2_annual<-merge(Fut_t2_annual,CF_GCM,by="GCM")

Hist_t2_annual<-merge(Hist_t2_annual,H_other,by=c("Year"))

rm(F_other, H_other)

#### Create .xslx workbook with all data tables
setwd(WD_plots)
WriteXLS(c("Monthly_Precip_delta", "Monthly_Tmax_delta", "Monthly_Tmin_delta", "PrecipMax", "HeatMax", "ColdMax", "DroughtMax", "TotalOverHotTemp", "TotalOver95th", "TotalUnderColdTemp", "TotalUnder5th", "Future_Means"), 
         sprintf("%s_%s_%s_CCSP_Plot_data.xlsx", SiteID, Lat, Lon), BoldHeaderRow = TRUE)

##### Save Current workspace environment
save.image(sprintf("%s_%s_%s_Final_Environment.RData",SiteID, Lat, Lon))

#  EOF
