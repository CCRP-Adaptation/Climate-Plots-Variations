# RSS_Plot_Table_Creation vxx.R
# v02.1 - added Future_Means to xlsx output.
# v02 STABLE - 22 Oct 2015 - days below ColdTemp added and debugged.

#### Create tables with monthly tmax/tmin delta by CF
Monthly_Tmax_delta = data.frame(months,Tmax_delta=with(Future_all, tapply(TmaxCustom, list(Date$mon, CF), mean))-
                                  with(Baseline_all, tapply(TmaxCustom, list(Date$mon, CF), mean)))
Monthly_Tmax_delta = melt(Monthly_Tmax_delta)
names(Monthly_Tmax_delta) = c("month","CF","Tmax") 
Monthly_Tmax_delta$CF = factor(Monthly_Tmax_delta$CF, 
                               levels = c("Tmax_delta.Warm.Wet", "Tmax_delta.Hot.Wet", "Tmax_delta.Central", "Tmax_delta.Warm.Dry", "Tmax_delta.Hot.Dry"), 
                               labels=c("Warm Wet", "Hot Wet", "Central", "Warm Dry", "Hot Dry"))

Monthly_Tmin_delta = data.frame(months,Tmin_delta=with(Future_all, tapply(TminCustom, list(Date$mon, CF), mean))-
                                  with(Baseline_all, tapply(TminCustom, list(Date$mon, CF), mean)))
Monthly_Tmin_delta = melt(Monthly_Tmin_delta)
names(Monthly_Tmin_delta) = c("month","CF","Tmin") 
Monthly_Tmin_delta$CF = factor(Monthly_Tmin_delta$CF, 
                               levels = c("Tmin_delta.Warm.Wet", "Tmin_delta.Hot.Wet", "Tmin_delta.Central", "Tmin_delta.Warm.Dry", "Tmin_delta.Hot.Dry"), 
                               labels=c("Warm Wet", "Hot Wet", "Central", "Warm Dry", "Hot Dry"))


#### Create table with monthly precip delta by CF
Monthly_Precip_delta = data.frame(months,Precip_delta=with(Future_all, tapply(PrecipCustom, list(Date$mon, CF), mean))-
                                    with(Baseline_all, tapply(PrecipCustom, list(Date$mon, CF), mean)))
Monthly_Precip_delta = melt(Monthly_Precip_delta)
names(Monthly_Precip_delta) = c("month","CF","Precip")
Monthly_Precip_delta$CF = factor(Monthly_Precip_delta$CF, 
                                 levels = c("Precip_delta.Warm.Wet", "Precip_delta.Hot.Wet", "Precip_delta.Central", "Precip_delta.Warm.Dry", "Precip_delta.Hot.Dry"), 
                                 labels=c("Warm Wet", "Hot Wet", "Central", "Warm Dry", "Hot Dry"))


###### TOTAL & CONSECUTIVE DAYS OVER/UNDER THRESHOLD TEMPs ######

#### Historical
Historical_all$Over_100 = Historical_all$TmaxCustom > HotTemp
Hist_Total_100_Days = as.numeric(sum(Historical_all$Over_100)/HistYears)

Historical_all$HeatConsecutive=(Historical_all$Over_100)*unlist(lapply(rle(Historical_all$Over_100)$lengths, seq_len))
HeatMaxYearly_Hist = data.frame(HeatMax=tapply(Historical_all$HeatConsecutive, Historical_all$Date$year, max))
HeatMaxHist = as.numeric(mean(HeatMaxYearly_Hist$HeatMax))

Historical_all$Under_ColdTemp = Historical_all$TminCustom < ColdTemp
Hist_Total_Under_ColdTemp_Days = as.numeric(sum(Historical_all$Under_ColdTemp)/HistYears)

Historical_all$ColdConsecutive=(Historical_all$Under_ColdTemp)*unlist(lapply(rle(Historical_all$Under_ColdTemp)$lengths, seq_len))
ColdMaxYearly_Hist = data.frame(ColdMax=tapply(Historical_all$ColdConsecutive, Historical_all$Date$year, max))
ColdMaxHist = as.numeric(mean(ColdMaxYearly_Hist$ColdMax))

#### Baseline
Baseline_all$Over100=Baseline_all$TmaxCustom > (HotTemp)

BaseOver100 = data.frame(aggregate(Baseline_all$Over100 ~ Baseline_all$GCM,Future_all,sum))
names(BaseOver100) = c("GCM", "Total_100_Days")
BaseOver100 = merge(BaseOver100, CF_GCM, by ="GCM")
BaseOver100$timeframe = "Historical"

BaseOver100$Over100Comp = ifelse(BaseOver100$Total_100_Days != 0, (Hist_Total_100_Days)/(BaseOver100$Total_100_Days), 1)
BaseOver100$Adjusted = ifelse((BaseOver100$Over100Comp == 0), Hist_Total_100_Days, (BaseOver100$Total_100_Days * BaseOver100$Over100Comp))

##### Under ColdTemp
Baseline_all$UnderColdTemp=Baseline_all$TminCustom < ColdTemp

BaseUnderColdTemp = data.frame(aggregate(Baseline_all$UnderColdTemp ~ Baseline_all$GCM,Future_all,sum))  # sum by GCM
names(BaseUnderColdTemp) = c("GCM", "Total_Cold_Days")
BaseUnderColdTemp = merge(BaseUnderColdTemp, CF_GCM, by ="GCM")
BaseUnderColdTemp$timeframe = "Historical"

BaseUnderColdTemp$UnderColdTempComp = ifelse(BaseUnderColdTemp$Total_Cold_Days != 0, 
                                             (Hist_Total_Under_ColdTemp_Days)/(BaseUnderColdTemp$Total_Cold_Days), 1)
BaseUnderColdTemp$Adjusted = ifelse((BaseUnderColdTemp$UnderColdTempComp == 0), 
                                    Hist_Total_UnderColdTemp_Days, (BaseUnderColdTemp$Total_Cold_Days * BaseUnderColdTemp$UnderColdTempComp))


#### Future
Future_all$Over100=Future_all$TmaxCustom > (HotTemp)

FutureOver100 = data.frame(aggregate(Future_all$Over100 ~ Future_all$GCM,Future_all,sum))
names(FutureOver100) = c("GCM", "Total_100_Days")
FutureOver100 = merge(FutureOver100, CF_GCM, by ="GCM")
FutureOver100$timeframe = "Future"

FutureOver100$Over100Comp = ifelse(FutureOver100$Total_100_Days != 0, (Hist_Total_100_Days)/(FutureOver100$Total_100_Days), 1)
FutureOver100$Adjusted = ifelse((BaseOver100$Over100Comp == 0), Hist_Total_100_Days, (FutureOver100$Total_100_Days * BaseOver100$Over100Comp))

##### Under
Future_all$UnderColdTemp=Future_all$TminCustom < ColdTemp

FutureUnderColdTemp = data.frame(aggregate(Future_all$UnderColdTemp ~ Future_all$GCM,Future_all,sum))  # sum by GCM
names(FutureUnderColdTemp) = c("GCM", "Total_Cold_Days")
FutureUnderColdTemp = merge(FutureUnderColdTemp, CF_GCM, by ="GCM")
FutureUnderColdTemp$timeframe = "Future"

FutureUnderColdTemp$UnderColdTempComp = ifelse(FutureUnderColdTemp$Total_Cold_Days != 0, 
                                               (Hist_Total_Under_ColdTemp_Days)/(FutureUnderColdTemp$Total_Cold_Days), 1)
FutureUnderColdTemp$Adjusted = ifelse((BaseUnderColdTemp$UnderColdTempComp == 0), 
                                      Hist_Total_UnderColdTemp_Days, (FutureUnderColdTemp$Total_Cold_Days * BaseUnderColdTemp$UnderColdTempComp))

### Total
TotalOver100 = data.frame(rbind(FutureOver100, BaseOver100))
TotalUnderColdTemp = data.frame(rbind(FutureUnderColdTemp, BaseUnderColdTemp))

# Remove extra data frames
rm(FutureOver100, BaseOver100, FutureUnderColdTemp, BaseUnderColdTemp)


#Baseline max consecutive days over 100F per year by GCM and CF in 1950-1999 (HistYears = 1950 thru 1999 inclusive = 50)
Baseline_all$HeatConsecutive=(Baseline_all$Over100)*unlist(lapply(rle(Baseline_all$Over100)$lengths, seq_len))
HeatMax_baseline=data.frame(with(Baseline_all, tapply(HeatConsecutive, list(GCM, Date$year), max)))
HeatMax_baseline=data.frame(CF=CF_GCM$CF, HeatMaxDays=rowMeans(HeatMax_baseline)) #max values averaged over 30 years for each GCM
HeatMax_baseline$CF = factor(HeatMax_baseline$CF, ordered=TRUE, 
                                 levels=c("Warm Wet","Hot Wet","Central","Warm Dry","Hot Dry")) #correctly order the CFs
HeatMax_baseline$timeframe = "Historical"

HeatMax_baseline$HeatConsecutiveComp = ifelse(HeatMax_baseline$HeatMaxDays != 0, (HeatMaxHist)/(HeatMax_baseline$HeatMaxDays), 1)
HeatMax_baseline$Adjusted = ifelse((HeatMax_baseline$HeatConsecutiveComp == 0), HeatMaxHist, (HeatMax_baseline$HeatMaxDays * HeatMax_baseline$HeatConsecutiveComp))

       # Historical Cold days
Baseline_all$ColdConsecutive=(Baseline_all$UnderColdTemp)*unlist(lapply(rle(Baseline_all$UnderColdTemp)$lengths, seq_len))
ColdMax_baseline=data.frame(with(Baseline_all, tapply(ColdConsecutive, list(GCM, Date$year), max)))
ColdMax_baseline=data.frame(CF=CF_GCM$CF, ColdMaxDays=rowMeans(ColdMax_baseline)) #max values averaged over 30 years for each GCM
ColdMax_baseline$CF = factor(ColdMax_baseline$CF, ordered=TRUE, 
                             levels=c("Warm Wet","Hot Wet","Central","Warm Dry","Hot Dry")) #correctly order the CFs
ColdMax_baseline$timeframe = "Historical"

ColdMax_baseline$ColdConsecutiveComp = ifelse(ColdMax_baseline$ColdMaxDays != 0, (ColdMaxHist)/(ColdMax_baseline$ColdMaxDays), 1)
ColdMax_baseline$Adjusted = ifelse((ColdMax_baseline$ColdConsecutiveComp == 0), ColdMaxHist, (ColdMax_baseline$ColdMaxDays * ColdMax_baseline$ColdConsecutiveComp))


#Future max consecutive days over 100F per year by GCM and CF in 2040 
Future_all$HeatConsecutive=(Future_all$Over100)*unlist(lapply(rle(Future_all$Over100)$lengths, seq_len))
HeatMax_future=data.frame(with(Future_all, tapply(HeatConsecutive, list(GCM, Date$year), max)))
HeatMax_future=data.frame(CF=CF_GCM$CF, HeatMaxDays=rowMeans(HeatMax_future)) #max values averaged over 30 years for each GCM
HeatMax_future$CF = factor(HeatMax_future$CF, ordered=TRUE, 
                               levels=c("Warm Wet","Hot Wet","Central","Warm Dry","Hot Dry")) #correctly order the CFs
HeatMax_future$timeframe = "Future"

   # Future max consecutive cold days by GCM & CF
Future_all$ColdConsecutive=(Future_all$UnderColdTemp)*unlist(lapply(rle(Future_all$UnderColdTemp)$lengths, seq_len))
ColdMax_future=data.frame(with(Future_all, tapply(ColdConsecutive, list(GCM, Date$year), max)))
ColdMax_future=data.frame(CF=CF_GCM$CF, ColdMaxDays=rowMeans(ColdMax_future)) #max values averaged over 30 years for each GCM
ColdMax_future$CF = factor(ColdMax_future$CF, ordered=TRUE, 
                           levels=c("Warm Wet","Hot Wet","Central","Warm Dry","Hot Dry")) #correctly order the CFs
ColdMax_future$timeframe = "Future"

#max consecutive 100 deg days
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

DroughtMaxSeasons_Hist_melt = melt(DroughtMaxSeasons_Hist)
DroughtMaxSeasons_Hist_melt$Season<-DroughtMaxSeasons_Hist_melt$variable

##### Future/Baseline No Precip
Future_all$NoPrecip=Future_all$Precip <= PrecipThreshold
Baseline_all$NoPrecip=Baseline_all$Precip <= PrecipThreshold
Future_all$DroughtConsecutive=(Future_all$NoPrecip)*unlist(lapply(rle(Future_all$NoPrecip)$lengths, seq_len)) #count consecutive days over 100 F.
Baseline_all$DroughtConsecutive=(Baseline_all$NoPrecip)*unlist(lapply(rle(Baseline_all$NoPrecip)$lengths, seq_len)) #count consecutive days over 100 F.

#Baseline max drought duration (days with 0 precip)
DroughtMaxGCMs_baseline=data.frame(with(Baseline_all, tapply(DroughtConsecutive, list(GCM, Date$year), max)))
DroughtMaxGCMs_baseline=data.frame(CF=CF_GCM$CF, DroughtMaxDays=rowMeans(DroughtMaxGCMs_baseline)) #max values averaged over 30 years for each GCM
DroughtMaxGCMs_baseline$CF = factor(DroughtMaxGCMs_baseline$CF,ordered=TRUE,
                                    levels=c("Warm Wet","Hot Wet","Central","Warm Dry","Hot Dry")) #correctly order the CFs
DroughtMaxGCMs_baseline$timeframe = "Historical"

##### Adjust baseline to match Historical
DroughtMaxGCMs_baseline$DroughtLengthComp = ((DroughtMax_Hist)/DroughtMaxGCMs_baseline$DroughtMaxDays)
DroughtMaxGCMs_baseline$Adjusted = with(DroughtMaxGCMs_baseline, DroughtMaxDays * DroughtLengthComp)

#Future max drought duration (days with 0 precip)
DroughtMaxGCMs_future=data.frame(with(Future_all, tapply(DroughtConsecutive, list(GCM, Date$year), max)))
DroughtMaxGCMs_future=data.frame(CF=CF_GCM$CF, DroughtMaxDays=rowMeans(DroughtMaxGCMs_future)) #max values averaged over 30 years for each GCM
DroughtMaxGCMs_future$CF = factor(DroughtMaxGCMs_future$CF,ordered=TRUE,
                                  levels=c("Warm Wet","Hot Wet","Central","Warm Dry","Hot Dry")) #correctly order the CFs
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
DroughtSeasons_baseline=data.frame(CF=CF_GCM$CF,(with(Baseline_all, tapply(DroughtConsecutive, list(GCM, Date$year, season), max))))
DroughtSeasons_baseline=data.frame(CF=CF_GCM$CF, 
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


DroughtSeasons_baseline$CF = factor(DroughtSeasons_baseline$CF,ordered=TRUE,levels=c("Warm Wet","Hot Wet","Central","Warm Dry","Hot Dry")) #correctly order the CFs
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


DroughtSeasons_future$CF = factor(DroughtSeasons_future$CF,ordered=TRUE,levels=c("Warm Wet","Hot Wet","Central","Warm Dry","Hot Dry")) #correctly order the CFs
DroughtSeasons_future$timeframe = "Future"

keeps = c("CF", "WinterAdj", "SpringAdj", "SummerAdj", "FallAdj", "timeframe")

DroughtSeasons_baseline = DroughtSeasons_baseline[keeps]
DroughtSeasons_future = DroughtSeasons_future[keeps]

# Combine baseline and future drought (max consec zero precip days) dfs for boxplotting
DroughtSeasons=data.frame(rbind(DroughtSeasons_baseline,DroughtSeasons_future))
names(DroughtSeasons) = c("CF", "Winter", "Spring", "Summer", "Fall", "timeframe")
DroughtSeasons=melt(DroughtSeasons)
names(DroughtSeasons)=c("CF","Timeframe","Season","DroughtMax")

#### Remove extra data frames
rm(DroughtSeasons_baseline, DroughtSeasons_future)


###### HISTORICAL MAX PRECIP ##########
PrecipMax_Hist = mean(tapply(Historical_all$PrecipCustom, Historical_all$Date$year, max))

##### Max Precip by CF
PrecipMax_baseline=data.frame(with(Baseline_all, tapply(PrecipCustom, list(GCM, Date$year), max)))
PrecipMax_baseline=data.frame(CF=CF_GCM$CF, PrecipMax=rowMeans(PrecipMax_baseline)) #max values averaged over 30 years for each GCM
PrecipMax_baseline$CF = factor(PrecipMax_baseline$CF, ordered=TRUE, 
                               levels=c("Warm Wet","Hot Wet","Central","Warm Dry","Hot Dry")) #correctly order the CFs
PrecipMax_baseline$timeframe = "Historical"

PrecipMax_baseline$PrecipComp = ((PrecipMax_Hist)/(PrecipMax_baseline$PrecipMax))
PrecipMax_baseline$AdjustedMaxPrecip = with(PrecipMax_baseline, PrecipMax * PrecipComp)

PrecipMax_future=data.frame(with(Future_all, tapply(PrecipCustom, list(GCM, Date$year), max)))
PrecipMax_future=data.frame(CF=CF_GCM$CF, PrecipMax=rowMeans(PrecipMax_future)) #max values averaged over 30 years for each GCM
PrecipMax_future$CF = factor(PrecipMax_future$CF, ordered=TRUE, 
                             levels=c("Warm Wet","Hot Wet","Central","Warm Dry","Hot Dry")) #correctly order the CFs
PrecipMax_future$timeframe = "Future"

PrecipMax_future$PrecipComp = ((PrecipMax_Hist)/(PrecipMax_baseline$PrecipMax))
PrecipMax_future$AdjustedMaxPrecip = with(PrecipMax_future, PrecipMax * PrecipComp)

PrecipMax=data.frame(rbind(PrecipMax_future, PrecipMax_baseline))

### #Remove extra data frames
rm(PrecipMax_baseline, PrecipMax_future)

#### Create .xslx workbook with all data tables
setwd(WD_plots)
WriteXLS(c("Monthly_Precip_delta", "Monthly_Tmax_delta", "Monthly_Tmin_delta", "PrecipMax", "HeatMax", "ColdMax", "DroughtMax", "TotalOver100", "TotalUnderColdTemp", "Future_Means"), 
         sprintf("%s_%s_%s_CCSP_Plot_data.xlsx", SiteID, Lat, Lon), BoldHeaderRow = TRUE)

##### Save Current workspace environment
save.image(sprintf("%s_%s_%s_Final_Environment.RData",SiteID, Lat, Lon))

#  EOF