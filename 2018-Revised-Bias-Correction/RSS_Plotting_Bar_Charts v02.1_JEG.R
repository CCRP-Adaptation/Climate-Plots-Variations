# RSS_Plotting vxx.R

# _JEG v02.1 - moved subset of futures into variable. revised 30 March 2016
# _JEG v02 - fixed error in subsetting future data.
# _JEG v01 - modifed to read RData file; added SiteID, Lat, Lon to output file names. Input and output directories are the same.  18 Nov 2015.
# modified version of v01 (22 Oct 2016) that expresses all but the by-season 4-panel drought chart as bar charts instead of box plots
# (by-season 4-panel drought simply dropped from this version)
# this version also allows the user to set the site name and the cold and hot temp thresholds implicit in the associated input files
# related to number of days in which conditions are above or below a temp threshold (graph titles incorporate this input)
# note also that the input csv files for the days-above the hot temp threshold and days below the cold temp threshold should be named
# "TotalOverHotTemp" and "TotalUnderColdTemp", respectively

# Note: the subset of scenarios for graphing are identified individually for each graph in lines 39-59; we could instead have a single
# place to identify the subset and then adjust the coding for each plot to follow that choice (but note that unless the subset is
# Warm Dry, Central, & Hot Wet, color coding will have to be adjusted for each plot

# Color coding info:  COLOR CODING IS INCONSISTENT BELOW. I suggest use of R system colors: "blue", "green", "yellow", "orange", "red" and not hex
# blue = "#2B83BA"
# green = "#ABDDA4"
# yellow = "#FFFFBF"
# orange = "#FDAE61"
# red = "#D7191C"

#setwd(WD_plots)
library(ggplot2)
library(plyr)
library(lubridate)
library(dplyr)
library(forcats)
library(reshape2)

rm(list=ls())

setwd("~/RSS/Parks/CHCU/Figs MACA")
load("CHCU_36.02_-107.898_Final_Environment.RData")

################ INITIALS ##########################33

# Need to check all the subsets
### NEED TO CHANGE LINE 72 for ordering scenarios on plots
FutureSubset <- c("Warm Wet","Hot Dry")          # Select two scenarios from the CFs vector specified in CMIP5_Parsing script. Names must match.
Scenario1<-FutureSubset[1]
Scenario2<-FutureSubset[2]

#Colors for RCP 4.5, RCP 8.5
col.RCP2 = c("blue", "red")

## All 508-compliant color scheme -- navy (hot wet), light blue (warm wet), pink (warm dry), red (hot dry)
colors5 <-   c("#12045C","#9A9EE5","#F3D3CB","#E10720","white")
#colors2<-c("#12045C","#F3D3CB") #navy - hot wet; pink - warm dry
colors2 <- c("blue", "orange")
colors3 <- c("dark grey","blue", "orange")

##Plot parameters

#Height and width 
PlotWidth = 15
PlotHeight = 9

#ggplot theme to control formatting parameters for plots with month on the x-axis
PlotTheme = theme(axis.text=element_text(size=20),    #Text size for axis tick mark labels
                  axis.title.x=element_text(size=24, hjust=0.5, margin=margin(t=20, r=20, b=20, l=20)),               #Text size and alignment for x-axis label
                  axis.title.y=element_text(size=24, vjust=0.5,  margin=margin(t=20, r=20, b=20, l=20)),              #Text size and alignment for y-axis label
                  plot.title=element_text(size=26,face="bold",hjust=0.5, margin=margin(t=20, r=20, b=20, l=20)),      #Text size and alignment for plot title
                  legend.title=element_text(size=24),                                                                    #Text size of legend category labels
                  legend.text=element_text(size=20),                                                                   #Text size of legend title
                  legend.position = "right")                                                                           #Legend position

#X-axis labels for monthly plots
MonthLabels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")

OutDir = "WW HD Scenarios"

#################### END INITIALS ##########################

########################## SCENARIO TABLES #####################

MP3<-subset(Monthly_Precip_delta, CF %in% FutureSubset)
MP3$month<-factor(MP3$month,levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

DM3<-subset(DroughtLength, CF %in% FutureSubset & timeframe == "Future")
DM3$CF<-factor(DM3$CF,levels=c(Scenario1, "Central", Scenario2))
DMH<-subset(DroughtLength, timeframe == "Historical")

Tmax3<-subset(Monthly_Tmax_delta, CF %in% FutureSubset)
Tmax3$month<-factor(Tmax3$month,levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

Tmin3<-subset(Monthly_Tmin_delta, CF %in% FutureSubset)
Tmin3$month<-factor(Tmin3$month,levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

TOHotTempH<-subset(TotalOverHotTemp, timeframe == "Historical")
TOHotTemp3<-subset(TotalOverHotTemp, CF %in% FutureSubset & timeframe == "Future")

TCOHotTempH<-subset(TotalHeatConsecutive, timeframe == "Historical")
TCOHotTemp3<-subset(TotalHeatConsecutive, CF %in% FutureSubset)

TUColdTempH<-subset(TotalUnderColdTemp, timeframe == "Historical")
TUColdTemp3<-subset(TotalUnderColdTemp, CF %in% FutureSubset& timeframe == "Future")

TULowQH<-subset(TotalUnderLowQ, CF == "Historical")
TULowQ3<-subset(TotalUnderLowQ, CF %in% FutureSubset & timeframe == "Future")

TOHighQH<-subset(TotalOverHighQ, CF == "Historical")
TOHighQ3<-subset(TotalOverHighQ, CF %in% FutureSubset & timeframe == "Future")

################################################# END SCENARIO TABLES ####################################

############################################# OUTPUT PLOTS ###############################################

setwd(OutDir)

###Scatter plot showing delta precip and tavg, color by emissions scenario, x-axis scaled 0-max, with points for averages of 3 CFs
scatter = ggplot(Future_Means, aes(DeltaTavg, 365*DeltaPr, xmin=Tavg25, xmax=Tavg75, ymin=365*Pr25, ymax=365*Pr75))
scatter + geom_point(aes(color=emissions),size=4) + 
  PlotTheme + 
  labs(title = paste(SiteID, "- Changes in climate means in", Year,"by GCM run"), 
            x = "Change in annual average temperature (F)", 
            y = "Change in average annual precipitation (in)") +
  scale_colour_manual(values=col.RCP2)+
  guides(color=guide_legend(title="Emissions\nScenarios\n")) +
  # geom_rect(color = "blue", alpha=0) + 
  #  geom_hline(aes(yintercept=365*mean(Future_Means$DeltaPr)),linetype=2) + 
  #  geom_vline(aes(xintercept=mean(Future_Means$DeltaTavg)),linetype=2)  +
  geom_point(aes(x=mean(DeltaTavg), y=mean(365*DeltaPr)), shape=23, size=10, fill='black', colour='black') +
  geom_point(aes(x=mean(DeltaTavg[which(CF==Scenario1)]), y=mean(365*DeltaPr[which(CF==Scenario1)])), shape=23, size=10, fill='black', colour='black') +
  geom_point(aes(x=mean(DeltaTavg[which(CF==Scenario2)]), y=mean(365*DeltaPr[which(CF==Scenario2)])), shape=23, size=10, fill='black', colour='black') +
  scale_x_continuous(limits=c(0, max(Future_Means$DeltaTavg)+.25))

ggsave(sprintf("%s_%s_%s_GCM_Scatter_CF_Averages_Plot.png", SiteID, Lat, Lon), width = PlotWidth, height = PlotHeight)

      # set CF order for bar graph of change in average monthly precip by CF 

MP3$CF<-factor(MP3$CF,levels=c(Scenario1, Scenario2), ordered=is.ordered(MP3$CF))

#Bar graph of change in average monthly precip by CF
ggplot(MP3, aes(x=month,y=30*Precip,fill=CF)) +
  geom_bar(stat="identity",position="dodge") +
  PlotTheme +
  labs(title = paste(SiteID, "- Change in Avg. Monthly Precip. in Historical (1950-2005) vs.", Year), 
       x = "Month", y = "Change in Precipitation (in)") +
  scale_fill_manual(name="Climate Future",values = colors2) +
  scale_x_discrete(labels = MonthLabels)

ggsave(sprintf("%s_%s_%s_CF_Avg_Monthly_Precip_Delta_Bar.png", SiteID, Lat, Lon), width = PlotWidth, height = PlotHeight)


    # set CF order for bar graph of change in average monthly precip by CF 
Tmax3$CF<-factor(Tmax3$CF,levels=c(Scenario1, "Central", Scenario2), ordered=is.ordered(Tmax3$CF))

    #Line plot of change in MaxTemp by CF/month
ggplot(Tmax3, aes(x=month, y=Tmax, group=CF, colour = CF)) +
  geom_line(size = 2, stat = "identity") + geom_point(shape = 21, size = 5, fill = "white") +
  PlotTheme +
  labs(title = paste(SiteID, "- Change in Avg. Monthly Tmax in Historical (1950-2005) vs.", Year), 
            x = "Month", y = "Change in Temperature (Deg F)") +
  scale_color_manual(name="Climate Future",values = colors2) +
  scale_y_continuous(limits=c(0, ceiling(max(Monthly_Tmax_delta$Tmax)))) + 
  scale_x_discrete(labels = MonthLabels)

ggsave(sprintf("%s_%s_%s_Avg_Monthly_Tmax_Delta_Line.png", SiteID, Lat, Lon), width = PlotWidth, height = PlotHeight)


     #set CF order for bar graph of change in average monthly precip by CF 
Tmin3$CF<-factor(Tmin3$CF,levels=c(Scenario1, Scenario2), ordered=is.ordered(Tmin3$CF))

      ####Line Plot of change in MinTemp by CF/Month
ggplot(Tmin3, aes(x=month, y=Tmin, group=CF, colour = CF)) +
  geom_line(size = 2, stat = "identity") + 
  geom_point(shape = 21, size = 5, fill = "white") +
  PlotTheme +
  labs(title = paste(SiteID, "- Change in Avg. Daily Tmin in Historical (1950-2005) vs.", Year),
            x = "Month", y = "Change in Temperature (Deg F)") +
  scale_color_manual(name="Climate Future",values = colors2) +
  scale_y_continuous(limits=c(0, ceiling(max(Monthly_Tmin_delta$Tmin)))) +
  scale_x_discrete(labels = MonthLabels)

ggsave(sprintf("%s_%s_%s_Avg_Monthly_Tmin_Delta_Line.png", SiteID, Lat, Lon), width = PlotWidth, height = PlotHeight)


        # create dataset for bar plot of drought duration (max consecutive # of days)
DM3mean<-ddply(DM3, "CF", summarise,
                   DroughtLength=mean(DroughtLength))
newrow <- data.frame( "CF" = character(), "DroughtLength" = numeric(), stringsAsFactors=FALSE)
newrow[nrow(newrow) + 1, ] <- c( "Historical", mean(DMH$DroughtLength))
DM3mean<-rbind(DM3mean, newrow)
DM3mean$CF<-factor(DM3mean$CF,levels=c("Historical",Scenario1, Scenario2), ordered=is.ordered(DM3mean$CF))
DM3mean$DroughtLength<-as.numeric(DM3mean$DroughtLength)
rm(newrow)

    #Bar graph of future drougth duration (max consecutive # of days) vs. historical mean and CF
ggplot(DM3mean, aes(x=CF,y=DroughtLength,fill=CF)) +
  geom_bar(stat="identity",position="dodge") +
  PlotTheme +
  labs(title = paste(SiteID, "- Max Annual Avg. Drought Length in Historical (1950-2005) vs.", Year), 
       x = "Historical & Future Climate Scenarios", y = "Drought Length (Days)") +
  scale_fill_manual(name="", values = colors3)

ggsave(sprintf("%s_%s_%s_Max_Drought_Length_Bar.png", SiteID, Lat, Lon), width = PlotWidth, height = PlotHeight)


      # create dataset for bar graph of total number of days/year over hot temperature threshold
TOHotTempmean<-ddply(TOHotTemp3, "CF", summarise,
               HotDays=mean(HotDays))
newrow <- data.frame( "CF" = character(), "HotDays" = numeric(), stringsAsFactors=FALSE)
newrow[nrow(newrow) + 1, ] <- c( "Historical", mean(TOHotTempH$HotDays))
TOHotTempmean<-rbind(TOHotTempmean, newrow)
TOHotTempmean$CF<-factor(TOHotTempmean$CF,levels=c("Historical",Scenario1, "Central", Scenario2), ordered=is.ordered(TOHotTempmean$CF))
TOHotTempmean$HotDays<-as.numeric(TOHotTempmean$HotDays)
rm(newrow)

      #Bar graph of total number of days/year over hot temperature threshold
ggplot(TOHotTempmean, aes(x=CF,y=HotDays,fill=CF)) +
  geom_bar(stat="identity",position="dodge") +
  PlotTheme +
  #coord_cartesian(ylim=c(50, 65)) +
  labs(title = paste(SiteID, "- Avg. Tot. Days/Yr >", HotTemp, "Deg. in Historical (1950-2005) vs.", Year), 
            x = "Historical & Future Climate Scenarios", y = "Days/Yr", colour = "Climate Future")  +
  scale_fill_manual(name="",values = colors3)

ggsave(sprintf("%s_%s_%s_Days_Over_HotTemp.png", SiteID, Lat, Lon), width = PlotWidth, height = PlotHeight)


    # creare dataset for  bar graph of total number of CONSECUTIVE days/year over hot temperature threshold
TCOHotTempmean<-ddply(TCOHotTemp3, "CF", summarise,
                 ConsHotDays=mean(ConsHotDays))
newrow <- data.frame( "CF" = character(), "ConsHotDays" = numeric(), stringsAsFactors=FALSE)
newrow[nrow(newrow) + 1, ] <- c( "Historical", mean(TCOHotTempH$ConsHotDays))
TCOHotTempmean<-rbind(TCOHotTempmean, newrow)
TCOHotTempmean$CF<-factor(TCOHotTempmean$CF,levels=c("Historical",Scenario1, "Central", Scenario2), ordered=is.ordered(TCOHotTempmean$CF))
TCOHotTempmean$ConsHotDays<-as.numeric(TCOHotTempmean$ConsHotDays)
rm(newrow)

      #Bar graph of total number of CONSECUTIVE days/year over hot temperature threshold
ggplot(TCOHotTempmean, aes(x=CF,y=ConsHotDays,fill=CF)) +
  geom_bar(stat="identity",position="dodge") +
  PlotTheme +
  # coord_cartesian(ylim=c(0, 40)) +
  labs(title = paste(SiteID, "- Consec. Days/Yr >", HotTemp, "Deg in Historical (1950-2005) vs.", Year), 
            x = "Historical & Future Climate Scenarios", y = "Days/Yr", colour = "Climate Future")  +
  scale_fill_manual(name="",values = colors3)

ggsave(sprintf("%s_%s_%s_Consecutive Days_Over_HotTemp.png", SiteID, Lat, Lon), width = PlotWidth, height = PlotHeight)


    # create dataset for bar graph of total number of days/year under cold temperature threshold
TUColdTempmean<-ddply(TUColdTemp3, "CF", summarise,
                 ColdDays=mean(ColdDays))
newrow <- data.frame( "CF" = character(), "ColdDays" = numeric(), stringsAsFactors=FALSE)
newrow[nrow(newrow) + 1, ] <- c( "Historical", mean(TUColdTempH$ColdDays))
TUColdTempmean<-rbind(TUColdTempmean, newrow)
TUColdTempmean$CF<-factor(TUColdTempmean$CF,levels=c("Historical",Scenario1, "Central", Scenario2), ordered=is.ordered(TUColdTempmean$CF))
TUColdTempmean$ColdDays<-as.numeric(TUColdTempmean$ColdDays)
rm(newrow)

      # Bar graph of total number of days/year over cold temperature threshold
ggplot(TUColdTempmean, aes(x=CF,y=ColdDays,fill=CF)) +
  geom_bar(stat="identity",position="dodge") +
  PlotTheme +
    #  coord_cartesian(ylim=c(30, 65)) +
  labs(title = paste(SiteID, "- Days/Yr <", ColdTemp, "Deg in Historical (1950-2005) vs.", Year), 
            x = "Historical & Future Climate Scenarios", y = "Days/Yr", colour = "Climate Future")  +
  scale_fill_manual(name="",values = colors3)

ggsave(sprintf("%s_%s_%s_Days_Under_ColdTemp.png", SiteID, Lat, Lon), width = PlotWidth, height = PlotHeight)


  #Create data frame for days/year under historic 5th percentile value
TULowQmean<-ddply(TULowQ3, "CF", summarise,
                      LowQDays=mean(LowQDays))
newrow <- data.frame( "CF" = character(), "LowQDays" = numeric(), stringsAsFactors=FALSE)
newrow[nrow(newrow) + 1, ] <- c( "Historical", Hist_Total_LowQ_Days)
TULowQmean<-rbind(TULowQmean, newrow)
TULowQmean$CF<-factor(TULowQmean$CF,levels=c("Historical",Scenario1, "Central", Scenario2), ordered=is.ordered(TULowQmean$CF))
TULowQmean$LowQDays<-as.numeric(TULowQmean$LowQDays)

      # Bar graph of days/year with Tmin under historic 5th percentile value
ggplot(TULowQmean, aes(x=CF, y=LowQDays, fill=CF)) +
  geom_bar(stat="identity", position="dodge") +
  PlotTheme +
  labs(title = paste0(SiteID, " - Days/Yr with Tmin < Historic ", round(QuantileLow*100, 0), "th Percentile (", round(HistTminLow, 1), " °F) \nin Historical (1950-2005) vs. ", Year, sep=""),
            x = "Historical & Future Climate Scenarios", y = "Days/Yr", colour = "Climate Future") +
  scale_fill_manual(name="",values = colors3)

ggsave(sprintf("%s_%s_%s_Days_Under_LowPercentile.png", SiteID, Lat, Lon), width = PlotWidth, height = PlotHeight)

  #Create data frame for days/year over historic 95th percentile value
TOHighQmean<-ddply(TOHighQ3, "CF", summarise,
                           HighQDays=mean(HighQDays))
newrow <- data.frame( "CF" = character(), "HighQDays" = numeric(), stringsAsFactors=FALSE)
newrow[nrow(newrow) + 1, ] <- c( "Historical", Hist_Total_HighQ_Days)
TOHighQmean<-rbind(TOHighQmean, newrow)
TOHighQmean$CF<-factor(TOHighQmean$CF,levels=c("Historical",Scenario1, "Central", Scenario2), ordered=is.ordered(TOHighQmean$CF))
TOHighQmean$HighQDays<-as.numeric(TOHighQmean$HighQDays)

      # Bar graph of days/year with Tmin under historic 5th percentile value
ggplot(TOHighQmean, aes(x=CF, y=HighQDays, fill=CF)) +
  geom_bar(stat="identity", position="dodge") +
  PlotTheme +
  labs(title = paste0(SiteID, " - Days/Yr with Tmax > Historic ", round(QuantileHigh*100, 0), "th Percentile (", round(HistTmaxHigh, 1), " °F) \nin Historical (1950-2005) vs. ", Year, sep=""),
            x = "Historical & Future Climate Scenarios", y = "Days/Yr", colour = "Climate Future") +
  scale_fill_manual(name="",values = colors3)

ggsave(sprintf("%s_%s_%s_Days_Over_HighPercentile.png", SiteID, Lat, Lon), width = PlotWidth, height = PlotHeight)

###Scatter plot showing delta precip and tavg, color by emissions scenario, with box for all 3 CF's
if(grepl("Warm", Scenario1)){
  xmin1 <- Tavg0
  xmax1 <- Tavg
  xmin2 <- Tavg
  xmax2 <- Tavg100
  col1 <- "blue"
  col2 <- "red"
}else{ 
  xmin1 <- Tavg
  xmax1 <- Tavg100
  xmin2 <- Tavg0
  xmax2 <- Tavg
  col1 <- "red"
  col2 <- "blue"
}
if(grepl("Dry", Scenario1) | grepl("Moist", Scenario1)){
  ymin1 <- 365*Pr0
  ymax1 <- 365*PrAvg
  ymin2 <- 365*PrAvg
  ymax2 <- 365*Pr100
}else{
  ymin1 <- 365*PrAvg
  ymax1 <- 365*Pr100
  ymin2 <- 365*Pr0
  ymax2 <- 365*PrAvg
}

scatter = ggplot(Future_Means, aes(DeltaTavg, 365*DeltaPr))
scatter + geom_point(aes(color=emissions),size=4) + 
  PlotTheme + 
  labs(title = paste(SiteID, "- Changes in climate means in", Year,"by GCM run"), 
            x = "Change in annual average temperature (F)", 
            y = "Change in average annual precipitation (in)") +
  scale_colour_manual(values=col.RCP2)+
  guides(color=guide_legend(title="Emissions\nScenarios\n")) +
  geom_rect(xmin=xmin1, xmax=xmax1, ymin=ymin1, ymax=ymax1, color = col1, alpha=0, size=1) + 
  geom_rect(xmin=Tavg25, xmax=Tavg75, ymin=365*Pr25, ymax=365*Pr75, color = "yellow", alpha=0, size=1) +
  geom_rect(xmin=xmin2, xmax=xmax2, ymin=ymin2, ymax=ymax2, color = col2, alpha=0, size=1) +
  geom_hline(aes(yintercept=365*mean(Future_Means$DeltaPr)),linetype=2) + 
  geom_vline(aes(xintercept=mean(Future_Means$DeltaTavg)),linetype=2)  
#scale_y_continuous(limits=c(-3.75,3.75))

ggsave(sprintf("%s_%s_%s_GCM_Scatter_Plot_3CFs.png", SiteID, Lat, Lon), width = PlotWidth, height = PlotHeight)

############################################################################

###### SUMMARY TABLE CALCULATIONS
# Create Summary Table for calculations with text for each variable and season
Summary.Table<-setNames(data.frame(matrix(ncol = 7, nrow = 14)), c("Var","Season","Historical",paste(Scenario1,"",sep=" "),
                                                                   paste(Scenario1,"% Delta",sep=" "),paste(Scenario2,"",sep=" "),paste(Scenario2,"% Delta",sep=" ")))
Summary.Table$Var<-c("Seasonal avg. daily Max Temp (°F)","","","","Seasonal avg. daily Min Temp (°F)","","","",
                     paste("Avg total days/yr >", HotTemp, "°F"), paste("Avg total days/yr <", ColdTemp, "°F"),"Seasonal Precipitation (in)","","","")
Summary.Table$Season<-c("W","Sp","Su","F","W","Sp","Su","F","","","W","Sp","Su","F")

Historical_Tmax = aggregate(TmaxCustom ~ season, data=Baseline_all, FUN=mean)

#Historical Seasonal data
### Create dataframe of historical seasonal data
Historical_TMax= data.frame(months,with(Baseline_all, tapply(TmaxCustom, list(Date$mon), mean)));Historical_TMax=melt(Historical_TMax)
names(Historical_TMax)=c("months","var","TMax")
Historical_TMin= data.frame(months,with(Baseline_all, tapply(TminCustom, list(Date$mon), mean)));Historical_TMin=melt(Historical_TMin)
names(Historical_TMin)=c("months","var","TMin")
Historical_Precip= data.frame(months,with(Baseline_all, tapply(PrecipCustom, list(Date$mon), mean))*30);Historical_Precip=melt(Historical_Precip)
names(Historical_Precip)=c("months","var","Precip")

#Historical Tmax - Create variable that subsets historical Tmax data by quarter
WHTmax<-subset(Historical_TMax,months=="December"|months=="January"|months=="February");WHTmax<-tapply(WHTmax$TMax,WHTmax$var,mean)
SpHTmax<-subset(Historical_TMax,months=="March"|months=="April"|months=="May");SpHTmax<-tapply(SpHTmax$TMax,SpHTmax$var,mean)
SuHTmax<-subset(Historical_TMax,months=="June"|months=="July"|months=="August");SuHTmax<-tapply(SuHTmax$TMax,SuHTmax$var,mean)
FHTmax<-subset(Historical_TMax,months=="September"|months=="October"|months=="November");FHTmax<-tapply(FHTmax$TMax,FHTmax$var,mean)

#Historical Tmin - Create variable that subsets historical Tmin data by quarter
WHTmin<-subset(Historical_TMin,months=="December"|months=="January"|months=="February");WHTmin<-tapply(WHTmin$TMin,WHTmin$var,mean)
SpHTmin<-subset(Historical_TMin,months=="March"|months=="April"|months=="May");SpHTmin<-tapply(SpHTmin$TMin,SpHTmin$var,mean)
SuHTmin<-subset(Historical_TMin,months=="June"|months=="July"|months=="August");SuHTmin<-tapply(SuHTmin$TMin,SuHTmin$var,mean)
FHTmin<-subset(Historical_TMin,months=="September"|months=="October"|months=="November");FHTmin<-tapply(FHTmin$TMin,FHTmin$var,mean)

# Days over hot temperature
HHotTemp<-subset(TOHotTempmean,CF=='Historical')

# Days under cold temperature
HColdTemp<-subset(TUColdTempmean,CF=='Historical')

#Historical Precip -- a sum of the monthly precip for each season, not average of all months
WHP<-subset(Historical_Precip,months=="December"|months=="January"|months=="February");WHP<-tapply(WHP$Precip,WHP$var,sum)
SpHP<-subset(Historical_Precip,months=="March"|months=="April"|months=="May");SpHP<-tapply(SpHP$Precip,SpHP$var,sum)
SuHP<-subset(Historical_Precip,months=="June"|months=="July"|months=="August");SuHP<-tapply(SuHP$Precip,SuHP$var,sum)
FHP<-subset(Historical_Precip,months=="September"|months=="October"|months=="November");FHP<-tapply(FHP$Precip,FHP$var,sum)

#max annual avg drought length
DM3mean

Historical<-c(WHTmax,SpHTmax,SuHTmax,FHTmax,WHTmin,SpHTmin,SuHTmin,FHTmin,HHotTemp$HotDays,
              HColdTemp$ColdDays,WHP,SpHP,SuHP,FHP)

Summary.Table$Historical<-round(Historical,digits=2)

#Creat dataframes for each variable from future dataset by season
Future_Tmax<-data.frame(months,with(Future_all,tapply(TmaxCustom, list(Date$mon, CF), mean))); Future_Tmax=melt(Future_Tmax)
names(Future_Tmax)<-c("months","CF","Tmax"); Future_Tmax$CF<-gsub('([[:punct:]])|\\s+',' ',Future_Tmax$CF)
#Subset future Tmax by season
WTmax<-subset(Future_Tmax,months=="December"|months=="January"|months=="February")
SpTmax<-subset(Future_Tmax,months=="March"|months=="April"|months=="May")
SuTmax<-subset(Future_Tmax,months=="June"|months=="July"|months=="August")
FTmax<-subset(Future_Tmax,months=="September"|months=="October"|months=="November")
#Summarize by scenario
#Tmax - Scenario 2
WTmax.ab<-ddply(WTmax, "CF", summarise, mean=mean(Tmax))
SpTmax.ab<-ddply(SpTmax, "CF", summarise, mean=mean(Tmax))
SuTmax.ab<-ddply(SuTmax, "CF", summarise, mean=mean(Tmax))
FTmax.ab<-ddply(FTmax, "CF", summarise, mean=mean(Tmax))

#Create dataframe and subset Future Tmin by season
Future_Tmin<-data.frame(months,with(Future_all,tapply(TminCustom, list(Date$mon, CF), mean))); Future_Tmin=melt(Future_Tmin)
names(Future_Tmin)<-c("months","CF","Tmin"); Future_Tmin$CF<-gsub('([[:punct:]])|\\s+',' ',Future_Tmin$CF)   
WTmin<-subset(Future_Tmin,months=="December"|months=="January"|months=="February")
SpTmin<-subset(Future_Tmin,months=="March"|months=="April"|months=="May")
SuTmin<-subset(Future_Tmin,months=="June"|months=="July"|months=="August")
FTmin<-subset(Future_Tmin,months=="September"|months=="October"|months=="November")
#Summarize by scenario
WTmin.ab<-ddply(WTmin, "CF", summarise, mean=mean(Tmin))
SpTmin.ab<-ddply(SpTmin, "CF", summarise, mean=mean(Tmin))
SuTmin.ab<-ddply(SuTmin, "CF", summarise, mean=mean(Tmin))
FTmin.ab<-ddply(FTmin, "CF", summarise, mean=mean(Tmin))

#Create dataframe and subset Future Precip by season (=daily mean*30)
Future_Precip= data.frame(months,with(Future_all, tapply(PrecipCustom, list(Date$mon, CF), mean))*30);Future_Precip=melt(Future_Precip)
names(Future_Precip)<-c("months","CF","Precip"); Future_Precip$CF<-gsub('([[:punct:]])|\\s+',' ',Future_Precip$CF)
WPrecip<-subset(Future_Precip,months=="December"|months=="January"|months=="February")
SpPrecip<-subset(Future_Precip,months=="March"|months=="April"|months=="May")
SuPrecip<-subset(Future_Precip,months=="June"|months=="July"|months=="August")
FPrecip<-subset(Future_Precip,months=="September"|months=="October"|months=="November")
#Summarize by scenario
WPrecip.ab<-ddply(WPrecip,"CF",summarise,sum=sum(Precip))
SpPrecip.ab<-ddply(SpPrecip,"CF",summarise,sum=sum(Precip))
SuPrecip.ab<-ddply(SuPrecip,"CF",summarise,sum=sum(Precip))
FPrecip.ab<-ddply(FPrecip,"CF",summarise,sum=sum(Precip))

# Fill in Scenario 1 Absolute Values for tables
#Tmax - mean seasonal Tmax for scenario 1
WTmax.ab1<-WTmax.ab$mean[WTmax.ab$CF==Scenario1]
SpTmax.ab1<-SpTmax.ab$mean[SpTmax.ab$CF==Scenario1]
SuTmax.ab1<-SuTmax.ab$mean[SuTmax.ab$CF==Scenario1]
FTmax.ab1<-FTmax.ab$mean[FTmax.ab$CF==Scenario1]
#Tmin - mean seasonal Tmin for scenario 1
WTmin.ab1<-WTmin.ab$mean[WTmin.ab$CF==Scenario1]
SpTmin.ab1<-SpTmin.ab$mean[SpTmin.ab$CF==Scenario1]
SuTmin.ab1<-SuTmin.ab$mean[SuTmin.ab$CF==Scenario1]
FTmin.ab1<-FTmin.ab$mean[FTmin.ab$CF==Scenario1]
#Precip - Seasonal total Precip for Scenario 1 (sum of monthly mean for season)
WPrecip.ab1<-WPrecip.ab$sum[WPrecip.ab$CF==Scenario1]
SpPrecip.ab1<-SpPrecip.ab$sum[SpPrecip.ab$CF==Scenario1]
SuPrecip.ab1<-SuPrecip.ab$sum[SuPrecip.ab$CF==Scenario1]
FPrecip.ab1<-FPrecip.ab$sum[FPrecip.ab$CF==Scenario1]

#Fill in Table
Ab1<-c(WTmax.ab1,SpTmax.ab1,SuTmax.ab1,FTmax.ab1,WTmin.ab1,SpTmin.ab1,SuTmin.ab1,FTmin.ab1,
       TOHotTempmean$HotDays[TOHotTempmean$CF==Scenario1],TUColdTempmean$ColdDays[TUColdTempmean$CF==Scenario1],
       WPrecip.ab1,SpPrecip.ab1,SuPrecip.ab1,FPrecip.ab1)

Summary.Table[[paste(Scenario1,"",sep=" ")]]<-round(Ab1,digits=2)
# Percent change Calculations
#TMax %change
WXP1<-((WTmax.ab1-WHTmax)/WHTmax)*100
SpXP1<-((SpTmax.ab1-SpHTmax)/SpHTmax)*100
SuXP1<-((SuTmax.ab1-SuHTmax)/SuHTmax)*100
FXP1<-((FTmax.ab1-FHTmax)/FHTmax)*100

#TMin %change
WNP1<-((WTmin.ab1-WHTmin)/WHTmin)*100
SpNP1<-((SpTmin.ab1-SpHTmin)/SpHTmin)*100
SuNP1<-((SuTmin.ab1-SuHTmin)/SuHTmin)*100
FNP1<-((FTmin.ab1-FHTmin)/FHTmin)*100

# Seasonal Precip delta
WPP1<-((WPrecip.ab1-WHP)/WHP)*100
SpPP1<-((SpPrecip.ab1-SpHP)/SpHP)*100
SuPP1<-((SuPrecip.ab1-SuHP)/SuHP)*100
FPP1<-((FPrecip.ab1-FHP)/FHP)*100

#Days >hot temp. threshold %change
TOHotTempP<-ddply(TOHotTemp3,"CF",summarise,mean=mean(HotDays))
TOHotTempP<-TOHotTempP$mean[TOHotTempP$CF==Scenario1]
TOHotTempPerc1<-((TOHotTempP - HHotTemp$HotDays)/HHotTemp$HotDays)*100

#Days <cold temp threshold %change
TUColdTempPerc<-ddply(TUColdTemp3,"CF",summarise,mean=mean(ColdDays))
TUColdTempPerc<-TUColdTempPerc$mean[TUColdTempPerc$CF==Scenario1]
TUColdTempPerc1<-((TUColdTempPerc-HColdTemp$ColdDays)/HColdTemp$ColdDays)*100

Ab1delta<-c(WXP1,SpXP1,SuXP1,FXP1,WNP1,SpNP1,SuNP1,FNP1,
            TOHotTempPerc1,TUColdTempPerc1,WPP1,SpPP1,SuPP1,FPP1)

Summary.Table[[paste(Scenario1,"% Delta",sep=" ")]]<-round(Ab1delta,digits=2)

# Fill in Scenario 2 Absolute Values for tables
#Tmax - mean seasonal Tmax for scenario 1
WTmax.ab2<-WTmax.ab$mean[WTmax.ab$CF==Scenario2]
SpTmax.ab2<-SpTmax.ab$mean[SpTmax.ab$CF==Scenario2]
SuTmax.ab2<-SuTmax.ab$mean[SuTmax.ab$CF==Scenario2]
FTmax.ab2<-FTmax.ab$mean[FTmax.ab$CF==Scenario2]
#Tmin - mean seasonal Tmin for scenario 1
WTmin.ab2<-WTmin.ab$mean[WTmin.ab$CF==Scenario2]
SpTmin.ab2<-SpTmin.ab$mean[SpTmin.ab$CF==Scenario2]
SuTmin.ab2<-SuTmin.ab$mean[SuTmin.ab$CF==Scenario2]
FTmin.ab2<-FTmin.ab$mean[FTmin.ab$CF==Scenario2]
#Precip - Seasonal total Precip for Scenario 1 (sum of monthly mean for season)
WPrecip.ab2<-WPrecip.ab$sum[WPrecip.ab$CF==Scenario2]
SpPrecip.ab2<-SpPrecip.ab$sum[SpPrecip.ab$CF==Scenario2]
SuPrecip.ab2<-SuPrecip.ab$sum[SuPrecip.ab$CF==Scenario2]
FPrecip.ab2<-FPrecip.ab$sum[FPrecip.ab$CF==Scenario2]

#Fill in Table
Ab2<-c(WTmax.ab2,SpTmax.ab2,SuTmax.ab2,FTmax.ab2,WTmin.ab2,SpTmin.ab2,SuTmin.ab2,FTmin.ab2,
       TOHotTempmean$HotDays[TOHotTempmean$CF==Scenario2],TUColdTempmean$ColdDays[TUColdTempmean$CF==Scenario2],
       WPrecip.ab2,SpPrecip.ab2,SuPrecip.ab2,FPrecip.ab2)

Summary.Table[[paste(Scenario2,"",sep=" ")]]<-round(Ab2,digits=2)
# Percent change Calculations
#TMax %change
WXP2<-((WTmax.ab2-WHTmax)/WHTmax)*100
SpXP2<-((SpTmax.ab2-SpHTmax)/SpHTmax)*100
SuXP2<-((SuTmax.ab2-SuHTmax)/SuHTmax)*100
FXP2<-((FTmax.ab2-FHTmax)/FHTmax)*100

#TMin %change
WNP2<-((WTmin.ab2-WHTmin)/WHTmin)*100
SpNP2<-((SpTmin.ab2-SpHTmin)/SpHTmin)*100
SuNP2<-((SuTmin.ab2-SuHTmin)/SuHTmin)*100
FNP2<-((FTmin.ab2-FHTmin)/FHTmin)*100

# Seasonal Precip delta
WPP2<-((WPrecip.ab2-WHP)/WHP)*100
SpPP2<-((SpPrecip.ab2-SpHP)/SpHP)*100
SuPP2<-((SuPrecip.ab2-SuHP)/SuHP)*100
FPP2<-((FPrecip.ab2-FHP)/FHP)*100

#Days >100 deg %change
TOHotTempP<-ddply(TOHotTemp3,"CF",summarise,mean=mean(HotDays))
TOHotTempP<-TOHotTempP$mean[TOHotTempP$CF==Scenario2]
TOHotTempPerc2<-((TOHotTempP - HHotTemp$HotDays)/HHotTemp$HotDays)*100

#Days <32 deg %change
TUColdTempPerc<-ddply(TUColdTemp3,"CF",summarise,mean=mean(ColdDays))
TUColdTempPerc<-TUColdTempPerc$mean[TUColdTempPerc$CF==Scenario2]
TUColdTempPerc2<-((TUColdTempPerc-HColdTemp$ColdDays)/HColdTemp$ColdDays)*100

Ab2delta<-c(WXP2,SpXP2,SuXP2,FXP2,WNP2,SpNP2,SuNP2,FNP2,
            TOHotTempPerc2,TUColdTempPerc2,WPP2,SpPP2,SuPP2,FPP2)

Summary.Table[[paste(Scenario2,"% Delta",sep=" ")]]<-round(Ab2delta,digits=2)

write.csv(Summary.Table, paste(SiteID, "SummaryTable.csv"))

###PROGRAM COMPLETE###

