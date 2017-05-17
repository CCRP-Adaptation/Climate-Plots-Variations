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

rm(list=ls())
#setwd("/Volumes/Seagate1_Blue2TB/COLM RSS/Figs/")
#setwd("e:/COLM RSS/Figs/")
# setwd('D:/Users/gschuurman/Documents/Planning/Resource Stewardship Strategies/HOBE RSS - Fall 2016 & 2016/HOBE Climate future plots/RSS R scripts_20151022')
# setwd('D:/CHOH RSS/Figs Scenarios2/')
setwd("C:/Users/arcarlson/Documents/RSS Plots/CMIP5_BOR/RMNP")

#load("COLM_39.0625_-108.6875_Final_Environment.RData")
load("RMNP_40.4375_-105.5625_Final_Environment.RData")

TotalOverHotTemp <- TotalOver100

FilePre <- paste(SiteID, Lat, Lon, "CF_", sep="_")

      # Need to check all the subsets
      ### NEED TO CHANGE LINE 72 for ordering scenarios on plots
FutureSubset <- c("Hot Wet","Warm Dry")          # ("Hot Dry", "Central", "Warm Wet")

MP3<-subset(Monthly_Precip_delta, CF %in% FutureSubset)
MP3$month<-factor(MP3$month,levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

DM3<-subset(DroughtMax, CF %in% FutureSubset & timeframe == "Future")
DM3$CF<-factor(DM3$CF,levels=c("Warm Dry", "Central", "Hot Wet"))
DMH<-subset(DroughtMax, timeframe == "Historical")

Tmax3<-subset(Monthly_Tmax_delta, CF %in% FutureSubset)
Tmax3$month<-factor(Tmax3$month,levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

Tmin3<-subset(Monthly_Tmin_delta, CF %in% FutureSubset)
Tmin3$month<-factor(Tmin3$month,levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

TOHotTempH<-subset(TotalOverHotTemp, timeframe == "Historical")
TOHotTemp3<-subset(TotalOverHotTemp, CF %in% FutureSubset & timeframe == "Future")

TCOHotTempH<-subset(HeatMax, timeframe == "Historical")
TCOHotTemp3<-subset(HeatMax, CF %in% FutureSubset)

TUColdTempH<-subset(TotalUnderColdTemp, timeframe == "Historical")
TUColdTemp3<-subset(TotalUnderColdTemp, CF %in% FutureSubset& timeframe == "Future")


###############################################################working############################################


      # set CF order for bar graph of change in average monthly precip by CF 

MP3$CF<-factor(MP3$CF,levels=c("Warm Dry", "Hot Wet"), ordered=is.ordered(MP3$CF))

#Bar graph of change in average monthly precip by CF
ggplot(MP3, aes(x=month,y=30*Precip,fill=CF)) +
  geom_bar(stat="identity",position="dodge") +
  theme(axis.text=element_text(size=16),axis.title.x=element_text(size=20,vjust=-0.2),
        axis.title.y=element_text(size=20,vjust=1.0),
        plot.title=element_text(size=24,face="bold",vjust=2)) +
  labs(title = paste(SiteID, " - Change in average monthly precipitation in 2040 (2025-2055) vs 1950-1999"), 
       x = "Month", y = "Change in Precipitation (in)") +
  scale_fill_manual(name="Climate Future",values = c("#2B83BA","orange","#D7191C"))

ggsave(paste(FilePre,"Avg_Monthly_Precip_Delta_Bar.png", sep=""), width = 15, height = 9)


    # set CF order for bar graph of change in average monthly precip by CF 
Tmax3$CF<-factor(Tmax3$CF,levels=c("Warm Dry", "Central", "Hot Wet"), ordered=is.ordered(Tmax3$CF))

    #Line plot of change in MaxTemp by CF/month
ggplot(Tmax3, aes(x=month, y=Tmax, group=CF, colour = CF)) +
  geom_line(size = 2, stat = "identity") + geom_point(shape = 21, size = 5, fill = "white") +
  theme(axis.text=element_text(size=16),
        axis.title.x=element_text(size=16,vjust=-0.2),
        axis.title.y=element_text(size=20,vjust=1.0),
        plot.title=element_text(size=24,face="bold",vjust=2)) +
  labs(title = paste(SiteID, " - Change in average daily Tmax in 2040 (2025-2055) vs 1950-1999"), 
            x = "Month", y = "Change in Temperature (Degrees F)") +
  scale_color_manual(name="Climate Future",values = c("#2B83BA","orange","#D7191C")) +
  scale_y_continuous(limits=c(0, ceiling(max(Monthly_Tmax_delta$Tmax))))

ggsave(paste(FilePre,"Avg_Monthly_Tmax_Delta_Line.png", sep=""), width = 15, height = 9)


     #set CF order for bar graph of change in average monthly precip by CF 
Tmin3$CF<-factor(Tmin3$CF,levels=c("Warm Dry", "Hot Wet"), ordered=is.ordered(Tmin3$CF))

      ####Line Plot of change in MinTemp by CF/Month
ggplot(Tmin3, aes(x=month, y=Tmin, group=CF, colour = CF)) +
  geom_line(size = 2, stat = "identity") + 
  geom_point(shape = 21, size = 5, fill = "white") +
  theme(axis.text=element_text(size=16),
        axis.title.x=element_text(size=16,vjust=-0.2),
        axis.title.y=element_text(size=20,vjust=1.0),
        plot.title=element_text(size=24,face="bold",vjust=2)) +
  labs(title = paste(SiteID, " - Change in average daily Tmin in 2040 (2025-2055) vs 1950-1999"),
            x = "Month", y = "Change in Temperature (Degrees F)") +
  scale_color_manual(name="Climate Future",values = c("#2B83BA","orange","#D7191C")) +
  scale_y_continuous(limits=c(0, ceiling(max(Monthly_Tmin_delta$Tmin))))

ggsave(paste(FilePre,"Avg_Monthly_Tmin_Delta_Line.png", sep=""), width = 15, height = 9)


        # create dataset for bar plot of drought duration (max consecutive # of days)
DM3mean<-ddply(DM3, "CF", summarise,
                   MeanContinOverHotTemp=mean(DroughtMaxDays))

newrow <- data.frame( "CF" = character(), "MeanContinOverHotTemp" = numeric(), stringsAsFactors=FALSE)
newrow[nrow(newrow) + 1, ] <- c( "Historical", mean(DMH$DroughtMaxDays))

DM3mean<-rbind(DM3mean, newrow)

DM3mean$CF<-factor(DM3mean$CF,levels=c("Historical","Warm Dry", "Central", "Hot Wet"), ordered=is.ordered(DM3mean$CF))

DM3mean$MeanContinOverHotTemp<-as.numeric(DM3mean$MeanContinOverHotTemp)

rm(newrow)


    #Bar graph of future drougth duration (max consecutive # of days) vs. historical mean and CF
ggplot(DM3mean, aes(x=CF,y=MeanContinOverHotTemp,fill=CF)) +
  geom_bar(stat="identity",position="dodge") +
  theme(axis.text=element_text(size=16),axis.title.x=element_text(size=20,vjust=-0.2),
        axis.title.y=element_text(size=20,vjust=0.2),
        plot.title=element_text(size=24,face="bold",vjust=2)) +
  coord_cartesian(ylim=c(0, 20)) +
  labs(title = paste(SiteID, " - Max Annual Avg. Drought Length in Historical (1950-1999) & Future (2025-2055)"), 
       x = "Historical & Future Climate Scenarios", y = "Drought Length (Days)") +
  scale_fill_manual(name="",values = c("dark grey", "blue", "orange","#FFFFBF","#D7191C"))

ggsave(paste(FilePre,"Max_Drought_Length_Bar.png", sep=""), width = 15, height = 9)


      # create dataset for bar graph of total number of days/year over hot temperature threshold
TOHotTempmean<-ddply(TOHotTemp3, "CF", summarise,
               MeanOverHotTemp=mean(Adjusted))

newrow <- data.frame( "CF" = character(), "MeanOverHotTemp" = numeric(), stringsAsFactors=FALSE)
newrow[nrow(newrow) + 1, ] <- c( "Historical", mean(TOHotTempH$Adjusted))

TOHotTempmean<-rbind(TOHotTempmean, newrow)

TOHotTempmean$CF<-factor(TOHotTempmean$CF,levels=c("Historical","Warm Dry", "Central", "Hot Wet"), ordered=is.ordered(TOHotTempmean$CF))

TOHotTempmean$MeanOverHotTemp<-as.numeric(TOHotTempmean$MeanOverHotTemp)

rm(newrow)

      #Bar graph of total number of days/year over hot temperature threshold
ggplot(TOHotTempmean, aes(x=CF,y=MeanOverHotTemp,fill=CF)) +
  geom_bar(stat="identity",position="dodge") +
  theme(axis.text=element_text(size=16),axis.title.x=element_text(size=20,vjust=-0.2),
        axis.title.y=element_text(size=20,vjust=0.8),
        plot.title=element_text(size=24,face="bold",vjust=2)) +
  #coord_cartesian(ylim=c(50, 65)) +
  labs(list(title = paste(SiteID, " - Avg. Tot. Days/Yr > ", HotTemp, "Deg in Historical (1950-1999) & Future (2025-2055)"), 
            x = "Historical & Future Climate Scenarios", y = paste("Days"), colour = "Climate Future"))  +
  scale_fill_manual(name="",values = c("dark grey","blue", "orange","#FFFFBF","#D7191C"))

ggsave(paste(FilePre,"Days_Over_HotTemp.png", sep=""), width = 15, height = 9)


    # creare dataset for  bar graph of total number of CONSECUTIVE days/year over hot temperature threshold
TCOHotTempmean<-ddply(TCOHotTemp3, "CF", summarise,
                 MeanContinOverHotTemp=mean(Adjusted))

newrow <- data.frame( "CF" = character(), "MeanContinOverHotTemp" = numeric(), stringsAsFactors=FALSE)
newrow[nrow(newrow) + 1, ] <- c( "Historical", mean(TCOHotTempH$Adjusted))

TCOHotTempmean<-rbind(TCOHotTempmean, newrow)

TCOHotTempmean$CF<-factor(TCOHotTempmean$CF,levels=c("Historical","Warm Dry", "Central", "Hot Wet"), ordered=is.ordered(TCOHotTempmean$CF))

TCOHotTempmean$MeanContinOverHotTemp<-as.numeric(TCOHotTempmean$MeanContinOverHotTemp)

rm(newrow)

      #Bar graph of total number of CONSECUTIVE days/year over hot temperature threshold
ggplot(TCOHotTempmean, aes(x=CF,y=MeanContinOverHotTemp,fill=CF)) +
  geom_bar(stat="identity",position="dodge") +
  theme(axis.text=element_text(size=16),axis.title.x=element_text(size=20,vjust=-0.2),
        axis.title.y=element_text(size=20,vjust=0.8),
        plot.title=element_text(size=18,face="bold",vjust=2)) +
  # coord_cartesian(ylim=c(0, 40)) +
  labs(list(title = paste(SiteID, " - Consec. Days/Yr > ", HotTemp, "Deg in Historical (1950-1999) & ", Year), 
            x = "Historical & Future Climate Scenarios", y = paste("Days"), colour = "Climate Future"))  +
  scale_fill_manual(name="",values = c("dark grey", "blue","orange","#D7191C"))

ggsave(paste(FilePre,"Consecutive Days_Over_HotTemp.png",sep=""), width = 15, height = 9)


    # creare dataset for bar graph of total number of days/year under cold temperature threshold
TUColdTempmean<-ddply(TUColdTemp3, "CF", summarise,
                 MeanUnderColdTemp=mean(Adjusted))

newrow <- data.frame( "CF" = character(), "MeanUnderColdTemp" = numeric(), stringsAsFactors=FALSE)
newrow[nrow(newrow) + 1, ] <- c( "Historical", mean(TUColdTempH$Adjusted))

TUColdTempmean<-rbind(TUColdTempmean, newrow)

TUColdTempmean$CF<-factor(TUColdTempmean$CF,levels=c("Historical","Warm Dry", "Central", "Hot Wet"), ordered=is.ordered(TUColdTempmean$CF))

TUColdTempmean$MeanUnderColdTemp<-as.numeric(TUColdTempmean$MeanUnderColdTemp)

rm(newrow)

      # Bar graph of total number of days/year over cold temperature threshold
ggplot(TUColdTempmean, aes(x=CF,y=MeanUnderColdTemp,fill=CF)) +
  geom_bar(stat="identity",position="dodge") +
  theme(axis.text=element_text(size=16),axis.title.x=element_text(size=20,vjust=-0.2),
        axis.title.y=element_text(size=18,vjust=0.8),
        plot.title=element_text(size=18,face="bold",vjust=2)) +
    #  coord_cartesian(ylim=c(30, 65)) +
  labs(list(title = paste(
        
        
        
        
        
        
        , " - Days/Yr < ", ColdTemp, "Deg in Historical (1950-1999) & ", Year), 
            x = "Historical & Future Climate Scenarios", y = paste("Days"), colour = "Climate Future"))  +
  scale_fill_manual(name="",values = c("dark grey", "blue", "orange", "#D7191C"))

ggsave(paste(FilePre, "Days_Under_ColdTemp.png", sep=""), width = 15, height = 9)


    ###### Summary differences  ######
    ## Do these calcs only on subset files - non-subset files include historical data!!


    # Days under cold temperature
tapply(TUColdTemp3$Adjusted, TUColdTemp3$CF, mean)
mean(TUColdTempH$Adjusted)
TUColdTempmean
   
    # Days over hot temperature
tapply(TOHotTemp3$Adjusted,TOHotTemp3$CF,mean)
mean(TOHotTempH$Adjusted)
TOHotTempmean


    # Tmin and Tmax deltas
tapply(Tmin3$Tmin,Tmin3$CF,mean)
tapply(Tmax3$Tmax,Tmax3$CF,mean)

    # Precip delta
tapply(MP3$Precip*365,MP3$CF,mean)
AvgHistPrYr <- Avg_Hist_Precip*365
AvgHistPrYr



#setwd(WD_plots)

###PROGRAM COMPLETE###

