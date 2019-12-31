# RSS_Plotting vxx.R

# v01.4 Fixed missing line plots for Avg_Monthly_Tmin_Delta and Avg_Monthly_Tmax_Delta by removing fill parameter on geom_line. Line color is already specified by aes(colour=CF)
# v01.3 Added GCM scatter w/ GCMs labelled. Revised plot titles to read Year.
# v01.2 Added a scatter w/o box, darker/larger axis labels
# v01.1 STABLE 22 Oct 2016 - added plots for below cold. Ref lines fixed. Scales OK, SiteID to plot titles & file names
# v01 18 Oct 2015 - stable, writes to /figs.  Vertical plot scale issues not all sorted out.
#                 - in 4-panel drought plot, all ref lines show on all plots (needs fixing)


library(ggplot2)
library(RColorBrewer)

rm(list=ls())

setwd("~/RSS/Parks/GUCO/Figs CMIP5")
load("GUCO_36.1875_-79.8125_Final_Environment.RData")


######### INITIALS #########

##Color schemes

#Colors for CF values plotted side by side (match order of CFs vector)
col.CF5 = rev(brewer.pal(5,"Spectral"))  #Can replace with another color pallette, or a manual color vector

#Colors for RCP 4.5, RCP 8.5
col.RCP2 = c("blue", "red")

##Plot parameters

#Height and width 
PlotWidth = 15
PlotHeight = 9

#ggplot theme to control formatting parameters for plots with month on the x-axis
PlotTheme = theme(axis.text=element_text(size=20),                                                                      #Text size for axis tick mark labels
                    axis.title.x=element_text(size=24, hjust=0.5, margin=margin(t=20, r=20, b=20, l=20)),               #Text size and alignment for x-axis label
                    axis.title.y=element_text(size=24, vjust=0.5,  margin=margin(t=20, r=20, b=20, l=20)),              #Text size and alignment for y-axis label
                    plot.title=element_text(size=26,face="bold",hjust=0.5, margin=margin(t=20, r=20, b=20, l=20)),      #Text size and alignment for plot title
                    legend.title=element_text(size=24),                                                                 #Text size of legend category labels
                    legend.text=element_text(size=20),                                                                  #Text size of legend title
                    legend.position = "right")                                                                          #Legend position

#X-axis labels for monthly plots
MonthLabels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")
                 
######## END INITIALS ########

###Scatter plot showing delta precip and tavg, color by emissions scenario, with box for central CF
scatter = ggplot(Future_Means, aes(DeltaTavg, 365*DeltaPr, xmin=Tavg25, xmax=Tavg75, ymin=365*Pr25, ymax=365*Pr75))
scatter + geom_point(aes(color=emissions),size=4) + 
  PlotTheme + 
  labs(list(title = paste(SiteID, "- Changes in climate means in", Year,"by GCM run"), 
            x = "Change in annual average temperature (F)", 
            y = "Change in average annual precipitation (in)")) +
  scale_colour_manual(values=c("blue", "red"))+
  guides(color=guide_legend(title="Emissions\nScenarios\n")) +
  geom_rect(color = "blue", alpha=0) + 
  geom_hline(aes(yintercept=365*mean(Future_Means$DeltaPr)),linetype=2) + 
  geom_vline(aes(xintercept=mean(Future_Means$DeltaTavg)),linetype=2)  
#scale_y_continuous(limits=c(-3.75,3.75))

ggsave(sprintf("%s_%s_%s_GCM_Scatter_Plot.png", SiteID, Lat, Lon), width = PlotWidth, height = PlotHeight)

###Scatter plot showing delta precip and tavg, color by emissions scenario, x-axis scaled 0-max
scatter = ggplot(Future_Means, aes(DeltaTavg, 365*DeltaPr, xmin=Tavg25, xmax=Tavg75, ymin=365*Pr25, ymax=365*Pr75))
scatter + geom_point(aes(color=emissions),size=4) + 
  PlotTheme + 
  labs(list(title = paste(SiteID, "- Changes in climate means in", Year,"by GCM run"), 
            x = "Change in annual average temperature (F)", 
            y = "Change in average annual precipitation (in)")) +
  scale_colour_manual(values=col.RCP2)+
  guides(color=guide_legend(title="Emissions\nScenarios\n")) +
  geom_point(aes(x=mean(DeltaTavg), y=mean(365*DeltaPr)), shape=23, size=10, fill='black', colour='black') +
  scale_x_continuous(limits=c(0, max(Future_Means$DeltaTavg)+.25))

ggsave(sprintf("%s_%s_%s_GCM_Scatter_noBox_Plot.png", SiteID, Lat, Lon), width = PlotWidth, height = PlotHeight)

#  scatter plots with GCM name identifying points. For all, and separate 4.5 and 8.5 plots

png(filename = sprintf("%s_%s_%s_GCM_Scatter_8.5-4.5 w GCM labels.png", SiteID, Lat, Lon), width = 1280, height = 1280)
plot(Future_Means$DeltaTavg, 365*Future_Means$DeltaPr, pch=20, main=paste("RCP 4.5 & 8.5", Year), 
            xlab="Delta T Avg", ylab="Delta Prcip (in/yr)", cex.axis=1.5, cex.lab=1.5)
text(365*DeltaPr ~ DeltaTavg,data=Future_Means,subset = emissions == "RCP 4.5", col="blue",
      labels=Future_Means$GCM, pos=3, cex=1.5)
text(365*DeltaPr ~ DeltaTavg,data=Future_Means, subset = emissions == "RCP 8.5", col="red",
     labels=Future_Means$GCM, pos=3, cex=1.5)
dev.off()

png(filename = sprintf("%s_%s_%s_GCM_Scatter_4.5 w GCM labels.png", SiteID, Lat, Lon), width = 1280, height = 1280)
plot(365*DeltaPr ~ DeltaTavg, data=Future_Means, subset = emissions == "RCP 4.5", pch=20, main= paste("RCP 4.5", Year), 
     xlab="Delta T Avg", ylab="Delta Prcip (in/yr)", cex.axis=1.5, cex.lab=1.5)
text(365*DeltaPr ~ DeltaTavg, data=Future_Means, subset = emissions == "RCP 4.5", label=GCM, pos=3, cex=1.5, col="blue")
dev.off()

png(filename = sprintf("%s_%s_%s_GCM_Scatter_8.5 w GCM labels.png", SiteID, Lat, Lon), width = 1280, height = 1280)
plot(365*DeltaPr ~ DeltaTavg, data=Future_Means, subset = emissions == "RCP 8.5",
     pch=20, main=paste("RCP 8.5", Year), xlab="Delta T Avg", ylab="Delta Prcip (in/yr)", cex.axis=1.5, cex.lab=1.5)
text(365*DeltaPr ~ DeltaTavg, data=Future_Means, subset = emissions == "RCP 8.5", label=GCM, pos=3, cex=1.5, col="red")
dev.off()


#Bar graph of monthly precip by CF
ggplot(Monthly_Precip_delta, aes(x=month,y=30*Precip,fill=CF)) +
  geom_bar(stat="identity",position="dodge") +
  PlotTheme +
  labs(list(title = paste(SiteID, "- Change in average monthly precipitation in", Year,"vs 1950-1999"), 
          x = "Month", y = "Change in Precipitation (in)")) +
  scale_fill_manual(name="Climate Future",values = col.CF5) + 
  scale_x_discrete(labels = MonthLabels)

ggsave(sprintf("%s_%s_%s_Avg_Monthly_Precip_Delta_Bar.png", SiteID, Lat, Lon), width = PlotWidth, height = PlotHeight)

#Bar graph of seasonal precip by CF
ggplot(Seasonal_Precip_delta, aes(x=Season,y=30*Precip,fill=CF)) +
  geom_bar(stat="identity",position="dodge") +
  PlotTheme +
  labs(list(title = paste(SiteID, "- Change in average seasonal precipitation in", Year,"vs 1950-1999"), 
            x = "Season", y = "Change in Precipitation (in)")) +
  scale_fill_manual(name="Climate Future",values = col.CF5)

ggsave(sprintf("%s_%s_%s_Avg_Seasonal_Precip_Delta_Bar.png", SiteID, Lat, Lon), width = PlotWidth, height = PlotHeight)


#Line plot of change in MaxTemp by CF/month
ggplot(Monthly_Tmax_delta, aes(x=month, y=Tmax, group=CF, colour = CF)) +
  geom_line(size = 2, stat = "identity") + geom_point(shape = 21, size = 5, fill = "white") +
  PlotTheme +
  labs(list(title = paste(SiteID, "- Change in average daily maximum temperature in", Year,"vs 1950-1999"), 
            x = "Month", y = "Deg F")) +
  scale_color_manual(name="Climate Future",values = col.CF5) +
  scale_y_continuous(limits=c(0, ceiling(max(Monthly_Tmax_delta$Tmax)))) + 
  scale_x_discrete(labels = MonthLabels)

ggsave(sprintf("%s_%s_%s_Avg_Monthly_Tmax_Delta_Line.png", SiteID, Lat, Lon), width = PlotWidth, height =PlotHeight)


####Line Plot of change in MinTemp by CF/Month
ggplot(Monthly_Tmin_delta, aes(x=month, y=Tmin, group=CF, colour = CF)) +
  geom_line(size = 2, stat = "identity") + 
  geom_point(shape = 21, size = 5, fill = "white") +
  PlotTheme +
  labs(list(title = paste(SiteID, "- Change in average daily minimum temperature in", Year,"vs 1950-1999"),
            x = "Month", y = "Deg F")) +
  scale_color_manual(name="Climate Future",values = col.CF5) +
  scale_y_continuous(limits=c(0, ceiling(max(Monthly_Tmin_delta$Tmin))))+ 
  scale_x_discrete(labels = MonthLabels)

ggsave(sprintf("%s_%s_%s_Avg_Monthly_Tmin_Delta_Line.png", SiteID, Lat, Lon), width = PlotWidth, height = PlotHeight)


#Box plot of precip, aggregated by month
ggplot(Monthly_Precip_delta, aes(x=month,y=Precip*30)) +
  geom_boxplot(fill="lightblue") + 
  PlotTheme +
  # scale_y_continuous(limits=c(-0.4, 0.4)) +
  geom_hline(aes(yintercept=0)) +
  labs(list(title = paste(SiteID, "- Change in average monthly precipitation in", Year,"vs 1950-1999"), 
            x = "Month", y = "Change in Precipitation (in)", colour = "Climate Future")) + 
  scale_x_discrete(labels = MonthLabels)

ggsave(sprintf("%s_%s_%s_Delta_Precip_by_CF_Box.png", SiteID, Lat, Lon), width = PlotWidth, height = PlotHeight)

#Box plot of precip, aggregated by season
ggplot(Seasonal_Precip_delta, aes(x=Season,y=Precip*30)) +
  geom_boxplot(fill="lightblue") + 
  PlotTheme +
  # scale_y_continuous(limits=c(-0.4, 0.4)) +
  geom_hline(aes(yintercept=0)) +
  labs(list(title = paste(SiteID, "- Change in average seasonal precipitation in", Year,"vs 1950-1999"), 
            x = "Month", y = "Change in Precipitation (in)", colour = "Climate Future")) 

ggsave(sprintf("%s_%s_%s_Delta_Precip_by_CF_by_Season_Box.png", SiteID, Lat, Lon), width = PlotWidth, height = PlotHeight)


#Box plot of temp, aggregated
ggplot(Monthly_Tmax_delta, aes(x=month,y=Tmax)) +
  geom_boxplot(fill="orange") + 
  PlotTheme +
  # scale_y_continuous(limits=c(0, 6)) +
  labs(list(title = paste(SiteID, "- Change in average daily maximum temperature in", Year),
          x = "Month", y = "Average Daily Maximum Temperature (F)", colour = "Climate Future")) +
  scale_x_discrete(labels = MonthLabels)

ggsave(sprintf("%s_%s_%s_Avg_Monthly_Tmax_Delta_Box.png", SiteID, Lat, Lon), width = PlotWidth, height = PlotHeight)


########## Boxplot of heat wave days (max consecutive days over threshold temperature)

#Max (avg) Total Days Over 100 per year
ggplot(subset(TotalOverHotTemp, timeframe == "Future", select=c("CF", "Adjusted")), aes(x=CF, y=Adjusted)) +
  geom_boxplot(fill="orange") + 
  PlotTheme +
  #scale_y_continuous(limits=c(0, 40)) +
  geom_hline(aes(yintercept=Hist_Total_HotTemp_Days), size = 1) +
  labs(list(title = paste(SiteID, " - Total", HotTemp, "Degree Days in Historical (1950-1999) vs Future (2025-2055)"), 
            x = "Climate Future", y = paste("Days Over", HotTemp,"F per Year"), colour = "Climate Future")) 

ggsave(sprintf("%s_%s_%s_Days_Over_HotTemp.png", SiteID, Lat, Lon), width = PlotWidth, height = PlotHeight)


####Boxplot Max Consecutive Days over HotTemp by CF
ggplot(subset(HeatMax, timeframe == "Future", select=c("CF", "Adjusted")), aes(x=CF, y=Adjusted)) +
  geom_boxplot(fill="darkred") + 
  PlotTheme +
  #scale_y_continuous(limits=c(0, 30)) +
  geom_hline(aes(yintercept=HeatMaxHist), size = 1) +
  labs(list(title = paste(SiteID, "Max Consecutive", HotTemp, "Degree Days in Historical (1950-1999) vs Future (2025-2055)"), 
            x = "Climate Future", y = paste("Days Over", HotTemp,"F"), colour = "Climate Future")) 

ggsave(sprintf("%s_%s_%s_Consec_Days_Over_HotTemp_Box.png", SiteID, Lat, Lon), width = PlotWidth, height = PlotHeight)


########## Boxplots of cold days (total & max consecutive days below threshold temperature)

#Max (avg) Total Days Under Cold Tem;p
ggplot(subset(TotalUnderColdTemp, timeframe == "Future", select=c("CF", "Adjusted")), aes(x=CF, y=Adjusted)) +
  geom_boxplot(fill="blue") + 
  PlotTheme +
  geom_hline(aes(yintercept= Hist_Total_ColdTemp_Days), size = 1) +
  labs(list(title = paste(SiteID, " - Total Days Below ", ColdTemp, " F in Historical (1950-1999) vs Future (2025-2055)"), 
            x = "Climate Future", y = paste("Days Below", ColdTemp,"F per Year"), colour = "Climate Future")) 

ggsave(sprintf("%s_%s_%s_Days_Below_ColdTemp.png", SiteID, Lat, Lon), width = PlotWidth, height = PlotHeight)


####Boxplot Max Consecutive Days below Cold Temp by CF
ggplot(subset(ColdMax, timeframe == "Future", select=c("CF", "Adjusted")), aes(x=CF, y=Adjusted)) +
  geom_boxplot(fill="blue") + 
  PlotTheme +
  #scale_y_continuous(limits=c(0, 30)) +
  geom_hline(aes(yintercept=ColdMaxHist), size = 1) +
  labs(list(title = paste(SiteID, "Max Consecutive Days Below ", ColdTemp, " F in Historical (1950-1999) vs", Year), 
            x = "Climate Future", y = paste("Days Belowr", ColdTemp,"F"), colour = "Climate Future")) 

ggsave(sprintf("%s_%s_%s_Consec_Days_Below_Cold_Temp_Box.png", SiteID, Lat, Lon), width = PlotWidth, height = PlotHeight)


#############Boxplot of drought duration
#Baseline and Future drought duration (max consecutive number of days)
ggplot(subset(DroughtMax, timeframe == "Future", select=c("CF", "Adjusted")), aes(x=CF, y=Adjusted)) +
  geom_boxplot(fill="tan") + 
  PlotTheme +
 # scale_y_continuous(limits=c(20, 45)) +
  geom_hline(aes(yintercept=DroughtMax_Hist), size = 1, linetype = "dashed") +
  labs(list(title = paste(SiteID, "- Max Drought Length in Historical (1950-1999) vs", Year),
            x = "Climate Future", y = "Drought Length (Days)", colour = "Climate Future")) 

ggsave(sprintf("%s_%s_%s_Max_Drought_Length_Box.png", SiteID, Lat, Lon), width = PlotWidth, height = PlotHeight)


####Boxplot of Drought Length (Seasonal)
ggplot(subset(DroughtSeasons, (Timeframe == "Future" & (Season == "Winter" |Season == "Spring" |Season == "Summer" |Season == "Fall")),
              select=c("CF","DroughtMax", "Season", "Timeframe")), aes(CF, DroughtMax)) + 
  geom_boxplot(fill = "tan") + 
  PlotTheme + 
  theme(axis.text = element_text(size = 18), strip.text = element_text(size = 24)) +
  labs(list(title = paste(SiteID, "- Max Drought Length in Historical (1950-1999) and", Year), 
            x = "Climate Future", y = "Days per year")) + 
  scale_fill_manual(values = c("tan", "tan4")) + 
  #scale_x_discrete(labels = abbreviate(CFs, minlength = 1, strict = FALSE)) +
  facet_wrap(~Season,ncol = 2) + geom_hline(data = DroughtMaxSeasons_Hist_melt, aes(yintercept = value), linetype = "dashed")  #scale_y_continuous(limits=c(0,35))

ggsave(sprintf("%s_%s_%s_Max_Drought_Length_Season_Box.png", SiteID, Lat, Lon), width = PlotWidth, height = PlotHeight)


#######Box Plot for Max 24-hr Precip
ggplot(subset(PrecipMax, timeframe == "Future", select=c("CF", "AdjustedMaxPrecip", "timeframe")), aes(x=CF, y=AdjustedMaxPrecip)) +
  geom_boxplot(fill = "lightblue") +
  PlotTheme +
 # scale_y_continuous(limits=c(0.5, 1.5)) +
  geom_hline(aes(yintercept=PrecipMax_Hist), size = 1, show.legend = TRUE, linetype = "dashed") +
  labs(list(title = paste(SiteID, "- Maximum 24-Hour Precipitation in Historical (1950-1999) vs", Year), 
            x = "Climate Future", y = "Precipitation (in)", colour = "Climate Future")) 

ggsave(sprintf("%s_%s_%s_Max_24hr_Precip_Box.png", SiteID, Lat, Lon), width = PlotWidth, height = PlotHeight)

setwd(WD)

###PROGRAM COMPLETE###
