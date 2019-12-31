#To be added to CMIP5_Plot_Table_Creation

#### Create table with monthly precip delta by CF as % change from baseline
Monthly_Precip_delta_percent = data.frame(months,Precip_delta=100*(with(Future_all, tapply(PrecipCustom, list(Date$mon, CF), mean))-
                                                                     with(Baseline_all, tapply(PrecipCustom, list(Date$mon, CF), mean)))/with(Baseline_all, tapply(PrecipCustom, list(Date$mon, CF), mean)))
Monthly_Precip_delta_percent = melt(Monthly_Precip_delta_percent)
names(Monthly_Precip_delta_percent) = c("month","CF","Precip")
Monthly_Precip_delta_percent$CF = factor(Monthly_Precip_delta_percent$CF, 
                                 levels = c("Precip_delta.Warm.Wet", "Precip_delta.Hot.Wet", "Precip_delta.Central", "Precip_delta.Warm.Dry", "Precip_delta.Hot.Dry"), 
                                 labels=c("Warm Wet", "Hot Wet", "Central", "Warm Dry", "Hot Dry"))

MP3<-subset(Monthly_Precip_delta_percent, CF %in% FutureSubset)
MP3$CF<-factor(MP3$CF,levels=c("Warm Dry", "Hot Wet"), ordered=is.ordered(MP3$CF))

#Bar graph of change in average monthly precip by CF
ggplot(MP3, aes(x=month,y=30*Precip,fill=CF)) +
  geom_bar(stat="identity",position="dodge") +
  theme(axis.text=element_text(size=16),axis.title.x=element_text(size=20,vjust=-0.2),
        axis.title.y=element_text(size=20,vjust=1.0),
        plot.title=element_text(size=24,face="bold",vjust=2)) +
  labs(title = "Percent change in average monthly precipitation in 2040 (2025-2055) vs 1950-1999", 
       x = "Month", y = "Change in Precipitation (%)") +
  scale_fill_manual(name="Climate Future",values = c("#2B83BA","orange","#D7191C"))

setwd("~/RSS Plots/WHSA/Figs CMIP5")
ggsave("WHSA_32.8125_-106.3125_CF_Avg_Monthly_Precip_Delta_Percent_Bar.png", width = 15, height = 9)

#Bar graph of precip by CF
ggplot(Monthly_Precip_delta_percent, aes(x=month,y=30*Precip,fill=CF)) +
  geom_bar(stat="identity",position="dodge") +
  theme(axis.text=element_text(size=16),axis.title.x=element_text(size=20,vjust=-0.2),
        axis.title.y=element_text(size=20,vjust=0.2),
        plot.title=element_text(size=24,face="bold",vjust=2)) +
  labs(list(title = paste(SiteID, "- Change in average monthly precipitation in", Year,"vs 1950-1999"), 
            x = "Month", y = "Change in Precipitation (%)")) +
  scale_fill_manual(name="Climate Future",values = rev(brewer.pal(5,"Spectral")))
ggsave("WHSA_32.8125_-106.3125_Avg_Monthly_Precip_Delta_Percent_Bar.png", width = 15, height = 9)

#Box plot of precip, aggregated
ggplot(Monthly_Precip_delta_percent, aes(x=month,y=Precip*30)) +
  geom_boxplot(fill="lightblue") + 
  theme(axis.text=element_text(size=14),
        axis.title.x=element_text(size=16,vjust=-0.2)
        ,axis.title.y=element_text(size=16,vjust=0.2),
        plot.title=element_text(size=18,face="bold",vjust=2)) +
  # scale_y_continuous(limits=c(-0.4, 0.4)) +
  geom_hline(aes(yintercept=0)) +
  labs(list(title = paste(SiteID, "- Change in average monthly precipitation in", Year,"vs 1950-1999"), 
            x = "Month", y = "Change in Precipitation (%)", colour = "Climate Future")) 

ggsave(sprintf("WHSA_32.8125_-106.3125_Percent_Delta_Precip_by_CF_Box.png", SiteID, Lat, Lon), width = 15, height = 9)
