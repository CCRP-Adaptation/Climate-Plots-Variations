#############
## Water Balance plots for BIBE
# Uses BOTH monthly and annual data from WB output. One folder for annual and one for monthly,
# each with a .csv of combined output

library(RColorBrewer)
library(ggplot2)
library(plyr)
library(dplyr)
rm(list=ls())

RColorBrewer::display.brewer.all()

rm(list=ls())

setwd('C:/Users/achildress/Desktop/Files/Projects/BIBE-TAR/BIBE_WB/Output/')
annual<-read.csv("annual.csv",header=T)
monthly<-read.csv("monthly_all.csv",header=T)

#### Monthly plots

monthly$SOIL_in<-monthly$SOIL * 0.39
monthly$deficit<-monthly$D * 0.39 *1

monthly$per<-NA
monthly$per[which(monthly$year<2000)]<-"Historical"
monthly$per[which(monthly$year>=2025 & monthly$year<=2055)]<-"future"
monthly<-monthly[-which(is.na(monthly$per)),]

monthly$scenario<-NA
monthly$scenario[which(monthly$per=="Historical")]<-"Historical"
monthly$scenario[which(monthly$per=="future" & monthly$GCM=="inmcm4.rcp85")]<-"warm wet"
monthly$scenario[which(monthly$per=="future" & monthly$GCM=="IPSL-CM5A-MR.rcp85")]<-"hot dry"

H_monthly<-subset(monthly,year<2000)
F_monthly<-subset(monthly,year>=2025 & year<=2055)
H_monthly$scenario<-"Historical"

Hist<-aggregate(cbind(SOIL_in,deficit)~Month+scenario+site,mean,data=H_monthly,na.rm=TRUE)
Hist<-rbind(Hist,Hist)
Fut<-aggregate(cbind(SOIL_in,deficit)~Month+scenario+site,mean,data=F_monthly,na.rm=TRUE)
Monthly<-rbind(Hist,Fut)
Monthly$scenario = factor(Monthly$scenario, levels = unique(Monthly$scenario))


data<-Fut[,1:3]
data$SOIL_IN<-Fut$SOIL_in-Hist$SOIL_in
data$deficit<-Fut$deficit-Hist$deficit
data$ss<-NA
data$ss[which(data$scenario == "hot dry" & data$site == "BIBE-NLM")] <- "NLM - hot dry"
data$ss[which(data$scenario == "warm wet" & data$site == "BIBE-NLM")] <- "NLM - warm wet"
data$ss[which(data$scenario == "hot dry" & data$site == "BIBE-OS")] <- "OS - hot dry"
data$ss[which(data$scenario == "warm wet" & data$site == "BIBE-OS")] <- "OS - warm wet"

# Plot change in spring soil moisture
spring_SM<-subset(data, Month>2 & Month<6)
spring_SM$Month<-month.abb[spring_SM$Month]
spring_SM$Month = factor(spring_SM$Month, levels = c("Mar","Apr","May"))


colors4<-c("red","blue","pink","light blue")
ggplot(spring_SM, aes(x=Month, y=SOIL_IN, group=ss, colour = ss)) +
  geom_line(colour = "black",size=2.5, stat = "identity") + # adds black outline
  geom_line(size = 2, stat = "identity") + 
  geom_point(colour = "black", size = 4, aes(fill = factor(ss), shape = factor(ss))) +
  theme(axis.text=element_text(size=16),
        axis.title.x=element_text(size=16,vjust=-0.2),
        axis.title.y=element_text(size=20,vjust=1.0),
        plot.title=element_text(size=24,face="bold",vjust=2,hjust=0.5),
        legend.text=element_text(size=20)) +
  labs(title = "Change in average monthly spring soil moisture \n 2040 (2025-2055) vs 1950-1999",
       x = "Month", y = "Change in soil moisture (inches)") +
  scale_color_manual(name="",values = colors4) +
  scale_fill_manual(name="",values = colors4) +
  # scale_x_discrete(limits = rev(levels(Month))) +
  scale_shape_manual(name="",values = c(21,22,23,24)) 


ggsave("BIBE Spring Soil Moisture.png", width = 15, height = 9)


# Plot change in summer deficit
summerD<-subset(data, Month>5 & Month<9)
summerD$Month<-month.abb[summerD$Month]
summerD$Month = factor(summerD$Month, levels = c("Jun","Jul","Aug"))

ggplot(summerD, aes(x=Month, y=deficit, group=ss, colour = ss)) +
  geom_line(colour = "black",size=2.5, stat = "identity") + # adds black outline
  geom_line(size = 2, stat = "identity") + 
  geom_point(colour = "black", size = 4, aes(fill = factor(ss), shape = factor(ss))) +
  theme(axis.text=element_text(size=16),
        axis.title.x=element_text(size=16,vjust=-0.2),
        axis.title.y=element_text(size=20,vjust=1.0),
        plot.title=element_text(size=24,face="bold",vjust=2,hjust=0.5),
        legend.text=element_text(size=20)) +
  labs(title = "Change in average summer water deficit \n 2040 (2025-2055) vs 1950-1999",
       x = "Month", y = "Change in water deficit (inches)") +
  scale_color_manual(name="",values = colors4) +
  scale_fill_manual(name="",values = colors4) +
  # scale_x_discrete(limits = rev(levels(Month))) +
  scale_shape_manual(name="",values = c(21,22,23,24)) 

ggsave("BIBE Summer Water Deficit.png", width = 15, height = 9)

### change linetype by site
colors2<-c("red","blue")
# data$Month<-month.abb[data$Month] ### Doesn't work w/ these run
# data$Month = factor(data$Month, levels = unique(data$Month))

ggplot(data, aes(x=Month, y=SOIL_IN, color = scenario, linetype=site)) + geom_line(size=2) +
  scale_colour_manual("scenario",values=colors2) + 
  labs(y = "Soil Moisture (in)", x = "Month", 
       title = "Change in average monthly soil moisture \n 2040 (2025-2055) vs 1950-1999")+ 
  theme(axis.text = element_text(size=20), axis.title = element_text(size=20), legend.text=element_text(size=14),
        plot.title=element_text(size=22, hjust=0.5),legend.position = c(.8,.8))

ggsave("Annual_soil_moisture_change.png", width = 15, height = 9)

ggplot(data, aes(x=Month, y=deficit, color = scenario, linetype=site)) + geom_line(size=2) +
  scale_colour_manual("scenario",values=colors2) + 
  labs(y = "Water deficit(in)", x = "Month", 
       title = "Change in average monthly water deficit \n 2040 (2025-2055) vs 1950-1999")+ 
  theme(axis.text = element_text(size=20), axis.title = element_text(size=20), legend.text=element_text(size=14),
        plot.title=element_text(size=22, hjust=0.5),legend.position = c(.8,.8))

ggsave("Annual_deficit_change.png", width = 15, height = 9)

# Stats
# mean spring soil moist
Monthly_sp<-subset(Monthly, Month >2 & Month<6)
aggregate(SOIL_in~scenario+site,data=Monthly_sp,mean,na.rm=TRUE)

Monthly_su<-subset(Monthly, Month >5 & Month<9)
aggregate(deficit~scenario+site,data=Monthly_su,mean,na.rm=TRUE)


### Redo redo average monthly to be absolutes and include historical average


colors3<-c("black","red","blue")
# Monthly$Month<-month.abb[Monthly$Month]
# Monthly$Month = factor(Monthly$Month, levels = unique(Monthly$Month))

ggplot(Monthly, aes(x=Month, y=SOIL_in, color = scenario, linetype=site)) + geom_line(size=2) +
  scale_colour_manual("scenario",values=colors3) + 
  labs(y = "Soil Moisture (in)", x = "Month", 
       title = "Average monthly soil moisture")+ 
  theme(axis.text = element_text(size=20), axis.title = element_text(size=20), legend.text=element_text(size=14),
        plot.title=element_text(size=22, hjust=0.5),legend.position = c(.8,.8))

ggsave("Annual_soil_moisture.png", width = 15, height = 9)

ggplot(Monthly, aes(x=Month, y=deficit, color = scenario, linetype=site)) + geom_line(size=2) +
  scale_colour_manual("scenario",values=colors3) + 
  labs(y = "Deficit (in)", x = "Month", 
       title = "Average monthly moisture deficit")+ 
  theme(axis.text = element_text(size=20), axis.title = element_text(size=20), legend.text=element_text(size=14),
        plot.title=element_text(size=22, hjust=0.5),legend.position = c(.8,.8))
ggsave("Annual_deficit.png", width = 15, height = 9)

## Try taking monthly data and aggregating to be annual
head(monthly)
a<-aggregate(cbind(SOIL_in,deficit)~Month+year+scenario+site,data=monthly,mean,na.rm=TRUE)
A<-aggregate(cbind(SOIL_in,deficit)~year+scenario+site,data=a,sum,na.rm=TRUE)
aggregate(deficit~scenario+site,data=A,mean,na.rm=TRUE)

NLM<-subset(A,site=="BIBE-NLM")
OS<-subset(A,site=="BIBE-OS")

scol<-c("black","red","blue") # Match to climate futures colors using 
slab<-unique(NLM$scenario)
LT<-c(1,2,3)
NLM.plot <- ggplot(NLM, aes(x=deficit, colour=scenario,fill=scenario),show.legend=F) +
  geom_density(alpha=0.3,size=1.3,aes(linetype=scenario))+ #alpha is transparency
  scale_colour_manual("Scenario",values=scol,labels=slab) + 
  scale_fill_manual("Scenario",values=scol,labels=slab)+ 
  scale_linetype_manual("Scenario",values=LT,labels=slab)+
  labs(y = "Density", x = "Annual water deficit (in)", title = "North Lone Mountain water deficit density plots")+ 
  theme(axis.text = element_text(size=20), axis.title = element_text(size=20), legend.text=element_text(size=14),
        plot.title=element_text(size=22, hjust=0.5),legend.position = c(.8,.8)) +
  annotate(geom="text", x=450, y=.02, label="Drier ->", color="black",size=8)
NLM.plot
ggsave("NLM-density.png", width = 15, height = 9)


slab<-unique(OS$scenario)
OS.plot <- ggplot(OS, aes(x=deficit, colour=scenario,fill=scenario),show.legend=F) +
  geom_density(alpha=0.3,size=1.3,aes(linetype=scenario))+ #alpha is transparency
  scale_colour_manual("Scenario",values=scol,labels=slab) + 
  scale_fill_manual("Scenario",values=scol,labels=slab)+ 
  scale_linetype_manual("Scenario",values=LT,labels=slab)+
  labs(y = "Density", x = "Annual water deficit (in)", title = "Oak Springs water deficit density plots")+ 
  theme(axis.text = element_text(size=20), axis.title = element_text(size=20), legend.text=element_text(size=14),
        plot.title=element_text(size=22, hjust=0.5),legend.position = c(.8,.8)) +
  annotate(geom="text", x=400, y=.02, label="Drier ->", color="black",size=8)
OS.plot
ggsave("OS-density.png", width = 15, height = 9)
