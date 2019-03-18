#############################################################################################################################################
#Script for producing annual water balance output plots from Dave Thoma's model
#Inputs: .csv files containing annual water balance series (one .csv file per GCM/scenario)
#Outputs: Annual deficit vs. AET scatter plot, annual deficit density plot
############################################################################################################################################


library(ggplot2)
library(RColorBrewer)
rm(list=ls())


################################## User-defined initials ###########################################################

setwd("C:/Users/arcarlson/Documents/RSS/Parks/CHCU/Water Balance")
ParkCode = "CHCU"

#Where output plots will be written
OutDir = "Outputs"

#Colors for historical + future scenarios 
c3 = c("black", "light blue", "red") #colors for historical, warm wet, and hot dry scenarios

#Future scenario and GCM names
cfNames = c("Warm Wet", "Hot Dry")
GCMNames= c("GFDL-ESM2M RCP8.5", "HadGEM2-ES365 RCP8.5")

#Input data 
cf1 = read.csv("WB_Annual_GFDL_ESM2M_rcp85_WarmWet.csv", header = T)
cf2 = read.csv("WB_Annual_HadGEM2_ES365_rcp85_HotDry.csv", header = T)
#nochange = read.csv("mri-cgcm3/annual.csv", header = T)

HistSub = 56  #Number of points to randomly subset historical data points. Should be ~equal to number of points in each future GCM scenario

################################# End user-defined initials #######################################################



######################################### Summarize input data ####################################################

#Aggregate data by year (if input data is monthly)
cf1.ann <- aggregate(cbind(w.pet, w) ~ year, data = cf1, FUN=mean); cf1.ann$GCM<-cfNames[1]
cf2.ann <- aggregate(cbind(w.pet, w) ~ year, data = cf2, FUN=mean); cf2.ann$GCM<-cfNames[2]
#nochange.ann <- aggregate(cbind(w.pet, w) ~ year, data = nochange, FUN=mean); nochange.ann$GCM<-"NoChange"

#Combine future scenarios
#ddata<-rbind(cf1.ann,cf2.ann,nochange)
ddata = rbind(cf1.ann, cf2.ann)

#Make deficits positive values 
ddata$w.pet <- -1*ddata$w.pet

#Create date categories 
ddata$per <- 'no date'
ddata$per[which(ddata$year>=1950)] <- '1950-2005'
ddata$per[which(ddata$year>= 2006)] <- '2006-2024'
ddata$per[which(ddata$year>=2025)] <- '2025-2055'
ddata$per[which(ddata$year>=2055)] <- 'no date'
unique(ddata$per)

#Create data frame containing annual deficit, water, date period, and GCM
hww <- data.frame(ddata$year, ddata$w.pet, ddata$w, ddata$per, ddata$GCM)
names(hww) <- c("year", "deficit", "wat", "per", "gcm") #w.pet>deficit; w>wat
hww<-subset(hww, per=="1950-2005"|per=="2025-2055")
#Convert mm's to in.'s 
hww$wat <- 1.000*hww$wat
hww$wat <- hww$wat/25.4
hww$deficit <- hww$deficit/25.4

#Subset historical vs. future values - use values from both GCM's for historical 
hist<-subset(hww, per=="1950-2005")
hist = hist[sample(1:nrow(hist), HistSub, replace=F),]; hist$gcm = "Historical"
fut<-subset(hww, per=="2025-2055")#;fut<-fut[,-4]
hww<-rbind(hist,fut)
hww$gcm = factor(hww$gcm, levels=c("Historical", cfNames))

############################################## End input data summary ################################################


#################################################### Output plots ####################################################

#Density Plot
basePlot <- ggplot(hww, aes(x=deficit, colour=gcm,fill=gcm),show.legend=F) +geom_density(alpha=0.3)+ 
  scale_colour_manual("Scenario",values=c3,labels=c("Historical",paste(cfNames, " (", GCMNames, ")", sep=""))) +
  scale_fill_manual("Scenario",values=c3,labels=c("Historical",paste(cfNames, " (", GCMNames, ")", sep="")))

labelled <- basePlot + 
  labs(
    y = "Density",
    x = "Annual Moisture Deficit (in)",
    title = paste(ParkCode, "- Water Deficit for Scenarios (2025-2055) and Historical Period (1950-2005)")) 
axLab <- labelled + theme(axis.text = element_text(size=20), axis.title = element_text(size=20), legend.text=element_text(size=14),
                          plot.title=element_text(size=22, hjust=0.5),legend.position = c(.15,.9))
axLab
ggsave(paste(OutDir, "/", ParkCode, " Water Balance Density.png", sep=""), width = 15, height = 9)

#AET/Deficit Plot -- All scenarios on one
basePlot <- ggplot(hww, aes(x=deficit, y=wat, colour=gcm)) + geom_point(size=3)+ geom_smooth(method="lm", se=FALSE, size=2)+ 
          scale_colour_manual("Scenario",values=c3, labels=c("Historical",paste(cfNames, " (", GCMNames, ")", sep="")))
labelled <- basePlot + 
  labs(
    y = "Annual Actual Evapotranspiration (in)",
    x = "Annual Moisture Deficit (in)",
    colour = "Period",
    title = paste(ParkCode, "- Water Deficit for Scenarios (2025-2055) and Historical Period (1950-2005)")) + 
    theme(plot.title = element_text(size =22, hjust = 0.5)) #+ geom_vline(xintercept=mean(Historical.wb$deficit), colour="black") +geom_vline(xintercept=mean(Future.wb$deficit), colour="blue")
# size is pts
axLab <- labelled + theme(axis.text = element_text(size=20), axis.title = element_text(size=20), legend.text=element_text(size=14),
                          legend.position = c(.85,.9)) #+xlim(20,45)+ylim(2,16)
axLab
ggsave(paste(OutDir, "/", ParkCode, " Water Balance.png", sep=""), width = 15, height = 9)

#Save 
write.csv(hww, paste(OutDir, "/", ParkCode, "_DeficitET.csv", sep=""))

###################################################### End output plots ###################################################################


###################################################### END SCRIPT #########################################################################
