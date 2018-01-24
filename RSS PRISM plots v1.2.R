##  PRISM_3 seas_ann_avgs_plots vxx.R
#   John Gross   
#   Inputs:  rData output from RSS parse script

#  v1.4 - Revised monthly average Tmax/Tmin/Ppt plots. Uses separate axes to improve visibility of ppt bar plot, 
#         added std. dev. bars to ppt bar plot, and changed x-axis labels to month abbreviations. 
#  v1.3 - Includes PRISM data up to 2016, edited code to easily change data end year (May 2017)
#  v1.2 - png outputs; standardized out file names; revised regr table (16 Nov 2015)
#  v1.1 - minor fixes to red-blue plot, 10-yr running mean
  #  need to deal with outfile directories - use info from RData file and get rid of code here
#  v1.0. 30 Oct 2015 - No known errors. Runs top to bottom.
  #   conversion to in and deg F; to read rdata file as input, modified some plots, directories

library(plotrix)
library(zoo)		# for rollmean
library(ggplot2)
library(grid)

rm(list=ls())
    #  Load data file ONLY if not following previous script
RDataFile <- "WHSA_32.8125_-106.3125_PRISM_PptTminTmax_IntermediateFiles.RData"
#################################################
# DataDir = location of .RData file
# OFDir   = location where output (plots) files will be written. End with /
# Source  = location of twoPolys_function.R file

WinDataDir <- "~/RSS Plots/BIBE/Figs PRISM"
WinOFDir <- "~/RSS Plots/BIBE/Figs PRISM"

DP1 <- "/Volumes/Seagate1_Blue2TB/CHOH RSS/Figs PRISM/"
DP2 <- "/Volumes/Seagate1_Blue2TB/Projects/RSS Climate/HOBE/"

OFP1 <- "/Volumes/Seagate1_Blue2TB/CHOH RSS/Figs PRISM/"

SP1 <- "~johng/R/functions/twoPolys_function.R"
SP2 <- "D:/R/functions/TwoPolys_function.R"
SP3 <- "E:/Backup_daily/R/TwoPolys_function.R"

MacDataDir <- DP1
MacOFDir <-  DP1
MacSource <- SP1

WinSource <- SP3

beginRefYr = 1900
endRefYr = 1970

BeginYr	= 1895   # is this data file or for plots?
EndYr = 2016
dataEndYr = 2016   # needed for rolling mean plot below.  
stepYrs	= 10		  # for period plots 
rollLen = 10      # period of calc for rolling average; default 10 = decadal

dpi = 600        

#################   End of Initials  ##########################  

if(.Platform$OS.type=="windows"){   
  DataDirInit <- WinDataDir
  OFDirI <- WinOFDir
  source(WinSource)}

if(.Platform$OS.type=="unix"){     # does not distinguish MacOS from others
  DataDirInit <- MacDataDir
  OFDirI <- MacOFDir
  source(MacSource)}

load(paste(DataDirInit,"/", RDataFile, sep=''))
DataDir <- DataDirInit  # cludge because loaded file has defined DataDir
OFDir <- OFDirI
rm(DataDirInit, OFDirI)

DoYrMon <- function(YrMon){    #  YrMon = char vector of year mon as 189501.  Return vector of decimal year like 1895.42
  year <- as.numeric(substr(YrMon, 1,4))
  mon <- as.numeric(substr(YrMon, 5,6))
  yearMon <- (mon-1) * 0.083 + 0.042
  yearMon <- year+yearMon
  YRMON <- cbind(year, mon, yearMon)
  return(YRMON)
}

setwd(DataDir)
dte = Sys.Date()
    # clean up trashy namespace
rm(DF1, DF2, DF3, DP1, DP2, OFP1,  SP1, SP2, MacDataDir, MacOFDir)
    #baseData is foundational dataset for most plots
yrMons<- data.frame(DoYrMon(PptMeans$YearMon))
baseData <- cbind(yrMons, seas=PptMeans$Season, tmin=TminMeans$TminF, tmax=TmaxMeans$TmaxF, 
                  tmean=(TminMeans$TminF+TmaxMeans$TmaxF)/2, ppt=PptMeans$PptIn)
names(baseData)[1:3] <- c("yr", "mon","yrmon")

refData<-baseData[baseData$yr >= beginRefYr & baseData$year <= endRefYr,]

	# maybe should use PRISM year avgs instead i.e. month 14
pptAvg = with(baseData, tapply(ppt, yr, mean))  * 12  # xx/mo ->: xx/yr
tminAvg = with(baseData, tapply(tmin, yr, mean))
tmaxAvg = with(baseData, tapply(tmax, yr, mean))
tmeanAvg = with(baseData, tapply(tmean, yr, mean))

cYr <- BeginYr:EndYr
yrAvgs <- data.frame(cYr, pptAvg, tminAvg, tmaxAvg, tmeanAvg)
yrAvgs$tAvg <- (yrAvgs$tminAvg+yrAvgs$tmaxAvg)/2

  ## interesting to compare PRISM vs calcuated Tmean

pptRef <- data.frame(yrAvgs[yrAvgs$cYr >= beginRefYr & yrAvgs$cYr <= endRefYr, 2])
names(pptRef) <- "ppt"


######################  Periods of Analysis  ######################

doP1 <- "YES"
doP2 <- "YES"
p1_start  = 1900
p1_end    = 1970
p2_start  = 1970
p2_end    = 2016
	
yrAvgs$tmaxP1 <- yrAvgs$tmaxAvg
yrAvgs$tmaxP1[which(yrAvgs$cYr < p1_start | yrAvgs$cYr > p1_end)] = NA

yrAvgs$tminP1 <- yrAvgs$tminAvg
yrAvgs$tminP1[which(yrAvgs$cYr < p1_start | yrAvgs$cYr > p1_end)] = NA

yrAvgs$tmeanP1 <- yrAvgs$tmeanAvg
yrAvgs$tmeanP1[which(yrAvgs$cYr < p1_start | yrAvgs$cYr > p1_end)] = NA
                              
yrAvgs$pptP1 <- yrAvgs$pptAvg
yrAvgs$pptP1[which(yrAvgs$cYr < p1_start | yrAvgs$cYr > p1_end)] = NA

yrAvgs$tmaxP2 <- yrAvgs$tmaxAvg
yrAvgs$tmaxP2[which(yrAvgs$cYr < p2_start | yrAvgs$cYr > p2_end)] = NA

yrAvgs$tminP2 <- yrAvgs$tminAvg
yrAvgs$tminP2[which(yrAvgs$cYr < p2_start | yrAvgs$cYr > p2_end)] = NA

yrAvgs$tmeanP2 <- yrAvgs$tmeanAvg
yrAvgs$tmeanP2[which(yrAvgs$cYr < p2_start | yrAvgs$cYr > p2_end)] = NA

yrAvgs$pptP2 <- yrAvgs$pptAvg
yrAvgs$pptP2[which(yrAvgs$cYr < p2_start | yrAvgs$cYr > p2_end)] = NA


	########################################
	# Data check - plot min and max by month
tmaxMon <- tapply(baseData$tmax, baseData$mon, mean)
tminMon <- tapply(baseData$tmin, baseData$mon, mean)
tmeanMon <- tapply(baseData$tmean, baseData$mon, mean)
pptMon  <- tapply(baseData$ppt, baseData$mon, mean)
pptMonSD <- tapply(baseData$ppt, baseData$mon, sd)

monAvg <- data.frame(cbind(tmaxMon, tminMon, pptMon, pptMonSD))
monAvg$mon <- seq(1:12)
monAvg$monNames <- c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")
tmaxq <- tapply(baseData$tmax, baseData$mon, quantile)
tminq <- tapply(baseData$tmin, baseData$mon, quantile)

for(i in 1:12){
	q <- tmaxq[[i]]
	monAvg$tmax25[i] <- q[2]  # 2 and 4 are 25th adn 75th quantile
	monAvg$tmax75[i] <- q[4]
	
	q <- tminq[[i]]
	monAvg$tmin25[i] <- q[2]
	monAvg$tmin75[i] <- q[4]
}

PlotName <- "Avg Monthly Tmin Tmax Ppt"
OFName <- paste(OFDir, "/PRISM ", PlotName, " ", SiteID, " ", Lat, " ", Lon, sep = "")		 

png(paste(OFName, ".png", sep=""), width=6.5*dpi, height=4.5*dpi, res=dpi)
par(mar=c(5,5,2,5))
attach(monAvg)
plot(tmax75~mon,
     type="l", col="red", lty=2, lwd=2,
     xlab=NA,
     ylab=expression(paste(Temperature, ~({}^o*F))),
     ylim=c(0,110),
     xaxt='n')
lines(tmax25~mon, col="red", lty=2, lwd=2)
lines(tmaxMon~mon, col="red", lwd=3)
lines(tmin75~mon, col="blue", lty=2, lwd=2) 
lines(tmin25~mon, col="blue", lty=2, lwd=2)
lines(tminMon~mon, col="blue", lwd=3)
par(new = T)
Ppt <- barplot(pptMon, names.arg=monNames, 
               xlab="Month", ylab=NA, axes=FALSE,
               ylim=c(0,20),
               border=NA)
segments(Ppt, pptMon - pptMonSD, Ppt, pptMon + pptMonSD)
axis(side=4)
mtext(side=4, line=3, "Precip (in/mon)")
detach(monAvg)
dev.off()							

#-------------------------------------------------#
############  Running average plots   #############
#-------------------------------------------------#

rTmin <- rollmean(tminAvg, rollLen)
rTmax <- rollmean(tmaxAvg, rollLen)
rTmean <- rollmean(tmeanAvg, rollLen)
rPpt  <- rollmean(pptAvg, rollLen)

rYr = seq(dataEndYr - length(rTmin)+1, dataEndYr)

rDat <- data.frame(cbind(rYr, rTmin, rTmax, rTmean, rPpt))
names(rDat)[1] <- "cYr"
rDat$yr <- rDat$cYr
yrAvgs <- merge(rDat, yrAvgs, all=TRUE)


PlotName <- "10-yr Running Means"
a <- ggplot(aes(cYr, tmaxAvg), data=yrAvgs) + geom_line() + geom_point() +
  theme(axis.text.y = element_text(size = 12, colour="black")) +
  theme(axis.title.y = element_text(size = 12, angle = 90)) +
  theme(axis.text.x = element_text(size =12, colour="black")) +
  
  ylab(expression(paste(Tmax, ~({}^o*F)))) + xlab("") +
  # geom_text(aes(x=1895, y=29, label="A")) +
  geom_smooth(method="lm")+ 
  geom_line(aes(cYr, rTmax), size=1.5, colour="brown")

b <- ggplot(aes(cYr, tminAvg), data=yrAvgs) + geom_line() + geom_point() +
  theme(axis.text.y = element_text(size = 12, colour="black")) +
  theme(axis.title.y = element_text(size = 12, angle = 90)) +
  theme(axis.text.x = element_text(size =12, colour="black")) +
  
  ylab(expression(paste(Tmin, ~({}^o*F)))) + xlab("") +
  # geom_text(aes(x=1895, y= 13.5, label = "B")) +
  geom_smooth(method="lm")+
  geom_line(aes(cYr, rTmin), size=1.5, colour="brown")

c <- ggplot(aes(cYr, tmeanAvg), data=yrAvgs) + geom_line() + geom_point() +
  theme(axis.text.y = element_text(size = 12, colour="black")) +
  theme(axis.title.y = element_text(size = 12, angle = 90)) +
  theme(axis.text.x = element_text(size =12, colour="black")) +
  
  ylab(expression(paste(Tmean, ~({}^o*F)))) + xlab("") +
  # geom_text(aes(x=1895, y= 13.5, label = "B")) +
  geom_smooth(method="lm")+
  geom_line(aes(cYr, rTmean), size=1.5, colour="brown")

d <- ggplot(aes(cYr, pptAvg), data=yrAvgs) + geom_line() + geom_point() +
  theme(axis.text.y = element_text(size = 12, colour="black")) +
  theme(axis.title.y = element_text(size = 12, angle = 90)) +
  theme(axis.text.x = element_text(size =12, colour="black")) +
  
  ylab("Precip (in/yr)") + xlab("Year") +
  # geom_text(aes(x=1895, y=350, label = "C")) +
  geom_smooth(method="lm")+
  geom_line(aes(cYr, rPpt), size=1.5, colour="brown")

				
grid.newpage()
pushViewport(viewport(layout=grid.layout(4,1)))
vplayout<- function(x,y)
	viewport(layout.pos.row=x, layout.pos.col=y)
print(a, vp=vplayout(1,1))
print(b, vp=vplayout(2,1))
print(c, vp=vplayout(3,1))
print(d, vp=vplayout(4,1))


OFName <- paste(OFDir, "/PRISM ", PlotName, " ", SiteID, " ", Lat, " ", Lon, ".png", sep = "")
png(OFName, width=6.5*dpi, height = 8.5*dpi)
grid.newpage()
pushViewport(viewport(layout=grid.layout(4,1)))
print(a, vp=vplayout(1,1))
print(b, vp=vplayout(2,1))
print(c, vp=vplayout(3,1))
print(d, vp=vplayout(4,1))
dev.off()   

#-----------------------------------------------------------#
#            ANNUAL AVERAGE LINES WITH REGRESSION           #
#-----------------------------------------------------------#
		#    ggplot graphics    #
		# annaul points, linear regression, and 95% CI
		# need to manually set position of a, b, c labels

		# gray zone is 95% confidence interval

a <- ggplot(yrAvgs) +		#  attach only data frame in ggplot call
			geom_smooth(method = lm, aes(cYr, tmaxAvg)) +
			geom_line(aes(cYr, tmaxAvg)) + geom_point(aes(cYr, tmaxAvg)) +
      theme(axis.text.x = element_text(size = 12, colour="black")) +
			theme(axis.text.y = element_text(size = 12, colour="black")) +
			theme(axis.title.y = element_text(size = 12, angle = 90)) +		
      ylab(expression(paste(Tmax, ~({}^o*F)))) + xlab("") 
          #	geom_line(aes(cYr, rTmax), colour = 'red', size=1)}  # rolling mean

      if(doP1 == "YES")a <- a + geom_smooth(method = lm, aes(cYr, tmaxP1))
      if(doP2 == "YES")a <- a + geom_smooth(method = lm, aes(cYr, tmaxP2))

	
b <- ggplot(data=yrAvgs) + geom_line(aes(cYr, tminAvg)) + geom_point(aes(cYr, tminAvg)) +
    theme(axis.text.x = element_text(size = 12, colour="black")) +	
   theme(axis.text.y = element_text(size = 12, colour="black")) +
		theme(axis.title.y = element_text(size = 12, angle = 90)) +
		
		ylab(expression(paste(Tmin, ~({}^o*F)))) + xlab("") +
		# geom_text(aes(x=1895, y= 13.5, label = "B")) +
		geom_smooth(aes(cYr, tminAvg), method="lm") 
        # geom_line(aes(cYr, rTmin), colour = 'blue', size=1)

      if(doP1 == "YES")(b <- b +	geom_smooth(method = lm, aes(cYr, tminP1)))
			if(doP2 == "YES")b <- b +	geom_smooth(method = lm, aes(cYr, tminP2)) 
						
c <- ggplot(data=yrAvgs) + geom_line(aes(cYr, tmeanAvg)) + geom_point(aes(cYr, tmeanAvg)) +
    theme(axis.text.x = element_text(size = 12, colour="black")) +	
    theme(axis.text.y = element_text(size = 12, colour="black")) +
		theme(axis.title.y = element_text(size = 12, angle = 90)) +
		
		ylab(expression(paste(Tmean, ~({}^o*F)))) + xlab("") +
		# geom_text(aes(x=1895, y= 13.5, label = "B")) +
		geom_smooth(aes(cYr, tmeanAvg), method="lm") 
		    # geom_line(aes(cYr, rTmean), colour = 'black', size=1)

    if(doP1 == "YES")c <- c + geom_smooth(method = lm, aes(cYr, tmeanP1)) 	
    if(doP2 == "YES") c <- c + geom_smooth(method = lm, aes(cYr, tmeanP2)) 

d <- ggplot(data=yrAvgs) + geom_line(aes(cYr, pptAvg)) + geom_point(aes(cYr, pptAvg)) +
    theme(axis.text.x = element_text(size = 12, colour="black")) +	
    theme(axis.text.y = element_text(size = 12, colour="black")) +
		theme(axis.title.y = element_text(size = 12, angle = 90)) +
				
		ylab("Precipitation (in/yr)") + xlab("Year") +
		# geom_text(aes(x=1895, y=350, label = "C")) +
		geom_smooth(aes(cYr, pptAvg), method="lm") 
		    #geom_line(aes(cYr, rPpt), colour = 'green', size=1)

    if(doP1 == "YES")d <- d + geom_smooth(method = lm, aes(cYr, pptP1))
    if(doP2 == "YES")d <- d + geom_smooth(method = lm, aes(cYr, pptP2)) 
		
grid.newpage()
pushViewport(viewport(layout=grid.layout(4,1)))
print(a, vp=vplayout(1,1))
print(b, vp=vplayout(2,1))
print(c, vp=vplayout(3,1))
print(d, vp=vplayout(4,1))

grid.newpage()
pushViewport(viewport(layout=grid.layout(2,1)))
print(a, vp=vplayout(1,1))
print(b, vp=vplayout(2,1))

grid.newpage()
pushViewport(viewport(layout=grid.layout(2,1)))
print(c, vp=vplayout(1,1))
print(d, vp=vplayout(2,1))

PlotName <- "Annual Means Lines Regressions"

OFName <- paste(OFDir, "/PRISM ", PlotName, " 4-panel ", SiteID, " ", Lat, " ", Lon, ".png", sep = "")
png(OFName, width=6.5*dpi, height = 8.5*dpi)

grid.newpage()
pushViewport(viewport(layout=grid.layout(4,1)))
print(a, vp=vplayout(1,1))
print(b, vp=vplayout(2,1))
print(c, vp=vplayout(3,1))
print(d, vp=vplayout(4,1))
dev.off()

OFName <- paste(OFDir, "/PRISM ", PlotName, " Tmin Tmax ", SiteID, " ", Lat, " ", Lon, ".png", sep = "")
png(OFName, width=6.5*dpi, height = 6.0*dpi)
grid.newpage()
pushViewport(viewport(layout=grid.layout(2,1)))
print(a, vp=vplayout(1,1))
print(b, vp=vplayout(2,1))
dev.off()

OFName <- paste(OFDir, "/PRISM ", PlotName, " Tmean Precip ", SiteID, " ", Lat, " ", Lon, ".png", sep = "")
png(OFName, width=6.5*dpi, height = 6.0*dpi)
grid.newpage()
pushViewport(viewport(layout=grid.layout(2,1)))
print(c, vp=vplayout(1,1))
print(d, vp=vplayout(2,1))
dev.off()

		# regressions for trends
 
lmTmax <- lm(yrAvgs$tmaxAvg~cYr)
lmTmaxP1 <- lm(yrAvgs$tmaxP1~cYr)
lmTmaxP2 <- lm(yrAvgs$tmaxP2~cYr)

lmTmin <- lm(yrAvgs$tminAvg~cYr)
lmTminP1 <- lm(yrAvgs$tminP1~cYr)
lmTminP2 <- lm(yrAvgs$tminP2~cYr)

lmTmean <- lm(yrAvgs$tmeanAvg~cYr)
lmTmeanP1 <- lm(yrAvgs$tmeanP1~cYr)
lmTmeanP2 <- lm(yrAvgs$tmeanP2~cYr)

lmPpt  <- lm(yrAvgs$pptAvg~cYr)		
lmPptP1 <- lm(yrAvgs$pptP1~cYr)		
lmPptP2 <- lm(yrAvgs$pptP2~cYr)		

    # make table of coefficients
probStar <- function(pVal){
  probStar <- "NS"
  if(pVal < 0.05)probStar <- "*"
  if(pVal < 0.01)probStar <- "**"
  if(pVal < 0.001)probStar <- "***"
  probStar
}

lmMetrics <- function(lmout){
  s <- summary(lmout)
  # equ <- as.character(s$call)
  # eq <- equ[2]
  YrCoeff <- s$coefficients[2,1]
  ses <- coef(s)[,"Std. Error"]   # gets intercept & slope
  seSlope <- ses[2]
  probCoeff <- s$coefficients[2,4]
  probSign <- probStar(probCoeff)
  r2 <- s$r.squared
  data.frame(YrCoeff,seSlope,probCoeff, probSign, r2)
}

regsTmax <-  rbind(lmMetrics(lmTmax), lmMetrics(lmTmaxP1), lmMetrics(lmTmaxP2))
regsTmin <-  rbind(lmMetrics(lmTmin), lmMetrics(lmTminP1), lmMetrics(lmTminP2))
regsTmean <- rbind(lmMetrics(lmTmean),lmMetrics(lmTmeanP1),lmMetrics(lmTmeanP2))
regsPpt <-   rbind(lmMetrics(lmPpt),  lmMetrics(lmPptP1),  lmMetrics(lmPptP2))

perAll <- paste(min(yrAvgs$cYr), max(yrAvgs$cYr), sep="-")
per1 <- paste(p1_start, p1_end, sep="-")
per2 <- paste(p2_start, p2_end, sep="-")
Period <- rep(c(perAll, per1, per2), 4)

lmTable <- cbind( Var=rep(c("Tmax", "Tmin", "Tmean", "Precip"),each=3), Period, rbind(regsTmax, regsTmin, regsTmean, regsPpt))

lmTable$YrCoeff <- lmTable$YrCoeff * 100   # convert to degF(in)/100-yrs
lmTable$seSlope <- lmTable$seSlope * 100
  #add units to YrCoeff field
colnames(lmTable) <- c("Var", "Period", "YrCoeff(degF(in)/100yrs)", "seSlope", "probCoeff", "probSign", "r2")

print(lmTable, row.names = F)

write.csv(lmTable, paste(OFDir, "/PRISM ", SiteID, " Regression Table test ", Sys.Date(), ".csv", 
      sep=""), row.names=FALSE)

rm(lmPpt,lmPptP1,lmPptP2,lmTmax,lmTmaxP1,lmTmaxP2,lmTmin,lmTminP1,lmTminP2,lmTmean,lmTmeanP1,lmTmeanP2)
rm(regsTmax, regsTmin, regsTmean, regsPpt, lmTable)

#-------------------------------------------------#
#         Decadal PLOTS by "step" and season  #####
#-------------------------------------------------#

nSteps = floor((EndYr - BeginYr)/stepYrs)	# year steps 
sStepAvgs = matrix(-99, nrow = nSteps * 4, ncol = 5)
seasStepAvgs = data.frame(sStepAvgs);  rm(sStepAvgs)
seasName <- c("Winter", "Spring", "Summer", "Fall")
seasStepAvgs$seas <- rep(seasName, nSteps)

names(seasStepAvgs) <- c("startYr", "tmin", "tmax", "tmean", "ppt", "seas")
	#columns are: 1=startYr 2=tmin 3=tmax 4=ppt 5=seas 
i=0

for(i in 0:(nSteps-1)){
	curBeginYr = BeginYr + i*stepYrs
	tempStepData <- baseData[baseData$yr >= curBeginYr & baseData$yr < (BeginYr + (i+1)*stepYrs), ]
	for(seas in 1:4) {
		stepYr = i * 4 + seas
		seasStepAvgs$startYr[stepYr] = curBeginYr
		seasStepAvgs$tmin[stepYr] = mean(tempStepData$tmin[tempStepData$seas == seasName[seas]])
		seasStepAvgs$tmax[stepYr] = mean(tempStepData$tmax[tempStepData$seas == seasName[seas]])
		seasStepAvgs$tmean[stepYr] = mean(tempStepData$tmean[tempStepData$seas == seasName[seas]])
		seasStepAvgs$ppt[stepYr] = mean(tempStepData$ppt[tempStepData$seas == seasName[seas]])
	}		# next seas
}			# next nSteps

rm(tempStepData)
seasStepAvgs$ppt <- seasStepAvgs$ppt * 12  # xx/mo to xx/yr

#-------------------------------------------------#
#    ANOMALY LINE PLOTS - By Decade and Season
#-------------------------------------------------#

#  1 per season, points at "step" yr intervals (decade if step == 10)
#  Labels (A, B, C) need to be placed manually
#  Adjust vertical scale using ylim(y min, y max)

########################################

yrLab <- c(seq(BeginYr+floor(.5*stepYrs), EndYr-floor(.5*stepYrs), by = stepYrs))
par(mfrow=c(3,1), mar=c(4,4,1,2),  cex = 1)
lineTyp = c(1,1,1,1)
lineCol = c("blue", "dark green", "red", "brown")

pdat <- seasStepAvgs   # period data

	# these are departures from entire period of record
seas
pdat$tmin[which(pdat$seas=="Winter")] <- pdat$tmin[which(pdat$seas=="Winter")] - mean(pdat$tmin[which(pdat$seas=="Winter")])
pdat$tmin[which(pdat$seas=="Spring")] <- pdat$tmin[which(pdat$seas=="Spring")] - mean(pdat$tmin[which(pdat$seas=="Spring")])
pdat$tmin[which(pdat$seas=="Summer")] <- pdat$tmin[which(pdat$seas=="Summer")] - mean(pdat$tmin[which(pdat$seas=="Summer")])
pdat$tmin[which(pdat$seas=="Fall")] <- pdat$tmin[which(pdat$seas=="Fall")] - mean(pdat$tmin[which(pdat$seas=="Fall")])

pdat$tmax[which(pdat$seas=="Winter")] <- pdat$tmax[which(pdat$seas=="Winter")] - mean(pdat$tmax[which(pdat$seas=="Winter")])
pdat$tmax[which(pdat$seas=="Spring")] <- pdat$tmax[which(pdat$seas=="Spring")] - mean(pdat$tmax[which(pdat$seas=="Spring")])
pdat$tmax[which(pdat$seas=="Summer")] <- pdat$tmax[which(pdat$seas=="Summer")] - mean(pdat$tmax[which(pdat$seas=="Summer")])
pdat$tmax[which(pdat$seas=="Fall")] <- pdat$tmax[which(pdat$seas=="Fall")] - mean(pdat$tmax[which(pdat$seas=="Fall")])

pdat$tmean[which(pdat$seas=="Winter")] <- pdat$tmean[which(pdat$seas=="Winter")] - mean(pdat$tmean[which(pdat$seas=="Winter")])
pdat$tmean[which(pdat$seas=="Spring")] <- pdat$tmean[which(pdat$seas=="Spring")] - mean(pdat$tmean[which(pdat$seas=="Spring")])
pdat$tmean[which(pdat$seas=="Summer")] <- pdat$tmean[which(pdat$seas=="Summer")] - mean(pdat$tmean[which(pdat$seas=="Summer")])
pdat$tmean[which(pdat$seas=="Fall")] <- pdat$tmean[which(pdat$seas=="Fall")] - mean(pdat$tmean[which(pdat$seas=="Fall")])

pdat$ppt[which(pdat$seas=="Winter")] <- pdat$ppt[which(pdat$seas=="Winter")] - mean(pdat$ppt[which(pdat$seas=="Winter")])
pdat$ppt[which(pdat$seas=="Spring")] <- pdat$ppt[which(pdat$seas=="Spring")] - mean(pdat$ppt[which(pdat$seas=="Spring")])
pdat$ppt[which(pdat$seas=="Summer")] <- pdat$ppt[which(pdat$seas=="Summer")] - mean(pdat$ppt[which(pdat$seas=="Summer")])
pdat$ppt[which(pdat$seas=="Fall")] <- pdat$ppt[which(pdat$seas=="Fall")] - mean(pdat$ppt[which(pdat$seas=="Fall")])

pdat$clr <- "NA"
pdat$clr[which(pdat$seas=="Winter")] <- "blue"
pdat$clr[which(pdat$seas=="Spring")] <- "darkgreen"
pdat$clr[which(pdat$seas=="Summer")] <- "red"
pdat$clr[which(pdat$seas=="Fall")] <- "brown"

      # need roundDown function to define axis ranges

PlotName <- "Season Decadal Anomaly Lines" 
a <- ggplot(pdat, aes(startYr+5, tmax, group=seas, colour=seas)) +
		#ylim(-1.75, 1.75) +
		geom_line(colour = pdat$clr, size=1) +
		geom_point(colour = pdat$clr, size=3.2, shape = pdat$seas) +
		
		theme(axis.text.y = element_text(size = 10)) +
		theme(axis.title.y = element_text(size = 12, angle = 90)) +
		scale_x_continuous(breaks = c(1920, 1940, 1960, 1980, 2000)) +
		ylab(expression(paste(Tmax, ~({}^o*F)))) + xlab("")
		
b <- ggplot(pdat, aes(startYr+5, tmin, group=seas, colour=seas)) +
		#ylim(-1.75, 1.75) +
		geom_line(colour = pdat$clr, size=1) +
		geom_point(colour = pdat$clr, size=3.2, shape = pdat$seas) +
		
		theme(axis.text.y = element_text(size = 10)) +
		theme(axis.title.y = element_text(size = 12, angle = 90)) +
		scale_x_continuous(breaks = c(1920, 1940, 1960, 1980, 2000)) +
		ylab(expression(paste(Tmin, ~({}^o*F)))) + xlab("")
		
c <- ggplot(pdat, aes(startYr+5, tmean, group=seas, colour=seas)) +
		#ylim(-1.75, 1.75) +
		geom_line(colour = pdat$clr, size=1) +
		geom_point(colour = pdat$clr, size=3.2, shape = pdat$seas) +
		
		theme(axis.text.y = element_text(size = 10)) +
		theme(axis.title.y = element_text(size = 12, angle = 90)) +
		scale_x_continuous(breaks = c(1920, 1940, 1960, 1980, 2000)) +
		ylab(expression(paste(Tmean, ~({}^o*F)))) + xlab("")
		
d <- ggplot(pdat, aes(startYr+5, ppt, group=seas, colour=seas)) +
		#ylim(-325, 325) +
		geom_line(colour = pdat$clr, size=1) +
		geom_point(colour = pdat$clr, size=3.2, shape = pdat$seas) +
		
		theme(axis.text.y = element_text(size = 7)) +
		theme(axis.title.y = element_text(size = 12, angle = 90)) +
		scale_x_continuous(breaks = c(1920, 1940, 1960, 1980, 2000)) +
		ylab("Precip (in / yr)") + xlab("")
				
grid.newpage()
pushViewport(viewport(layout=grid.layout(4,1)))
vplayout<- function(x,y)
	viewport(layout.pos.row=x, layout.pos.col=y)
print(a, vp=vplayout(1,1))
print(b, vp=vplayout(2,1))
print(c, vp=vplayout(3,1))
print(d, vp=vplayout(4,1))

OFName <- paste(OFDir, "/PRISM ", PlotName, " ", SiteID, " ", Lat, " ", Lon, ".png", sep = "")
png(OFName, width=6.5*dpi, height = 8.5*dpi)
{
  grid.newpage()
  pushViewport(viewport(layout=grid.layout(4,1)))
  print(a, vp=vplayout(1,1))    
  print(b, vp=vplayout(2,1))
  print(c, vp=vplayout(3,1))
  print(d, vp=vplayout(4,1))
}
dev.off()


#------------------------------------------------------------#
#   Boxplots of anomolies  - from entire period of record
#------------------------------------------------------------#
	
	#  calc seasonal avg for each year then subtract ref mean	
tminSeas <- data.frame(tapply(baseData$tmin, list(baseData$yr,baseData$seas), FUN=mean))
tmaxSeas <- data.frame(tapply(baseData$tmax, list(baseData$yr,baseData$seas), FUN=mean))
tmeanSeas <- data.frame(tapply(baseData$tmean, list(baseData$yr,baseData$seas), FUN=mean))
pptSeas  <- data.frame(tapply(baseData$ppt,   list(baseData$yr,baseData$seas),   FUN=mean))

  # need to swap this and next section so use tminSeas absolute value, then calc anomoly and plot			
	# departure from entire period of record
for(i in 1:4)
	{ 	tminSeas[,i] <- tminSeas[,i] - mean(tminSeas[,i])
	  	tmaxSeas[,i] <- tmaxSeas[,i] - mean(tmaxSeas[,i])
	  	tmeanSeas[,i] <- tmeanSeas[,i] - mean(tmeanSeas[,i])
		  pptSeas[,i] <- pptSeas[,i] - mean(pptSeas[,i])}

yrcat <- (round(unique(baseData$yr)/10)*10)   # year category (e.g., 1900,1900, 1910)
yLabPs <- length(unique(yrcat))

poffset <- c(-.2, 0, .2, .4)  # box offsets for win, spr, sum, fal
pcol <- c("lightblue", "lightgreen", "lightpink", "linen")

	# bp1 is first plot, with title and other features
bp1 <- function( yDat, xDat, pcol="lightblue", offst = -.2, Title="Title here", 
	Ylab="Degree (F)", yMax=-9999, yMin=-9999, yLabPos=1:12)
	{	if(yMax == -9999) yMax <- ceiling(max(yDat))
		if(yMin == -9999) yMin <- floor(min(yDat))
		
		boxplot(yDat ~ xDat, col=pcol, ylab=Ylab, 
			at= 1:yLabPos + offst, boxwex=0.2, 		
			ylim = c(yMin, yMax), main = Title)	}
 	
 	# bp2 are other plots added to e.g bp1
 	
bp2 <- function(yDat, xDat, pcol="lightblue", offst = 0, yLabPos=1:12)
	{	boxplot(yDat ~ xDat, boxwex=0.2, col=pcol,
		at=1:yLabPos + offst, add=TRUE, show.names=FALSE) }

PlotName <- "Seas Decadal Tmax Tmin Anomaly Box"
  OFName <- paste(OFDir, "/PRISM ", PlotName, " ", SiteID, " ", Lat, " ", Lon, ".png", sep = "")
  png(OFName, width=6.5*dpi, height = 8.5*dpi)
par(mfrow=c(2,1))
bp1(tmaxSeas$Win, yrcat, Title="Maximum Temperature Anomaly by Season and Decade", yLabPos = yLabPs,
    offst=poffset[1]) 
bp2(tmaxSeas$Spr, yrcat, pcol=pcol[2],offst=poffset[2], yLabPos=yLabPs)
bp2(tmaxSeas$Sum, yrcat, pcol=pcol[3],offst=poffset[3], yLabPos=yLabPs)
bp2(tmaxSeas$Fal, yrcat, pcol=pcol[4],offst=poffset[4], yLabPos=yLabPs)

bp1(tminSeas$Win, yrcat, Title="Minimum Temperature Anomaly by Season and Decade", yLabPos = yLabPs) 
bp2(tminSeas$Spr, yrcat, pcol=pcol[2],offst=poffset[2], yLabPos=yLabPs)
bp2(tminSeas$Sum, yrcat, pcol=pcol[3],offst=poffset[3], yLabPos=yLabPs)
bp2(tminSeas$Fal, yrcat, pcol=pcol[4],offst=poffset[4], yLabPos=yLabPs)
dev.off()

PlotName <- "Seas Decadal Tmean Ppt Anomaly Box"
  OFName <- paste(OFDir, "/PRISM ", PlotName, " ", SiteID, " ", Lat, " ", Lon, ".png", sep = "")
  png(OFName, width=6.5*dpi, height = 8.5*dpi)
par(mfrow=c(2,1))
bp1(tmeanSeas$Win, yrcat, Title="Mean Temperature Anomaly by Season and Decade", yLabPos = yLabPs) 
bp2(tmeanSeas$Spr, yrcat, pcol=pcol[2],offst=poffset[2], yLabPos=yLabPs)
bp2(tmeanSeas$Sum, yrcat, pcol=pcol[3],offst=poffset[3], yLabPos=yLabPs)
bp2(tmeanSeas$Fal, yrcat, pcol=pcol[4],offst=poffset[4], yLabPos=yLabPs)

bp1(pptSeas$Win, yrcat, Title="Precipitation Anomaly by Season and Decade", yLabPos = yLabPs, Ylab="Precip (in/mo)") 
bp2(pptSeas$Spr, yrcat, pcol=pcol[2],offst=poffset[2], yLabPos=yLabPs)
bp2(pptSeas$Sum, yrcat, pcol=pcol[3],offst=poffset[3], yLabPos=yLabPs)
bp2(pptSeas$Fal, yrcat, pcol=pcol[4],offst=poffset[4], yLabPos=yLabPs)
dev.off()   
	
#-------------------------------------------------#
#     Box Plots - Decadal Avgs Annual & Seasonal
#-------------------------------------------------#

		####  Period (e.g. decadal) averages  #####

nSteps = floor((EndYr - BeginYr)/stepYrs)	# year steps 
stepVal <- rep(0,nSteps)
midVal <- floor(stepYrs/2)

for(i in 0:(nSteps-1))
	stepVal[(1+i*stepYrs):((i+1)*stepYrs)] <- rep(midVal + i*stepYrs + BeginYr, stepYrs)
	
stepData <- yrAvgs[yrAvgs$cYr >= BeginYr,]
stepData <- stepData[1:length(stepVal),]
stepData$stepVal <- stepVal

tminLab <- expression(paste("Minimum Temperature (", degree*F,")", sep=""))
tmaxLab <- expression(paste("Maximum Temperature (", degree*F,")", sep=""))
tmeanLab <- expression(paste("Average Temperature (", degree*F,")", sep=""))
par(mfrow=c(4,1))

PlotName <- "Decadal Avg Tmin Tmax Tmean Ppt Box"
OFName <- paste(OFDir, "/PRISM ", PlotName, " ", SiteID, " ", Lat, " ", Lon, ".png", sep = "")
  png(OFName, width=6.5*dpi, height = 8.5*dpi)
  par(mfrow=c(2,2))
boxplot(tmaxAvg ~ stepVal, data=stepData, boxwex=0.5, col="lightpink", ylab=tmaxLab)
boxplot(tminAvg ~ stepVal, data=stepData, boxwex=0.5, col="lightblue", ylab=tminLab)
boxplot(tmeanAvg ~ stepVal, data=stepData, boxwex=0.5, col="lightgray", ylab=tmeanLab)
boxplot(pptAvg ~ stepVal, data=stepData, boxwex=0.5, col="lightgreen", ylab="Precip (mm/yr)")
dev.off()

##  ========= Decadal values by season

pltD <- baseData[baseData$yr >= BeginYr & baseData$yr < EndYr, ]
midVal <- floor(stepYrs/2)
pltD$midYr <- BeginYr + floor((pltD$yr-BeginYr)/stepYrs) * stepYrs + midVal

PlotName <- "Decadal Seasonal Tmax Box"
  OFName <- paste(OFDir, "/PRISM ", PlotName, " ", SiteID, " ", Lat, " ", Lon, ".png", sep = "")
  png(OFName, width=6.5*dpi, height = 8.5*dpi)
  par(mfrow=c(2,2))
boxplot(tmax ~ midYr, data=pltD[pltD[,4]=="Winter",], boxwex=0.5, col="lightblue", ylab= tmaxLab, main="Winter")
boxplot(tmax ~ midYr, data=pltD[pltD[,4]=="Spring",], boxwex=0.5, col="lightgreen", ylab=tmaxLab, main="Spring")
boxplot(tmax ~ midYr, data=pltD[pltD[,4]=="Summer",], boxwex=0.5, col="lightpink", ylab=tmaxLab, main="Summer")
boxplot(tmax ~ midYr, data=pltD[pltD[,4]=="Fall",], boxwex=0.5, col="linen", ylab=tmaxLab, main="Fall")
dev.off()

PlotName <- "Decadal Seasonal Tmin Box"
  OFName <- paste(OFDir, "/PRISM ", PlotName, " ", SiteID, " ", Lat, " ", Lon, ".png", sep = "")
  png(OFName, width=6.5*dpi, height = 8.5*dpi)
  par(mfrow=c(2,2))
boxplot(tmin ~ midYr, data=pltD[pltD[,4]=="Winter",], boxwex=0.5, col="lightblue", ylab=tminLab, main="Winter")
boxplot(tmin ~ midYr, data=pltD[pltD[,4]=="Spring",], boxwex=0.5, col="lightgreen", ylab=tminLab, main="Spring")
boxplot(tmin ~ midYr, data=pltD[pltD[,4]=="Summer",], boxwex=0.5, col="lightpink", ylab=tminLab, main="Summer")
boxplot(tmin ~ midYr, data=pltD[pltD[,4]=="Fall",], boxwex=0.5, col="linen", ylab=tminLab, main="Fall")
dev.off()

PlotName <- "Decadal Seasonal Tmean Box"
  OFName <- paste(OFDir, "/PRISM ", PlotName, " ", SiteID, " ", Lat, " ", Lon, ".png", sep = "")
  png(OFName, width=6.5*dpi, height = 8.5*dpi)
  par(mfrow=c(2,2))
boxplot(tmean ~ midYr, data=pltD[pltD[,4]=="Winter",], boxwex=0.5, col="lightblue", ylab=tmeanLab, main="Winter")
boxplot(tmean ~ midYr, data=pltD[pltD[,4]=="Spring",], boxwex=0.5, col="lightgreen", ylab=tmeanLab, main="Spring")
boxplot(tmean ~ midYr, data=pltD[pltD[,4]=="Summer",], boxwex=0.5, col="lightpink", ylab=tmeanLab, main="Summer")
boxplot(tmean ~ midYr, data=pltD[pltD[,4]=="Fall",], boxwex=0.5, col="linen", ylab=tmeanLab, main="Fall")
dev.off()

PlotName <- "Decadal Seasonal Ppt Box"
  OFName <- paste(OFDir, "/PRISM ", PlotName, " ", SiteID, " ", Lat, " ", Lon, ".png", sep = "")
  png(OFName, width=6.5*dpi, height = 8.5*dpi)
  par(mfrow=c(2,2))
boxplot(ppt ~ midYr, data=pltD[pltD[,4]=="Winter",], boxwex=0.5, col="lightblue", ylab="Precipitation (in/mon)", main="Winter")
boxplot(ppt ~ midYr, data=pltD[pltD[,4]=="Spring",], boxwex=0.5, col="lightgreen", ylab="Precipitation (in/mon)", main="Spring")
boxplot(ppt ~ midYr, data=pltD[pltD[,4]=="Summer",], boxwex=0.5, col="lightpink", ylab="Precipitation (in/mon)", main="Summer")
boxplot(ppt ~ midYr, data=pltD[pltD[,4]=="Fall",], boxwex=0.5, col="linen", ylab="Precipitation (in/mon)", main="Fall")
dev.off()   #  box plots

#--------------------------------------------------------------------------#
#       RED BLUE anomaly plots - from reference period (not entire record) #
#--------------------------------------------------------------------------#

mtempMin = mean(yrAvgs$tminAvg)
mtempMax = mean(yrAvgs$tmaxAvg)
mtempMean = mean(yrAvgs$tmeanAvg)
mppt = mean(yrAvgs$pptAvg)

mTmin = mean(refData$tmin)
mTmax = mean(refData$tmax)
mTmean = mean(refData$tmean)
mPrcp = mean(refData$ppt)
xlabel = "Year"

PlotName <- "Red-Blue Anomaly Filled Line"
  OFName <- paste(OFDir, "/PRISM ", PlotName, " ", SiteID, " ", Lat, " ", Lon, ".png", sep = "")
  png(OFName, width=6.5*dpi, height = 8.5*dpi)
  par(mfrow=c(4,1), bty="l", mar=c(4,4,1,2), cex = 1)
     
  # Tmax
temp = yrAvgs$tmaxAvg - mtempMax	
plot(yrAvgs$cYr, temp, type = 'n', xlab=xlabel, 
	ylab = expression(paste("Tmax (", degree*F,")", sep=""))
	, xlim =c(BeginYr, dataEndYr))
	# text(1896, 18.9, "A", cex = 1.2)	
abline(h = 0)	
tp <- TwoPolys(yrAvgs$cYr, temp, 0)
polygon(tp$X, tp$lower, col="blue")
polygon(tp$X, tp$upper, col="red")
    
# Tmin
temp = yrAvgs$tminAvg - mtempMin  
plot(yrAvgs$cYr, temp, type = 'n', xlab=xlabel, 	
	ylab = expression(paste("Tmin (", degree*F,")", sep="")), xlim =c(BeginYr, dataEndYr))
	# text(1896, 2.08, "B", cex = 1.2)		
abline(h = 0)

tp <- TwoPolys(yrAvgs$cYr, temp, 0)
polygon(tp$X, tp$lower, col="blue")
polygon(tp$X, tp$upper, col="red")
    # TMean
temp = yrAvgs$tmeanAvg - mtempMean
plot(yrAvgs$cYr, temp, type = 'n', xlab=xlabel, 	
	ylab = expression(paste("Tmean (", degree*F,")", sep="")), xlim =c(BeginYr, dataEndYr))
	# text(1896, 2.08, "B", cex = 1.2)		
abline(h = 0)
tp <- TwoPolys(yrAvgs$cYr, temp, 0)
polygon(tp$X, tp$lower, col="blue")
polygon(tp$X, tp$upper, col="red")

#### annual precip plot ###
temp = yrAvgs$pptAvg - mppt
xlabel = "Year"
ylabel = "Precip (in / yr)"
plot(yrAvgs$cYr, temp, type = 'n', xlab=xlabel,
     ylab=ylabel, xlim =c(BeginYr, dataEndYr))
#  text(1896, 622, "C", cex = 1.2)	
abline(h = 0)
tp <- TwoPolys(yrAvgs$cYr, temp, 0)
polygon(tp$X, tp$lower, col="brown")
polygon(tp$X, tp$upper, col="green")

dev.off()   

### EOF ###

