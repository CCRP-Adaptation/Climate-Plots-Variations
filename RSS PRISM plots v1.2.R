##  PRISM_3 seas_ann_avgs_plots vxx.R
#   John Gross   
#   Inputs:  rData output from RSS parse script

#  v1.5 - Converted all plots to ggplot except for Avg Monthly Tmin Tmax Precip, updated plot titles and captions.
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
library(cowplot)
library(reshape2)

rm(list=ls())
    #  Load data file ONLY if not following previous script
RDataFile <- "JECA_43.75_-103.83_PRISM_PptTminTmax_IntermediateFiles.RData"
#################################################
# DataDir = location of .RData file
# OFDir   = location where output (plots) files will be written. End with /

WinDataDir <- "~/RSS/Parks/JECA/Figs PRISM"
WinOFDir <- "~/RSS/Parks/JECA/Figs PRISM"

MacDataDir <- "/Volumes/Seagate1_Blue2TB/CHOH RSS/Figs PRISM/"
MacOFDir <-  "/Volumes/Seagate1_Blue2TB/CHOH RSS/Figs PRISM/"

doP1 <- "YES"  # Should a separate regression be calculated for the reference period (default 1900-1970)? 
doP2 <- "YES"  # Should a separate regression be calculate for the period after the reference period (default 1971-present)? 
beginRefYr = 1900
endRefYr = 1970

BeginYr	= 1895   # is this data file or for plots?
EndYr = 2016
dataEndYr = 2016   # needed for rolling mean plot below.  
stepYrs	= 10		  # for period plots 
rollLen = 10      # period of calc for rolling average; default 10 = decadal

dpi = 600    


##ggplot theme for all plots
#Theme for all plots
PlotTheme = theme_gray() %+replace% 
  theme(plot.title = element_text(size=18, face='bold', hjust=0.5, vjust=0.5),
        axis.text.y = element_text(size = 16, colour="black"),
        axis.title.y = element_text(size = 18, angle = 90, margin=margin(0,5,0,0)),
        axis.text.x = element_text(size = 16, colour="black"),
        axis.title.x = element_text(size = 18, margin=margin(5,0,0,0)),
        legend.position = "none",
        legend.title = element_text(size=16),
        legend.text = element_text(size=16)
  )

theme_set(PlotTheme)
TitleSize = theme_get()$plot.title$size  ##Needed for cowplot layouts

#################   End of Initials  ##########################  

if(.Platform$OS.type=="windows"){   
  DataDirInit <- WinDataDir
  OFDirI <- WinOFDir}

if(.Platform$OS.type=="unix"){     # does not distinguish MacOS from others
  DataDirInit <- MacDataDir
  OFDirI <- MacOFDir}

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
rm(WinDataDir, WinOFDir, MacDataDir, MacOFDir)
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

p1_start  = beginRefYr
p1_end    = endRefYr
p2_start  = endRefYr
p2_end    = EndYr
	
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
pptq <- tapply(baseData$ppt, baseData$mon, quantile)

for(i in 1:12){
	q <- tmaxq[[i]]
	monAvg$tmax25[i] <- q[2]  # 2 and 4 are 25th adn 75th quantile
	monAvg$tmax75[i] <- q[4]
	
	q <- tminq[[i]]
	monAvg$tmin25[i] <- q[2]
	monAvg$tmin75[i] <- q[4]
	
	q <- pptq[[i]]
	monAvg$ppt25[i] <- q[2]
	monAvg$ppt75[i] <- q[4]
}

PlotName <- "Avg Monthly Tmin Tmax Ppt"
OFName <- paste(OFDir, "/PRISM ", PlotName, " ", SiteID, " ", Lat, " ", Lon, sep = "")		 

png(paste(OFName, ".png", sep=""), width=6.5*dpi, height=4.5*dpi, res=dpi)
par(mfrow=c(1,1), mgp=c(0,.5,0), mar=c(4,3.75,2,3.75))
attach(monAvg)
Ppt = barplot(pptMon, names.arg=monNames,
        ylim=c(0, max(monAvg$ppt75)+.5),
        axes=FALSE,
        border=NA,
        col=rgb(.678, .847, .902, alpha=0.6))
segments(Ppt, ppt25, Ppt, ppt75, col="dark gray")
axis(side=4)
par(new = T)
plot(tmax75~mon, 
     type="l", col="red", lty=2, lwd=2,
     xlab=NA,
     ylab=NA,
     xaxt='n',
     xlim=c(.5, 12.5),
     ylim=c(0,110), 
     ps = 2,
     main=paste(SiteID, "- Monthly Climate Means", sep="") 
)
lines(tmax25~mon, col="red", lty=2, lwd=2)
lines(tmaxMon~mon, col="red", lwd=3)
lines(tmin75~mon, col="blue", lty=2, lwd=2) 
lines(tmin25~mon, col="blue", lty=2, lwd=2)
lines(tminMon~mon, col="blue", lwd=3)

axis(side=2)
mtext(side=1, line=1.75, "Month")
mtext(side=2, line=2, expression(paste(Temperature, ~({}^o*F))))
mtext(side=4, line=2, "Precip (in)")
mtext(side=1, line=2.75, paste("Dashed lines/error bars = 25th-75th percentile ranges. Data range = ", BeginYr, "-", EndYr, ".", sep=""), cex=0.75, adj=0.5)
legend("topleft", legend=c("Tmax", "Tmin"), col=c("red", "blue"), lwd=c(2,2), cex=0.75, bty="n")
legend(.4, 103, legend=c("Precip"), fill=c("light blue"), border=c(NA), cex=0.75, bty="n")
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

##ggplot
PlotName <- "10-yr Running Means"

#Colors for running means
RMColors = scale_color_manual(name="", values=c("brown", "black", "#3366FF"))

a <- ggplot(aes(x=cYr), data=yrAvgs) + 
  geom_line(aes(y=tmaxAvg, group=1, col="Annual means"), na.rm=TRUE) + 
  geom_point(aes(y=tmaxAvg, col="Annual means"), na.rm=TRUE) +
  ylab(expression(paste(Tmax, ~({}^o*F)))) + xlab("") +
  # geom_text(aes(x=1895, y=29, label="A")) +
  geom_smooth(method="lm", aes(y=tmaxAvg, group=2, col="Regression trend"), na.rm=TRUE)+ 
  geom_line(aes(y=rTmax, group=3, col=paste(rollLen, "-yr running mean", sep="")), size=1.5, na.rm=TRUE) +
  RMColors +
  scale_x_continuous(breaks=c(1900, 1920, 1940, 1960, 1980, 2000))

b <- ggplot(aes(cYr, tminAvg), data=yrAvgs) + geom_line(na.rm=TRUE) + geom_point(na.rm=TRUE) +
  ylab(expression(paste(Tmin, ~({}^o*F)))) + xlab("") +
  # geom_text(aes(x=1895, y= 13.5, label = "B")) +
  geom_smooth(method="lm", na.rm=TRUE)+
  geom_line(aes(cYr, rTmin), size=1.5, colour="brown", na.rm=TRUE) + 
  scale_x_continuous(breaks=c(1900, 1920, 1940, 1960, 1980, 2000))

c <- ggplot(aes(cYr, tmeanAvg), data=yrAvgs) + geom_line(na.rm=TRUE) + geom_point(na.rm=TRUE) +
  ylab(expression(paste(Tmean, ~({}^o*F)))) + xlab("") +
  # geom_text(aes(x=1895, y= 13.5, label = "B")) +
  geom_smooth(method="lm", na.rm=TRUE)+
  geom_line(aes(cYr, rTmean), size=1.5, colour="brown", na.rm=TRUE) +
  scale_x_continuous(breaks=c(1900, 1920, 1940, 1960, 1980, 2000))

d <- ggplot(aes(cYr, pptAvg), data=yrAvgs) + geom_line(na.rm=TRUE) + geom_point(na.rm=TRUE) +
  ylab("Precip (in/yr)") + xlab("") +
  # geom_text(aes(x=1895, y=350, label = "C")) +
  geom_smooth(method="lm", na.rm=TRUE)+
  geom_line(aes(cYr, rPpt), size=1.5, colour="brown", na.rm=TRUE) + 
  scale_x_continuous(breaks=c(1900, 1920, 1940, 1960, 1980, 2000))

p1 = plot_grid(a, b, c, d, nrow=4, align="v")
title = ggdraw() + draw_label(paste(SiteID, " - Annual Means and Trends", sep=""), 
                              fontface="bold", size=TitleSize, vjust=0.5)
legend = get_legend(a + theme(legend.position = "bottom"))
p2 = plot_grid(title, p1, legend, ncol=1, rel_heights=c(0.05, 1, .05))
p3 = add_sub(p2, paste("Gray shaded area around regression lines = standard error of predicted y's \nData range = ", BeginYr, "-", EndYr, sep=""), 
             y=0.5, hjust=0.5, vjust=0.5, size=12)
ggdraw(p3)

OFName = paste(OFDir, "/PRISM ", PlotName, " ", SiteID, " ", Lat, " ", Lon, ".png", sep = "")
ggsave(OFName, width=6.5, height=8.5, dpi=dpi)

##########################


#-----------------------------------------------------------#
#            ANNUAL AVERAGE LINES WITH REGRESSION           #
#-----------------------------------------------------------#
		#    ggplot graphics    #
		# annaul points, linear regression, and 95% CI
		# need to manually set position of a, b, c labels

		# gray zone is 95% confidence interval

PlotName = "Annual Means Lines Regressions"

a <- ggplot(yrAvgs) +		#  attach only data frame in ggplot call
			geom_smooth(method = lm, aes(cYr, tmaxAvg), na.rm=TRUE) +
			geom_line(aes(cYr, tmaxAvg), na.rm=TRUE) + geom_point(aes(cYr, tmaxAvg), na.rm=TRUE) +
      ylab(expression(paste(Tmax, ~({}^o*F)))) + xlab("") +
      scale_x_continuous(breaks=c(1900, 1920, 1940, 1960, 1980, 2000))
          #	geom_line(aes(cYr, rTmax), colour = 'red', size=1)}  # rolling mean

      if(doP1 == "YES")a <- a + geom_smooth(method = lm, aes(cYr, tmaxP1), na.rm=TRUE)
      if(doP2 == "YES")a <- a + geom_smooth(method = lm, aes(cYr, tmaxP2), na.rm=TRUE)

	
b <- ggplot(data=yrAvgs) + geom_line(aes(cYr, tminAvg), na.rm=TRUE) + geom_point(aes(cYr, tminAvg), na.rm=TRUE) +
		ylab(expression(paste(Tmin, ~({}^o*F)))) + xlab("") +
		# geom_text(aes(x=1895, y= 13.5, label = "B")) +
		geom_smooth(aes(cYr, tminAvg), method="lm", na.rm=TRUE) +
    scale_x_continuous(breaks=c(1900, 1920, 1940, 1960, 1980, 2000))
        # geom_line(aes(cYr, rTmin), colour = 'blue', size=1)

      if(doP1 == "YES")(b <- b +	geom_smooth(method = lm, aes(cYr, tminP1), na.rm=TRUE))
			if(doP2 == "YES")b <- b +	geom_smooth(method = lm, aes(cYr, tminP2), na.rm=TRUE) 
						
c <- ggplot(data=yrAvgs) + geom_line(aes(cYr, tmeanAvg), na.rm=TRUE) + geom_point(aes(cYr, tmeanAvg), na.rm=TRUE) +
		ylab(expression(paste(Tmean, ~({}^o*F)))) + xlab("") +
		# geom_text(aes(x=1895, y= 13.5, label = "B")) +
		geom_smooth(aes(cYr, tmeanAvg), method="lm", na.rm=TRUE) +
		    # geom_line(aes(cYr, rTmean), colour = 'black', size=1) 
    scale_x_continuous(breaks=c(1900, 1920, 1940, 1960, 1980, 2000))

    if(doP1 == "YES")c <- c + geom_smooth(method = lm, aes(cYr, tmeanP1), na.rm=TRUE) 	
    if(doP2 == "YES") c <- c + geom_smooth(method = lm, aes(cYr, tmeanP2), na.rm=TRUE) 

d <- ggplot(data=yrAvgs) + geom_line(aes(cYr, pptAvg), na.rm=TRUE) + geom_point(aes(cYr, pptAvg), na.rm=TRUE) +
    ylab("Precip (in/yr)") + xlab("") +
		# geom_text(aes(x=1895, y=350, label = "C")) +
		geom_smooth(aes(cYr, pptAvg), method="lm", na.rm=TRUE) +
		    #geom_line(aes(cYr, rPpt), colour = 'green', size=1) 
    scale_x_continuous(breaks=c(1900, 1920, 1940, 1960, 1980, 2000))

    if(doP1 == "YES")d <- d + geom_smooth(method = lm, aes(cYr, pptP1), na.rm=TRUE)
    if(doP2 == "YES")d <- d + geom_smooth(method = lm, aes(cYr, pptP2), na.rm=TRUE) 

#4-panel plot		
p1 = plot_grid(a, b, c, d, nrow=4, align="v")
title = ggdraw() + draw_label(paste(SiteID, " - Trends for Reference and Recent \nHistorical Periods", sep=""), 
                              fontface="bold", size=TitleSize, vjust=0.5)
p2 = plot_grid(title, p1, ncol=1, rel_heights = c(0.07, 1)) 
p3 = add_sub(p2, paste("Gray shaded area around regression lines = standard error of predicted y's \nReference period: ", beginRefYr, "-", endRefYr, "; Recent period: ", endRefYr+1, "-", EndYr, "; Overall period: ", BeginYr, "-", EndYr, sep=""),
             y=.5, hjust=0.5, vjust=0.5, size=12)
ggdraw(p3)

OFName <- paste(OFDir, "/PRISM ", PlotName, " 4-panel ", SiteID, " ", Lat, " ", Lon, ".png", sep = "")
ggsave(OFName, width=6.5, height=8.5, dpi=dpi)

#2-panel Tmax/Tmin plot
p1 = plot_grid(a, b, nrow=2, align="v")
p2 = plot_grid(title, p1, ncol=1, rel_heights = c(0.1, 1, 0.05)) 
p3 = add_sub(p2, paste("Gray shaded area around regression lines = standard error of predicted y's \nReference period: ", beginRefYr, "-", endRefYr, "; Recent period: ", endRefYr+1, "-", EndYr, "; Overall period: ", BeginYr, "-", EndYr, sep=""),
             y=.5, hjust=0.5, vjust=0.5, size=12)
ggdraw(p3)

OFName <- paste(OFDir, "/PRISM ", PlotName, " Tmin Tmax ", SiteID, " ", Lat, " ", Lon, ".png", sep = "")
ggsave(OFName, width=6.5, height=6.5, dpi=dpi)

#2-panel Tmean/Precip plot
p1 = plot_grid(c, d, nrow=2, align="v")
p2 = plot_grid(title, p1, ncol=1, rel_heights = c(0.1, 1, 0.05)) 
p3 = add_sub(p2, paste("Gray shaded area around regression lines = standard error of predicted y's \nReference period: ", beginRefYr, "-", endRefYr, "; Recent period: ", endRefYr+1, "-", EndYr, "; Overall period: ", BeginYr, "-", EndYr, sep=""),
             y=.5, hjust=0.5, vjust=0.5, size=12)
ggdraw(p3)

OFName <- paste(OFDir, "/PRISM ", PlotName, " Tmean Precip ", SiteID, " ", Lat, " ", Lon, ".png", sep = "")
ggsave(OFName, width=6.5, height=6.5, dpi=dpi)

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


      # need roundDown function to define axis ranges

SeasLineColors = scale_color_manual(name="Season: ", breaks=c("Fall", "Spring", "Summer", "Winter"),
                                values = c("brown", "darkgreen", "red", "blue"))

PlotName <- "Season Decadal Anomaly Lines" 
a <- ggplot(pdat, aes(x=startYr+5, y=tmax, shape=seas, colour=seas)) +
		#ylim(-1.75, 1.75) +
		geom_line(size=1) +
		SeasLineColors +
    scale_shape(name="Season: ") +
		geom_point(size=3.2) +
		ylab(expression(paste(Tmax, ~({}^o*F)))) + xlab("") +
    scale_x_continuous(breaks=c(1900, 1920, 1940, 1960, 1980, 2000))
		
b <- ggplot(pdat, aes(startYr+5, tmin, shape=seas, colour=seas)) +
		#ylim(-1.75, 1.75) +
		geom_line(size=1) +
		SeasLineColors +
    scale_shape(name="Season: ") +
		geom_point(size=3.2) +
		ylab(expression(paste(Tmin, ~({}^o*F)))) + xlab("") +
  scale_x_continuous(breaks=c(1900, 1920, 1940, 1960, 1980, 2000))
		
c <- ggplot(pdat, aes(startYr+5, tmean, shape=seas, colour=seas)) +
		#ylim(-1.75, 1.75) +
		geom_line(size=1) +
		SeasLineColors + 
    scale_shape(name="Season: ") +
    geom_point(size=3.2) +
		ylab(expression(paste(Tmean, ~({}^o*F)))) + xlab("") +
  scale_x_continuous(breaks=c(1900, 1920, 1940, 1960, 1980, 2000))
		
d <- ggplot(pdat, aes(startYr+5, ppt, shape=seas, colour=seas)) +
		#ylim(-325, 325) +
		geom_line(size=1) +
		SeasLineColors + 
    scale_shape(name="Season: ") +
    geom_point(size=3.2) +
		ylab("Precip (in/yr)") + xlab("") +
    scale_x_continuous(breaks=c(1900, 1920, 1940, 1960, 1980, 2000))
				
p1 = plot_grid(a, b, c, d, nrow=4, align="v")
title = ggdraw() + draw_label(paste(SiteID, "- Decadal Anomalies by Season"), 
                              fontface="bold", size=TitleSize, vjust=0.5)
legend = get_legend(a + theme(legend.position = "bottom"))
p2 = plot_grid(title, p1, legend, ncol=1, rel_heights = c(0.05, 1, 0.05)) 
p3 = add_sub(p2, paste("Anomaly = (Mean decadal value) - (mean of all decades) \n Decadal ranges = ", stepYrs, "-year steps from ", BeginYr, " to ", max(seasStepAvgs$startYr)+9, sep=""), y=0.5, hjust=0.5, size=12)
ggdraw(p3)

OFName <- paste(OFDir, "/PRISM ", PlotName, " ", SiteID, " ", Lat, " ", Lon, ".png", sep = "")
ggsave(OFName, width=6.5, height=8.5, dpi=dpi)


#------------------------------------------------------------#
#   Boxplots of anomolies  - from entire period of record
#------------------------------------------------------------#
	
	#  calc seasonal avg for each year then subtract ref mean	
tminSeas <- data.frame(tapply(baseData$tmin, list(baseData$yr,baseData$seas), FUN=mean))
tmaxSeas <- data.frame(tapply(baseData$tmax, list(baseData$yr,baseData$seas), FUN=mean))
tmeanSeas <- data.frame(tapply(baseData$tmean, list(baseData$yr,baseData$seas), FUN=mean))
pptSeas  <- data.frame(tapply(baseData$ppt, list(baseData$yr,baseData$seas),   FUN=mean))

  # need to swap this and next section so use tminSeas absolute value, then calc anomoly and plot			
	# departure from entire period of record
for(i in 1:4)
	{ 	tminSeas[,i] <- tminSeas[,i] - mean(tminSeas[,i])
	  	tmaxSeas[,i] <- tmaxSeas[,i] - mean(tmaxSeas[,i])
	  	tmeanSeas[,i] <- tmeanSeas[,i] - mean(tmeanSeas[,i])
		  pptSeas[,i] <- pptSeas[,i] - mean(pptSeas[,i])}

yrcat <- (round(unique(baseData$yr)/10)*10)   # year category (e.g., 1900,1900, 1910)
yrcat <- yrcat[1:(floor(length(yrcat)/10)*10)]  #remove incomplete decades at end of record
yLabPs <- length(unique(yrcat))

#Remove extra years from data
tmaxSeas <- tmaxSeas[1:(yLabPs*10),]
tminSeas <- tminSeas[1:(yLabPs*10),]
tmeanSeas <- tmeanSeas[1:(yLabPs*10),]
pptSeas <- pptSeas[1:(yLabPs*10),]

  #Melt data frame
GetDecadeSeasons = function(df){
  df$Year = as.numeric(row.names(df))
  df.m = melt(df, id="Year")
  colnames(df.m) = c("Year", "Season", "Var")
  df.m$Decade = (floor((df.m$Year+5)/10))*10
  df.m$Season = factor(df.m$Season, levels=c("Winter", "Spring" ,"Summer", "Fall"))
  return(df.m)
}

tmaxDecadeSeasons = GetDecadeSeasons(tmaxSeas)
tminDecadeSeasons = GetDecadeSeasons(tminSeas)
tmeanDecadeSeasons = GetDecadeSeasons(tmeanSeas)
pptDecadeSeasons = GetDecadeSeasons(pptSeas)

SeasBoxColors = scale_fill_manual(name="Season: ", values=c("lightblue", "lightgreen", "lightpink", "linen"))

a = ggplot(tmaxDecadeSeasons, aes(x=factor(Decade), y=Var, fill=Season)) + geom_boxplot() +
  SeasBoxColors + labs(x="", y=expression(paste("Tmax (", degree*F,")", sep="")))
b = ggplot(tminDecadeSeasons, aes(x=factor(Decade), y=Var, fill=Season)) + geom_boxplot() +
  SeasBoxColors + labs(x="", y=expression(paste("Tmin (", degree*F,")", sep="")))
c = ggplot(tmeanDecadeSeasons, aes(x=factor(Decade), y=Var, fill=Season)) + geom_boxplot() +
  SeasBoxColors + labs(x="", y=expression(paste("Tmean (", degree*F,")", sep="")))
d = ggplot(pptDecadeSeasons, aes(x=factor(Decade), y=Var, fill=Season)) + geom_boxplot() +
  SeasBoxColors + labs(x="", y="Ppt (in/year)")

PlotName <- "Seas Decadal Tmax Tmin Anomaly Box"
p1 = plot_grid(a, b, nrow=2, align="v")
title = ggdraw() + draw_label(paste(SiteID, "- Annual Anomalies by Season and Decade"), 
                              fontface="bold", size=TitleSize, vjust=0.5)
legend = get_legend(a + theme(legend.position = "bottom"))
p2 = plot_grid(title, p1, legend, ncol=1, rel_heights = c(0.1, 1, 0.05)) 
p3 = add_sub(p2, paste("Anomaly = (Mean annual value) - (mean of all years) \n Decadal ranges = ", stepYrs, "-year steps from ", BeginYr, " to ", max(tmaxDecadeSeasons$Decade+4), sep=""), y=0.5, hjust=0.5, size=12)
ggdraw(p3)

OFName <- paste(OFDir, "/PRISM ", PlotName, " ", SiteID, " ", Lat, " ", Lon, ".png", sep = "")
ggsave(OFName, width=8, height=6, dpi=dpi)

PlotName <- "Seas Decadal Tmean Ppt Anomaly Box"
p1 = plot_grid(c, d, nrow=2, align="v")
title = ggdraw() + draw_label(paste(SiteID, "- Annual Anomalies by Season and Decade"), 
                              fontface="bold", size=TitleSize, vjust=0.5)
legend = get_legend(a + theme(legend.position = "bottom"))
p2 = plot_grid(title, p1, legend, ncol=1, rel_heights = c(0.1, 1, 0.05)) 
p3 = add_sub(p2, paste("Anomaly = (Mean annual value) - (Mean of all years) \n Decadal ranges = ", stepYrs, "-year steps from ", BeginYr, " to ", max(tmaxDecadeSeasons$Decade+4), sep=""), y=0.5, hjust=0.5, size=12)
ggdraw(p3)

OFName <- paste(OFDir, "/PRISM ", PlotName, " ", SiteID, " ", Lat, " ", Lon, ".png", sep = "")
ggsave(OFName, width=8, height=6, dpi=dpi)

	
#-------------------------------------------------#
#     Box Plots - Decadal Avgs Annual & Seasonal
#-------------------------------------------------#
nSteps = floor((EndYr - BeginYr)/stepYrs)	# year steps 
stepVal <- rep(0,nSteps)
midVal <- floor(stepYrs/2)

for(i in 0:(nSteps-1))
  stepVal[(1+i*stepYrs):((i+1)*stepYrs)] <- rep(midVal + i*stepYrs + BeginYr, stepYrs)

stepData <- yrAvgs[yrAvgs$cYr >= BeginYr,]
stepData <- stepData[1:length(stepVal),]
stepData$stepVal <- stepVal


PlotName <- "Decadal Avg Tmin Tmax Tmean Ppt Box"

a = ggplot(stepData, aes(x=factor(stepVal), y=tmaxAvg)) + geom_boxplot(fill="red") +
  labs(x="", y=expression(paste("Tmax (", degree*F,")", sep=""))) +
  scale_x_discrete(breaks=c("1900", "1920", "1940", "1960", "1980", "2000"))
b = ggplot(stepData, aes(x=factor(stepVal), y=tminAvg)) + geom_boxplot(fill="blue") +
  labs(x="", y=expression(paste("Tmin (", degree*F,")", sep=""))) +
  scale_x_discrete(breaks=c("1900", "1920", "1940", "1960", "1980", "2000")) 
c = ggplot(stepData, aes(x=factor(stepVal), y=tminAvg)) + geom_boxplot(fill="tan") +
  labs(x="", y=expression(paste("Tmean (", degree*F,")", sep=""))) +
  scale_x_discrete(breaks=c("1900", "1920", "1940", "1960", "1980", "2000"))
d = ggplot(stepData, aes(x=factor(stepVal), y=pptAvg)) + geom_boxplot(fill="light blue") + 
  labs(x="", y="Precip (in/yr)") +
  scale_x_discrete(breaks=c("1900", "1920", "1940", "1960", "1980", "2000"))

p1 = plot_grid(a,b,c,d, nrow=2, ncol=2, align="v")
title = ggdraw() + draw_label(paste(SiteID, "- Annual Climate Means By Decade"), 
                              fontface="bold", size=TitleSize, vjust=0.5)
p2 = plot_grid(title, p1, ncol=1, rel_heights = c(0.05, 1)) 
p3 = add_sub(p2, paste("Decadal ranges = ", stepYrs, "-year steps from ", BeginYr, " to ", max(tmaxDecadeSeasons$Decade+4), sep=""), y=0.5, hjust=0.5, size=12)
ggdraw(p3)
OFName = paste(OFDir, "/PRISM ", PlotName, " ", SiteID, " ", Lat, " ", Lon, ".png", sep = "")
ggsave(OFName, width=8.5, height=8.5)


		####  Period (e.g. decadal) averages for all seasons  #####

##  ========= Decadal values by season
PlotName = "Decadal Seasonal Tmax Box"
p1 = ggplot(tmaxDecadeSeasons, aes(x=as.character(Decade), y=Var, fill=Season)) + geom_boxplot() +
  facet_wrap(~ Season, nrow=2, ncol=2) +
  SeasBoxColors + 
  labs(x="", y=expression(paste("Maximum Temperature (", degree*F,")", sep="")), title=paste(SiteID, "- Annual Tmax Anomalies by Decade and Season")) +
  theme(strip.text = element_text(size=16)) + 
  scale_x_discrete(breaks=c("1900", "1920", "1940", "1960", "1980", "2000"))
p2 = add_sub(p1, paste("Anomaly = (Mean annual value) - (Mean of all years) \n Decadal ranges = ", stepYrs, "-year steps from ", BeginYr, " to ", max(tmaxDecadeSeasons$Decade+4), sep=""), y=0.5, hjust=0.5, size=12)
ggdraw(p2)
OFName = paste(OFDir, "/PRISM ", PlotName, " ", SiteID, " ", Lat, " ", Lon, ".png", sep = "")
ggsave(OFName, width=8.5, height=8.5)

PlotName = "Decadal Seasonal Tmin Box"
p1 = ggplot(tminDecadeSeasons, aes(x=as.character(Decade), y=Var, fill=Season)) + geom_boxplot() +
  facet_wrap(~ Season, nrow=2, ncol=2) +
  SeasBoxColors + 
  labs(x="", y=expression(paste("Minimum Temperature (", degree*F,")", sep="")), title=paste(SiteID, "- Annual Tmin Anomalies by Decade and Season")) + 
  theme(strip.text = element_text(size=16)) + 
  scale_x_discrete(breaks=c("1900", "1920", "1940", "1960", "1980", "2000"))
p2 = add_sub(p1, paste("Anomaly = (Mean annual value) - (Mean of all years) \n Decadal ranges = ", stepYrs, "-year steps from ", BeginYr, " to ", max(tmaxDecadeSeasons$Decade+4), sep=""), y=0.5, hjust=0.5, size=12)
ggdraw(p2)
OFName = paste(OFDir, "/PRISM ", PlotName, " ", SiteID, " ", Lat, " ", Lon, ".png", sep = "")
ggsave(OFName, width=8.5, height=8.5)

PlotName = "Decadal Seasonal Tmean Box"
p1 = ggplot(tmeanDecadeSeasons, aes(x=as.character(Decade), y=Var, fill=Season)) + geom_boxplot() +
  facet_wrap(~ Season, nrow=2, ncol=2) +
  SeasBoxColors + 
  labs(x="", y=expression(paste("Mean Temperature (", degree*F,")", sep="")), title=paste(SiteID, "- Annual Tmean Anomalies by Decade and Season")) + 
  theme(strip.text = element_text(size=16)) + 
  scale_x_discrete(breaks=c("1900", "1920", "1940", "1960", "1980", "2000"))
p2 = add_sub(p1, paste("Anomaly = (Mean annual value) - (Mean of all years) \n Decadal ranges = ", stepYrs, "-year steps from ", BeginYr, " to ", max(tmaxDecadeSeasons$Decade+4), sep=""), y=0.5, hjust=0.5, size=12)
ggdraw(p2)
OFName = paste(OFDir, "/PRISM ", PlotName, " ", SiteID, " ", Lat, " ", Lon, ".png", sep = "")
ggsave(OFName, width=8.5, height=8.5)

PlotName = "Decadal Seasonal Ppt Box"
p1 = ggplot(pptDecadeSeasons, aes(x=as.character(Decade), y=Var, fill=Season)) + geom_boxplot() +
  facet_wrap(~ Season, nrow=2, ncol=2) +
  SeasBoxColors + 
  labs(x="", y=expression("Precipitation (in/mon)"), title=paste(SiteID, "- Annual Precip Anomalies by Decade and Season")) + 
  theme(strip.text = element_text(size=16)) + 
  scale_x_discrete(breaks=c("1900", "1920", "1940", "1960", "1980", "2000"))
p2 = add_sub(p1, paste("Anomaly = (Mean annual value) - (Mean of all years) \n Decadal ranges = ", stepYrs, "-year steps from ", BeginYr, " to ", max(tmaxDecadeSeasons$Decade+4), sep=""), y=0.5, hjust=0.5, size=12)
ggdraw(p2)
OFName = paste(OFDir, "/PRISM ", PlotName, " ", SiteID, " ", Lat, " ", Lon, ".png", sep = "")
ggsave(OFName, width=8.5, height=8.5)


#--------------------------------------------------------------------------#
#       RED BLUE anomaly plots 
#--------------------------------------------------------------------------#

mtempMin = mean(yrAvgs$tminAvg)
mtempMax = mean(yrAvgs$tmaxAvg)
mtempMean = mean(yrAvgs$tmeanAvg)
mppt = mean(yrAvgs$pptAvg)

mTmin = mean(refData$tmin)
mTmax = mean(refData$tmax)
mTmean = mean(refData$tmean)
mPrcp = mean(refData$ppt)

Annual.Anomaly = data.frame(cYr = yrAvgs$cYr,
                            aTmax = yrAvgs$tmaxAvg - mtempMax,
                            aTmin = yrAvgs$tminAvg - mtempMin,
                            aTmean = yrAvgs$tmeanAvg - mtempMean,
                            aPpt = yrAvgs$pptAvg - mppt)
Annual.Anomaly$aTmax.col = ifelse(Annual.Anomaly$aTmax > 0, "red", "blue")
Annual.Anomaly$aTmin.col = ifelse(Annual.Anomaly$aTmin > 0, "red", "blue")
Annual.Anomaly$aTmean.col = ifelse(Annual.Anomaly$aTmean > 0, "red", "blue")
Annual.Anomaly$aPpt.col = ifelse(Annual.Anomaly$aPpt > 0, "green", "brown")

PlotName <- "Red-Blue Anomaly Bar plot"
  OFName <- paste(OFDir, "/PRISM ", PlotName, " ", SiteID, " ", Lat, " ", Lon, ".png", sep = "")

  # Tmax
a = ggplot(Annual.Anomaly, aes(x=cYr, y=aTmax, fill=aTmax.col)) + 
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("blue", "red")) +
  labs(x="", y=expression(paste("Tmax (", degree*F,")", sep=""))) + 
  scale_x_continuous(breaks=c(1900, 1920, 1940, 1960, 1980, 2000))

  #Tmin
b = ggplot(Annual.Anomaly, aes(x=cYr, y=aTmin, fill=aTmin.col)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("blue", "red")) +
  labs(x="", y=expression(paste("Tmin (", degree*F,")", sep=""))) +  
  scale_x_continuous(breaks=c(1900, 1920, 1940, 1960, 1980, 2000))

  #Tmean 
c = ggplot(Annual.Anomaly, aes(x=cYr, y=aTmean, fill=aTmean.col)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("blue", "red")) +
  labs(x="", y=expression(paste("Tmean (", degree*F,")", sep=""))) +
  scale_x_continuous(breaks=c(1900, 1920, 1940, 1960, 1980, 2000))

  #Precip
d = ggplot(Annual.Anomaly, aes(x=cYr, y=aPpt, fill=aPpt.col)) + 
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("brown", "green")) +
  labs(x="", y="Precip (in/yr)") +
  scale_x_continuous(breaks=c(1900, 1920, 1940, 1960, 1980, 2000))

p1 = plot_grid(a,b,c,d, nrow=4, align="v")
title = ggdraw() + draw_label(paste(SiteID, "- Annual Climate Anomalies"), 
                              fontface="bold", size=TitleSize, vjust=0.5)
p2 = plot_grid(title, p1, ncol=1, rel_heights = c(0.05, 1)) 
p3 = add_sub(p2, paste("Anomaly = (Annual value) - (Mean of all years) \nData range = ", BeginYr, "-", EndYr, sep=""), y=0.5, hjust=0.5, size=12)
ggdraw(p3)
OFName = paste(OFDir, "/PRISM ", PlotName, " ", SiteID, " ", Lat, " ", Lon, ".png", sep = "")
ggsave(OFName, width=6.5, height=8.5)

#-------------------------------------------------#
############  Decadal barplots   #############
#-------------------------------------------------#
head(baseData)
baseData$decade <- baseData$yr - baseData$yr %% 10
DecDat<-aggregate(cbind(tmin,tmax,tmean)~decade,baseData,mean,na.rm=TRUE)
DecDat1<-aggregate(ppt~yr,baseData,sum,na.rm=TRUE)
DecDat1$decade<-DecDat1$yr - DecDat1$yr %% 10
DecDat2<-aggregate(ppt~decade,DecDat1,mean,na.rm=TRUE)
DecDat<-merge(DecDat,DecDat2,by="decade"); rm(DecDat1,DecDat2)

DecDat$Atmin<-DecDat$tmin-mean(DecDat$tmin)
DecDat$Atmax<-DecDat$tmax-mean(DecDat$tmax)
DecDat$Atmean<-DecDat$tmean-mean(DecDat$tmean)
DecDat$Appt<-DecDat$ppt-mean(DecDat$ppt)
# DecDat$Appt[1]<-(DecDat$ppt[1]*2)-mean(DecDat$ppt)

DecDat$Atmax.col = ifelse(DecDat$Atmax > 0, "red", "blue")
DecDat$Atmin.col = ifelse(DecDat$Atmin > 0, "red", "blue")
DecDat$Atmean.col = ifelse(DecDat$Atmean > 0, "red", "blue")
DecDat$Appt.col = ifelse(DecDat$Appt > 0, "green", "brown")

PLOT<-"PRISM Decadal barplot - "
# Tmax
ggplot(DecDat, aes(x=decade, y=Atmax, fill=Atmax.col)) + 
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("blue", "red")) +
  labs(title="Average Decadal Max Temperature Climate Anomaly",x="", y=expression(paste("Tmax (", degree*F,")", sep=""))) + 
  scale_x_continuous(breaks=c(1900, 1920, 1940, 1960, 1980, 2000)) 
ggsave(paste(PLOT,"Tmax.png",sep=""),width = 15, height = 9)

#Tmin
ggplot(DecDat, aes(x=decade, y=Atmin, fill=Atmin.col)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("blue", "red")) +
  labs(title="Average Decadal Min Temperature Climate Anomaly",x="", y=expression(paste("Tmin (", degree*F,")", sep=""))) +  
  scale_x_continuous(breaks=c(1900, 1920, 1940, 1960, 1980, 2000))
ggsave(paste(PLOT,"Tmin.png",sep=""),width = 15, height = 9)

#Tmean 
ggplot(DecDat, aes(x=decade, y=Atmean, fill=Atmean.col)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("blue", "red")) +
  labs(title="Average Decadal Mean Temperature Climate Anomaly",x="", y=expression(paste("Tmean (", degree*F,")", sep=""))) +
  scale_x_continuous(breaks=c(1900, 1920, 1940, 1960, 1980, 2000))
ggsave(paste(PLOT,"Tmean.png",sep=""),width = 15, height = 9)

#Precip
ggplot(DecDat, aes(x=decade, y=Appt, fill=Appt.col)) + 
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("brown", "dark green")) +
  labs(title="Average Decadal Annual Precipitation Climate Anomaly",x="", y="Precip (in/yr)") +
  scale_x_continuous(breaks=c(1900, 1920, 1940, 1960, 1980, 2000))
ggsave(paste(PLOT,"Ppt.png",sep=""),width = 15, height = 9)


###### SPEI calculations
library(SPEI)
SPEI_per<-12 # Use 6-month SPEI 
truncation<- -0.75

head(baseData)
baseData$Tmean_C<-(baseData$tmean - 32) * 5/9
baseData$Precip<- baseData$ppt *25.4
baseData$PET<-thornthwaite(baseData$Tmean_C, Lat)
SPEI<-spei(baseData$Precip - baseData$PET, SPEI_per)
baseData$SPEI<-SPEI$fitted

spei<-subset(baseData,select=c(yr,mon,SPEI))
spei$date<-as.Date(paste(spei$yr,spei$mon,"01",sep="-"),format="%Y-%m-%d")

spei$col[spei$SPEI>=0]<-"wet"
spei$col[spei$SPEI<0]<-"dry"
spei$col<-factor(spei$col, levels=c("wet","dry"))

ggplot(data = spei, aes(x = date, y = SPEI,fill = col)) + 
  geom_bar(stat="identity",aes(fill=col)) +
  scale_fill_manual(name="",values =c("blue","red")) +
  theme(axis.text=element_text(size=20),axis.title.x=element_text(size=20,vjust=-0.2),
        axis.title.y=element_text(size=18,vjust=0.8),
        plot.title=element_text(size=18,face="bold",vjust=2,hjust=0.5),
        legend.text=element_text(size=20), legend.title=element_text(size=20)) +
  labs(title = "SPEI values for Historical Period", 
       x = "Date", y = "SPEI") +
  guides(color=guide_legend(override.aes = list(size=7))) +
  scale_y_continuous(limits=c(min(spei$SPEI)-.5, max(spei$SPEI)+1))
ggsave("Historical SPEI.png",width = 15, height = 9)


### EOF ###

