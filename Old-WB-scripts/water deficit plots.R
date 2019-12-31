# water deficit plots vxx.R

# Plot results from Thoma's water balance model
# In csv file, rename column 'W-PET' to 'deficit' so there's no hyphen in the variable name

# v03 - changed y axis from water (precip) to ET, added density plot, cleaned up code - 28 June 2017

# ToDo - automate reading of variables from the deficit xlsx workbook so csv file is not needed.

library(ggplot2)
library(RColorBrewer)

RColorBrewer::display.brewer.all()

rm(list=ls())

setwd('D:/_RSS Climate/COLM/Data for Thoma')

ddata <- read.csv("deficitData.csv", header = T)


    # Define periods.  Must be a better way to do this.
ddata$per <- 'no date'
ddata$per[which(ddata$year>=1950)] <- '1950-1979'
ddata$per[which(ddata$year>=1980)] <- '1980-2009'
ddata$per[which(ddata$year>=2010)] <- '2010-2039'
ddata$per[which(ddata$year>=2040)] <- '2040-2069'
ddata$per[which(ddata$year>=2070)] <- '2070-2099'

      # converstion;  # implicit conversion of int to numerical and mm to inches
ddata$deficit <- -1*ddata$deficit
ddata$deficit <- ddata$deficit/25.4
ddata$etIn  <- ddata$sumet / 25.4      # total (sum) evapotranspiration

unique(ddata$per)   # check 

    
    ##  Subset data for plotting

cfName <- "HotWet"        #"HotWet"  "WarmDry" etc.  Must match names in input file
#cfName = "WarmDry"

cfdata <- subset(ddata, ddata$cf== cfName)   
plotData <- with(cfdata, data.frame(year, deficit, etIn, per))
plotData <- subset(plotData, plotData$per != '2010-2039')
rm(cfdata)

        #  define colors.  Can use ColorBrewer ramps - e.g. scale_colours_manual(values= brewer.pal(4, "YlOrRd"))
c4 <- c("black", "orange", "orangered", "darkred")                # 5 colors from yellow to dark red
# c5 = c("ffffb2", "#fecc5c", "#fd8d3c", "#f03b20", "#bd0026")    # ColorBrewer hex codes for 5 good colors

      ###  Line plot
      
basePlot <- ggplot(plotData, aes(x=deficit, y=etIn, colour=per)) + geom_point(size=3) + geom_smooth(method="lm", se=FALSE, size=2) +
            scale_color_manual(values=c4) +  
            scale_x_continuous(limits = c(10, 36)) + scale_y_continuous(limits = c(4, 22.5)) +
            labs(    y = "Annual Evapotranspiration (in)", x = "Annual moisture deficit (in)", colour = "Period") +
            theme(axis.text = element_text(size=16), axis.title = element_text(size=16), legend.text=element_text(size=14),
                legend.position = c(.8,.8))
basePlot
    
    ####  Density Plot

denPlot <-  ggplot(plotData, aes(x=deficit, fill=per)) + geom_density(alpha=0.2, na.rm=TRUE) + scale_colour_manual(values=c4) +
            scale_x_continuous(limits = c(10, 36)) + scale_y_continuous(limits = c(0, .225)) + theme(legend.position = "none") +
            labs( y = "Density",   x = "Annual moisture deficit (in)", fill = "Period") +
            theme(legend.position = c(.8,.8), axis.text = element_text(size=16), axis.title = element_text(size=16), 
                               legend.text=element_text(size=14))
denPlot


