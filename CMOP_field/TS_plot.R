########################
#### making TS plot ####
########################

library(popcycle)
set.evt.location("/Volumes/seaflow/CMOP_6")
set.project.location("~/CMOP_2013_f2")
set.cruise.id("CMOP_6")
cruise <-"CMOP_6"

library(rgl)
library(ggplot2)
library(zoo)
library(plotrix)

#### setting up abundance data ####

stat <- get.stat.table() # to load the aggregate statistics
stat$time <- as.POSIXct(stat$time,format="%FT%T",tz='GMT')

# subseting files #
pre.crypto1 <- subset(stat, pop == 'crypto') 
id <- which(pre.crypto1$flow_rate < 2400) #subset files that have low flow rate
pre.crypto2 <- pre.crypto1[-id,]
crypto <- subset(pre.crypto2, time > as.POSIXct("2013-09-10 16:50:00") & time < as.POSIXct("2013-09-20 00:00:00")) 


# roll mean abundance #
pre.crypto2$daily.mean <- rollapply(data=pre.crypto2$abundance, width=24, FUN=mean, na.rm=T, fill=NA)*24
crypto$daily.mean <- rollapply(data=crypto$abundance, width=24, FUN=mean, na.rm=T, fill=NA)*24


#### setting up salinity and temperature #### 

pre.flu <- read.csv("/Users/francois/CMOP/auxillary_data/salinityCMOP_6")
pre.flu2 <- as.data.frame(pre.flu, row.names=NULL)
pre.flu2$time <- as.POSIXct(strptime(pre.flu2$time.YYYY.MM.DD.hh.mm.ss.PST., "%Y/%m/%d %H:%M:%S"), tz="GMT")
ts <- subset(pre.flu2, time > as.POSIXct("2013-09-10 16:50:00") & time < as.POSIXct("2013-09-20 00:00:00"))






 require(plotrix, quietly=T)

  cols <- colorRampPalette(c("blue4","royalblue4","deepskyblue3", "seagreen3", "yellow", "orangered2","darkred"))
  sfl$date <- as.POSIXct(sfl$date,format="%FT%T",tz='GMT')

plot(sfl$ocean_tmp, sfl$salinity, col=cols(100)[cut(sfl$date,100)],pch=16,xlab=expression(paste("Temp (",degree,"C)")), ylab="Salinity (psu)",...)
    ylim <- par('usr')[c(3,4)]
    xlim <- par('usr')[c(1,2)]
   color.legend(xlim[2], ylim[1], xlim[2] + 0.02*diff(xlim), ylim[2], 
      legend=c("start","end"), rect.col=cols(100), gradient='y',align='rb',...)
mtext("Time", side=4, line=2,...)  

