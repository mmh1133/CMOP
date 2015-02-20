library(popcycle)
set.evt.location("/Volumes/seaflow/CMOP_6")
set.project.location("~/CMOP_2013_f2")
set.cruise.id("CMOP_6")
cruise <-"CMOP_6"

library(rgl)
library(ggplot2)
library(zoo)
library(plotrix)

###############################################################
#### all the setup for plotting everything you ever wanted ####
###############################################################

#### setting up abundance data ####

stat <- get.stat.table() # to load the aggregate statistics
stat$time <- as.POSIXct(stat$time,format="%FT%T",tz='GMT')

# subseting files #
pre.crypto1 <- subset(stat, pop == 'crypto') 
id <- which(pre.crypto1$flow_rate < 2400) #subset files that have low flow rate
pre.crypto2 <- pre.crypto1[-id,]
crypto <- subset(pre.crypto2, time > as.POSIXct("2013-09-23 22:50:00") & time < as.POSIXct("2013-09-26 00:50:00")) 



#### setting up salinity #### 

pre.flu <- read.csv("/Users/francois/CMOP/auxillary_data/salinityCMOP_6")
pre.flu2 <- as.data.frame(pre.flu, row.names=NULL)
pre.flu2$time <- as.POSIXct(strptime(pre.flu2$time.YYYY.MM.DD.hh.mm.ss.PST., "%Y/%m/%d %H:%M:%S"), tz="GMT")
flu <- subset(pre.flu2, time > as.POSIXct("2013-09-23 22:50:00") & time < as.POSIXct("2013-09-26 00:50:00"))




#### setting up binned data ####

<<<<<<< HEAD
yay <- read.csv("/Users/mariaham/CMOP/CMOP_field/crypto_HD_CMOP_6.binned.csv")
=======
yay <- read.csv("/Users/francois/CMOP/CMOP_field/crypto_HD_CMOP_6.binned.csv")
>>>>>>> b92b220ccfccc902f0629890da9e09a0e1bb77b1

yay$daily.GRmean <- rollapply(data=yay$h.dr.mean, width=24, FUN=mean, na.rm=T, fill=NA)*24
yay$daily.GRsd <- rollapply(data=yay$h.dr.sd, width=24, FUN=mean, na.rm=T, fill=NA)*24




#### setting up PAR data ####

<<<<<<< HEAD
in.dir <- out.dir <- "/Users/mariaham/CMOP/CMOP_field"
=======
in.dir <- out.dir <- "/Users/francois/CMOP/CMOP_field"
>>>>>>> b92b220ccfccc902f0629890da9e09a0e1bb77b1
Par.path <- paste0(in.dir,"/Par_",cruise)
	Par <- read.csv(Par.path, sep=",")
	Par$time <- as.POSIXct(Par$time, format="%Y/%m/%d %H:%M:%S",  tz= "GMT")
	Par$num.time <- as.numeric(Par$time)

Par2 <- subset(Par, time > as.POSIXct("2013-09-10 16:50:00") & time < as.POSIXct("2013-10-03 23:58:00"))
Par4<- as.data.frame(matrix(data=NA, nrow=23, ncol=2))
colnames(Par4) <- c("par.max", "time")

start <- as.POSIXct("2013-09-10 16:50:00")

for (i in 1:23)
{
	print(i)
	end <- start + 86400
	sub <- subset(Par2, Par2$time > start & Par2$time < end)
	
	if(nrow(sub) > 46){
		Par4$par.max[i] <- max(sub$par, na.rm=T)
		}else print(paste("error"))
	
		Par4$time[i] <- sub[which(sub$par == max(sub$par)), 'time']
	start <- end 
}

Par4$time2 <- as.POSIXct(Par4$time, origin="1970-01-01", tz='GMT')







##########################################################################################################################



########################
#### abundance plot ####
########################

par(mai=c(1,1.5,1,1))
<<<<<<< HEAD
plot(pre.crypto2$time, pre.crypto2$abundance, lwd=2, pch=16, xlab="time", ylab="abundance (10^6 cells/L)")
## how do I make an axis break (for y axis, so it doesn't look like a plot of nothing)?


=======
plot(pre.crypto2$time, pre.crypto2$abundance, lwd=2, pch=16, xlab="", ylab="abundance (10^6 cells/L)", cex.lab=2, ylim=c(0,1))
## how do I make an axis break (for y axis, so it doesn't look like a plot of nothing)?

gap.plot(pre.crypto2$time, pre.crypto2$abundance, gap = c(5,17), lwd=2, pch=16, xlab="time", ylab="abundance (10^6 cells/L)", ylim=c(0,20))

################################
#### one day abundance plot ####
################################

plot(crypto$time, crypto$abundance, lwd=2, pch=16, xlab="", ylab="abundance (10^6 cells/L)", cex.lab=1.5)
>>>>>>> b92b220ccfccc902f0629890da9e09a0e1bb77b1

#####################################
#### salinity vs. abundance plot ####
#####################################

par(mai=c(1,1.5,1,1))
<<<<<<< HEAD
plot(crypto$time, crypto$abundance, type="n", ylab="abundance (10^6 cells/L)", xlab="time")
=======
plot(crypto$time, crypto$abundance, type="n", ylab="abundance (10^6 cells/L)", xlab="", cex.lab=1.5)
points(smooth.spline(as.POSIXct(crypto$time, origin="1970-01-01", tz='GMT'), crypto$abundance, spar=0.5), lwd=2, pch=16, xlab="", ylab="", type="l", cex=5)
par(new=T)
plot(flu$time, flu$water_salinity, xlab="", ylab="", axes=F, type="n")
points(smooth.spline(as.POSIXct(flu$time, origin="1970-01-01", tz='GMT'), flu$water_salinity, spar=0.5), lwd=2, col="cyan4", pch=16, xlab="", ylab="", axes=F)
#type="l", cex=2, lty=2
axis(4)
mtext("salinity", side=4, line=3, cex=1.5)
legend(1380100000, 0.35, c("crypto abundance", "salinity"), lty=c(1,1), lwd=c(2.5,2.5), col=c("darkred", "darkblue"))
#legend not working probably due to time issue 



###########################
#### new sal vs. abund ####
###########################



plot(crypto$time, crypto$abundance, type="n", ylab="abundance (10^6 cells/L)", xlab="", cex.lab=1.5, axes=F)
>>>>>>> b92b220ccfccc902f0629890da9e09a0e1bb77b1
points(smooth.spline(as.POSIXct(crypto$time, origin="1970-01-01", tz='GMT'), crypto$abundance, spar=0.5), lwd=2, pch=16, xlab="", ylab="", type="l", cex=5)
par(new=T)
plot(flu$time, flu$water_salinity, xlab="", ylab="", axes=F, type="n")
points(smooth.spline(as.POSIXct(flu$time, origin="1970-01-01", tz='GMT'), flu$water_salinity, spar=0.5), lwd=2, col="cyan4", pch=16, xlab="", ylab="", axes=F)
#type="l", cex=2, lty=2
axis(4)
<<<<<<< HEAD
mtext("salinity", side=4, line=3)
=======
mtext("salinity", side=4, line=3, cex=1.5)
>>>>>>> b92b220ccfccc902f0629890da9e09a0e1bb77b1
legend(1380100000, 0.35, c("crypto abundance", "salinity"), lty=c(1,1), lwd=c(2.5,2.5), col=c("darkred", "darkblue"))
#legend not working probably due to time issue 



<<<<<<< HEAD
=======

>>>>>>> b92b220ccfccc902f0629890da9e09a0e1bb77b1
#######################
#### div rate plot ####
#######################

<<<<<<< HEAD
par(mai=c(1,1.1,1,1))
plotCI(as.POSIXct(yay$h.time, origin="1970-01-01", tz='GMT'), yay$daily.GRmean, uiw= yay$daily.GRsd, sfrac=0, pch=16, 	xlab="time", ylab="mean daily div rate", cex.main=2, cex.lab=1.5)
=======
plotCI(as.POSIXct(yay$h.time, origin="1970-01-01", tz='GMT'), yay$daily.GRmean, uiw= yay$daily.GRsd, sfrac=0, pch=16, 	xlab="time", ylab="daily div rate", main="mean daily div rate", cex.main=2, cex.lab=1.5)
>>>>>>> b92b220ccfccc902f0629890da9e09a0e1bb77b1
## IMPORTANT NOTE: I still can't figure out why the binned file is still the old one?
## the new div rate should be under 2.5 max 
## can you recommit the new file to git when you find it?




###############################
#### div rate vs. PAR plot ####
###############################


par(mai=c(1,1,1,1))
plotCI(as.POSIXct(yay$h.time, origin="1970-01-01", tz='GMT'), yay$daily.GRmean, uiw= yay$daily.GRsd, sfrac=0, pch=16, 	xlab="time", ylab="daily div rate", main="mean daily div rate vs. max PAR", cex.main=2, cex.lab=1.5)


	par(new=T)
	plot(Par4$time2, Par4$par.max, col="darkblue", pch=16, axes=F, type="o", xlab="", ylab="", cex.lab=1.5)	
	axis(4)
<<<<<<< HEAD
	mtext("PAR", side=4, line=3, cex.lab=1.5)
=======
	mtext("PAR", side=4, line=3)
>>>>>>> b92b220ccfccc902f0629890da9e09a0e1bb77b1



#####################################################################################################


##########################
#### non field data!! ####
##########################

## setup ##

library(lmodel2)
home <- "/Users/francois/CMOP/"
out.dir <- paste0(home, "Rhodo_labExperiment/")

m <- read.csv(paste0(out.dir,"model_output-V2.csv"))
cc <- read.csv(paste0(out.dir,"RHODO_div-rate.csv"))[-1,]

##################################
#### div rate of model vs. cc ####
##################################

par(pty='m')
plotCI(as.POSIXct(cc$time, origin="1970-01-01"), cc$div, cc$div.se, ylim=c(0,0.05), sfrac=0, lwd=2, pch=16, cex=1,ylab=NA, xlab=NA)
plotCI(m$time, m$div.ave, m$div.sd, col=2,add=T, sfrac=0, lwd=2, pch=16,cex=1)
mtext(substitute(paste("Division (h"^{-1},")")), side=2, line=3, cex=1)
mtext("time", side=1, line=3, cex=1)



















