library(popcycle)
set.evt.location("/Volumes/seaflow/CMOP_6")
set.project.location("~/CMOP_2013_f2")
set.cruise.id("CMOP_6")
cruise <-"CMOP_6"



##################################
##### making aux data tables #####
##################################

## fluorescence (Algae Watch) ## 

# pre.flu1 <- read.csv("/Users/francois/CMOP/auxillary_data/saturn03.240.A.AlgaeWatch_2013_09_PD1.csv")
# pre.flu2 <- read.csv("/Users/francois/CMOP/auxillary_data/saturn03.240.A.AlgaeWatch_2013_10_PD1.csv")
# pre.flu1 <- as.data.frame(pre.flu1, row.names=NULL)
# pre.flu2 <- as.data.frame(pre.flu2, row.names=NULL)
# flu<- rbind(pre.flu1, pre.flu2)

# flu$time <- as.POSIXct(strptime(flu$time, "%Y/%m/%d %H:%M:%S"), tz="GMT")

# write.csv(flu, paste("/Users/francois/CMOP/auxillary_data/fluorescence",cruise, sep=""), quote=F, row.names=F)



# ## fluorescence (CDOM) ## 

# pre.flu1 <- read.csv("/Users/francois/CMOP/auxillary_data/saturn03.240.A.CDOM_Fluorometer_2013_09_PD1.csv")
# pre.flu2 <- read.csv("/Users/francois/CMOP/auxillary_data/saturn03.240.A.CDOM_Fluorometer_2013_10_PD1.csv")
# pre.flu1 <- as.data.frame(pre.flu1, row.names=NULL)
# pre.flu2 <- as.data.frame(pre.flu2, row.names=NULL)
# flu<- rbind(pre.flu1, pre.flu2)

# flu$time <- as.POSIXct(strptime(flu$time, "%Y/%m/%d %H:%M:%S"), tz="GMT")

# write.csv(flu, paste("/Users/francois/CMOP/auxillary_data/CDOM_fluorescence",cruise, sep=""), quote=F, row.names=F)



# ## nitrate ## 

# pre.nit1 <- read.csv("/Users/francois/CMOP/auxillary_data/saturn03.240.A.ISUS_2013_09_PD1.csv")
# pre.nit2 <- read.csv("/Users/francois/CMOP/auxillary_data/saturn03.240.A.ISUS_2013_10_PD1.csv")
# pre.nit1 <- as.data.frame(pre.nit1, row.names=NULL)
# pre.nit2 <- as.data.frame(pre.nit2, row.names=NULL)
# nit<- rbind(pre.nit1, pre.nit2)

# nit$time <- as.POSIXct(strptime(nit$time, "%Y/%m/%d %H:%M:%S"), tz="GMT")

# write.csv(nit, paste("/Users/francois/CMOP/auxillary_data/nitrate",cruise, sep=""), quote=F, row.names=F)



# ## water temperature (Thermistor) ##

# pre.temp1 <- read.csv("/Users/francois/CMOP/auxillary_data/saturn03.240.A.Thermistor_2013_09_PD1.csv")
# pre.temp2 <- read.csv("/Users/francois/CMOP/auxillary_data/saturn03.240.A.Thermistor_2013_10_PD1.csv")
# pre.temp1 <- as.data.frame(pre.temp1, row.names=NULL)
# pre.temp2 <- as.data.frame(pre.temp2, row.names=NULL)
# temp<- rbind(pre.temp1, pre.temp2)

# temp$time <- as.POSIXct(strptime(temp$time, "%Y/%m/%d %H:%M:%S"), tz="GMT")

# write.csv(temp, paste("/Users/francois/CMOP/auxillary_data/water_temp",cruise, sep=""), quote=F, row.names=F)



# ## salinity ##

# pre.sal1 <- read.csv("/Users/francois/CMOP/auxillary_data/saturn03.240.A.CT_2013_09_PD1.csv")
# pre.sal2 <- read.csv("/Users/francois/CMOP/auxillary_data/saturn03.240.A.CT_2013_10_PD1.csv")
# pre.sal1 <- as.data.frame(pre.sal1, row.names=NULL)
# pre.sal2 <- as.data.frame(pre.sal2, row.names=NULL)
# sal<- rbind(pre.sal1, pre.sal2)

# sal$time <- as.POSIXct(strptime(sal$time, "%Y/%m/%d %H:%M:%S"), tz="GMT")

# write.csv(sal, paste("/Users/francois/CMOP/auxillary_data/salinity",cruise, sep=""), quote=F, row.names=F)



# ## oxygen ##

# pre.ox1 <- read.csv("/Users/francois/CMOP/auxillary_data/saturn03.240.A.Oxygen_2013_09_PD1.csv")
# pre.ox2 <- read.csv("/Users/francois/CMOP/auxillary_data/saturn03.240.A.Oxygen_2013_10_PD1.csv")
# pre.ox1 <- as.data.frame(pre.ox1, row.names=NULL)
# pre.ox2 <- as.data.frame(pre.ox2, row.names=NULL)
# ox<- rbind(pre.ox1, pre.ox2)

# ox$time <- as.POSIXct(strptime(ox$time, "%Y/%m/%d %H:%M:%S"), tz="GMT")

# write.csv(ox, paste("/Users/francois/CMOP/auxillary_data/oxygen",cruise, sep=""), quote=F, row.names=F)



# ## pH ##

# pre.pH1 <- read.csv("/Users/francois/CMOP/auxillary_data/saturn03.240.A.pH_2013_09_PD1.csv")
# pre.pH2 <- read.csv("/Users/francois/CMOP/auxillary_data/saturn03.240.A.pH_2013_10_PD1.csv")
# pre.pH1 <- as.data.frame(pre.pH1, row.names=NULL)
# pre.pH2 <- as.data.frame(pre.pH2, row.names=NULL)
# pH<- rbind(pre.pH1, pre.pH2)

# pH$time <- as.POSIXct(strptime(pH$time, "%Y/%m/%d %H:%M:%S"), tz="GMT")

# write.csv(pH, paste("/Users/francois/CMOP/auxillary_data/pH",cruise, sep=""), quote=F, row.names=F)



# ## turbidity ##

# pre.tur1 <- read.csv("/Users/francois/CMOP/auxillary_data/saturn03.240.A.Turbidity_2013_09_PD1.csv")
# pre.tur2 <- read.csv("/Users/francois/CMOP/auxillary_data/saturn03.240.A.Turbidity_2013_10_PD1.csv")
# pre.tur1 <- as.data.frame(pre.tur1, row.names=NULL)
# pre.tur2 <- as.data.frame(pre.tur2, row.names=NULL)
# tur<- rbind(pre.tur1, pre.tur2)

# tur$time <- as.POSIXct(strptime(tur$time, "%Y/%m/%d %H:%M:%S"), tz="GMT")

# write.csv(tur, paste("/Users/francois/CMOP/auxillary_data/turbidity",cruise, sep=""), quote=F, row.names=F)


# ## bulk orange ##

# pre.tur1 <- read.csv("/Users/francois/CMOP/auxillary_data/saturn03.240.A.CyanoWatch_2013_09_PD1.csv")
# pre.tur2 <- read.csv("/Users/francois/CMOP/auxillary_data/saturn03.240.A.CyanoWatch_2013_10_PD1.csv")
# pre.tur1 <- as.data.frame(pre.tur1, row.names=NULL)
# pre.tur2 <- as.data.frame(pre.tur2, row.names=NULL)
# tur<- rbind(pre.tur1, pre.tur2)

# tur$time <- as.POSIXct(strptime(tur$time, "%Y/%m/%d %H:%M:%S"), tz="GMT")

# write.csv(tur, paste("/Users/francois/CMOP/auxillary_data/bulk_orange",cruise, sep=""), quote=F, row.names=F)



## bulk orange ##

pre.tur1 <- read.csv("/Users/francois/CMOP/auxillary_data/dsdma.-1000.A.MET_2013_09_PD1.csv")
pre.tur2 <- read.csv("/Users/francois/CMOP/auxillary_data/dsdma.-1000.A.MET_2013_10_PD1.csv")
pre.tur1 <- as.data.frame(pre.tur1, row.names=NULL)
pre.tur2 <- as.data.frame(pre.tur2, row.names=NULL)
tur<- rbind(pre.tur1, pre.tur2)

tur$time <- as.POSIXct(strptime(tur$time, "%Y/%m/%d %H:%M:%S"), tz="GMT")

write.csv(tur, paste("/Users/francois/CMOP/auxillary_data/par",cruise, sep=""), quote=F, row.names=F)

cruise<-"CMOP_6"



##################################
##### making super rad plots #####
##################################

stat <- get.stat.table() # to load the aggregate statistics
stat$time <- as.POSIXct(stat$time,format="%FT%T",tz='GMT')

# subseting files #
pre.crypto1 <- subset(stat, pop == 'crypto') 
id <- which(pre.crypto1$flow_rate < 2400) #subset files that have low flow rate
pre.crypto2 <- pre.crypto1[-id,]
crypto <- subset(pre.crypto2, time > as.POSIXct("2013-09-16 19:55:00") & time < as.POSIXct("2013-09-26 00:50:00")) 

#week1: 2013-09-10 16:50:00 - 2013-09-13 16:00:00
#week2: 2013-09-16 19:55:00 - 2013-09-20 00:00:00
#week3: 2013-09-23 22:50:00 - 2013-09-27 10:10:00
#week4: 2013-09-30 18:55:00 - 2013-10-03 23:58:00


plot(crypto$time, crypto$abundance,ylim=c(0,0.8), pch=16, xlab="time", ylab="abundance (10^6 cells/L)", main="Cryptophyte", cex.main=2, cex.lab=1.5)
par(mai=c(1,1,1,1))


pre.flu <- read.csv("/Users/francois/CMOP/auxillary_data/parCMOP_6")
pre.flu2 <- as.data.frame(pre.flu, row.names=NULL)
pre.flu2$time <- as.POSIXct(strptime(pre.flu2$time.YYYY.MM.DD.hh.mm.ss.PST., "%Y/%m/%d %H:%M:%S"), tz="GMT")
flu <- subset(pre.flu2, time > as.POSIXct("2013-09-16 19:55:00") & time < as.POSIXct("2013-09-20 00:00:00"))
#2013-09-26 00:50:00

plot(flu$time, flu$water_salinity, col="blue", pch=16, xlab="time", ylab="bulk orange fluorescence", cex.lab=1.5)
par(mai=c(1,1,1,1))

png(filename="/Users/francois/CMOP/ASLO/figure_making/rough_plots/PE.png")
dev.off()


## double plot ##

plot(crypto$time, crypto$abundance,ylim=c(0,0.8), pch=16, xlab="time", ylab="abundance (10^6 cells/L)", main="Cryptophyte", cex.main=2, cex.lab=1.5)

par(new=T)

plot(flu$time, flu$water_salinity, col="blue", pch=16, xlab="time", cex.lab=1.5, axes=F)
par(mai=c(1,1,1,1))
axis(pch)


abline(v=as.POSIXct("2013-09-25 16:44:49", format="%Y-%m-%d %H:%M:%S"), lty="dashed")
abline(h=0.4)

#2013-09-24 05:00:00
#2013-09-24 16:44:49
#2013-09-25 05:00:00
#2013-09-25 16:44:49




#########################################
##### plotting div rate with stuff ######
#########################################


library(rgl)
library(ggplot2)
library(zoo)
library(plotrix)

yay <- read.csv("/Users/francois/CMOP/CMOP_field/crypto_HD_CMOP_6.binned.csv")

yay$daily.GRmean <- rollapply(data=yay$h.dr.mean, width=24, FUN=mean, na.rm=T, fill=NA)*24
yay$daily.GRsd <- rollapply(data=yay$h.dr.sd, width=24, FUN=mean, na.rm=T, fill=NA)*24

	#############################################
	plotCI(as.POSIXct(yay$h.time, origin="1970-01-01", tz='GMT'), yay$daily.GRmean, uiw= yay$daily.GRsd, sfrac=0, pch=16, 	xlab="time", ylab="daily div rate", main="mean daily div rate", cex.main=2, cex.lab=1.5)


	par(new=T)
	plot(Par4$time2, Par4$par.max, col="blue", pch=16, axes=F, type="o", xlab="", ylab="", cex.lab=1.5)	
	axis(4)
	mtext("PAR", side=4, line=3)
	###############################################


## single week ##
yay2 <- subset(yay, h.time > as.POSIXct("2013-09-16 19:55:00") & h.time < as.POSIXct("2013-09-20 00:00:00"))

plotCI(as.POSIXct(yay2$h.time, origin="1970-01-01", tz='GMT'), yay2$daily.GRmean, uiw= yay$daily.GRsd, sfrac=0, pch=16, xlab="time", ylab="daily div rate")

par(new=T)
plot(flu$time, flu$nitrate, col="blue", pch=16, axes=F)


#######################
## special par stuff ## 
#######################

in.dir <- out.dir <- "/Users/francois/CMOP/CMOP_field"
Par.path <- paste0(in.dir,"/Par_",cruise)
	Par <- read.csv(Par.path, sep=",")
	Par$time <- as.POSIXct(Par$time, format="%Y/%m/%d %H:%M:%S",  tz= "GMT")
	Par$num.time <- as.numeric(Par$time)


## subset days ## 

Par2 <- subset(Par, time > as.POSIXct("2013-09-10 16:50:00") & time < as.POSIXct("2013-10-03 23:58:00"))
#Par2$daily.mean <- rollmean(x=Par2$par, k=48, na.pad=T)


## make empty data frames ## 

Par3 <- data.frame(Date=as.Date(character()),File=character(), User=character(),stringsAsFactors=FALSE) 
Par3<- as.data.frame(matrix(data=NA, nrow=23, ncol=2))
colnames(Par3) <- c("par.mean", "time")

Par4<- as.data.frame(matrix(data=NA, nrow=23, ncol=2))
colnames(Par4) <- c("par.max", "time")


## get max PAR ## 

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

plot(Par4$time2, Par4$par.max, pch=16, col="red")
par(new=T)
plot(Par2$time, Par2$par, pch=16)



##################################
#### plotting the div vs. PAR ####
##################################


par(mai=c(1,1,1,1))
plotCI(as.POSIXct(yay$h.time, origin="1970-01-01", tz='GMT'), yay$daily.GRmean, uiw= yay$daily.GRsd, sfrac=0, pch=16, 	xlab="time", ylab="daily div rate", main="mean daily div rate vs. max PAR", cex.main=2, cex.lab=1.5)


	par(new=T)
	plot(Par4$time2, Par4$par.max, col="blue", pch=16, axes=F, type="o", xlab="", ylab="", cex.lab=1.5)	
	axis(4)
	mtext("PAR", side=4, line=3)

par(new=T)
plot(yay$h.time, yay$daily.par.mean, col="blue", pch=16, axes=F, type="o", xlab="", ylab="", cex.lab=1.5)

yay$daily.par.mean <- rollapply(data=yay$h4.par.mean, width=24, FUN=mean, na.rm=T, fill=NA)*24
yay$daily.par.sd <- rollapply(data=yay$h4.par.sd, width=24, FUN=mean, na.rm=T, fill=NA)*24


par(mai=c(1,1,1,1))
plot(as.POSIXct(yay$h.time, origin="1970-01-01", tz='GMT'), yay$h.dr.mean,  pch=16, 	xlab="time", ylab="daily div rate", main="mean daily div rate vs. max PAR", cex.main=2, cex.lab=1.5)



