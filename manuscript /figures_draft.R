library(popcycle)
set.evt.location("/Volumes/seaflow/CMOP_6")
set.project.location("~/CMOP_2013_f2")
set.cruise.id("CMOP_6")
cruise <-"CMOP_6"

library(rgl)
library(ggplot2)
library(zoo)
library(plotrix)

library(maps)
library(mapdata)
library(maptools)
library(scales)
library(mapproj)

library(marmap)


##################################################################
##################################################################
#####  												         #####
#####  all the setup for plotting everything you ever wanted #####
#####														 #####
##################################################################
##################################################################

#original 
#week1: 2013-09-10 16:50:00 - 2013-09-13 16:00:00
#week2: 2013-09-16 19:55:00 - 2013-09-20 00:00:00
#week3: 2013-09-23 22:50:00 - 2013-09-27 10:10:00
#week4: 2013-09-30 18:55:00 - 2013-10-03 23:58:00

#new
#week1: 2013-09-10 16:50:00 - 2013-09-13 24:00:00
#week2: 2013-09-14 00:00:00 - 2013-09-20 24:00:00
#week3: 2013-09-21 00:00:00 - 2013-09-28 24:00:00
#week4: 2013-09-29 00:00:00 - 2013-10-03 24:00:00

#new aux
#week1: 2013-09-10 16:50:00 - 2013-09-14 24:00:00
#week2: 2013-09-15 00:00:00 - 2013-09-21 24:00:00
#week3: 2013-09-22 00:00:00 - 2013-09-28 24:00:00
#week4: 2013-09-29 00:00:00 - 2013-10-03 24:00:00






#### setting up abundance data ####

stat <- get.stat.table() # to load the aggregate statistics
stat$time <- as.POSIXct(stat$time,format="%FT%T",tz='GMT')

# subseting files #
pre.crypto1 <- subset(stat, pop == 'crypto') 
id <- which(pre.crypto1$flow_rate < 2400) #subset files that have low flow rate
pre.crypto2 <- pre.crypto1[-id,]
crypto <- subset(pre.crypto2, time > as.POSIXct("2013-09-10 16:50:00") & time < as.POSIXct("2013-10-03 24:00:00")) 
crypto.week1 <- subset(pre.crypto2, time > as.POSIXct("2013-09-10 16:50:00") & time < as.POSIXct("2013-09-13 24:00:00")) 
crypto.week2 <- subset(pre.crypto2, time > as.POSIXct("2013-09-14 00:00:00") & time < as.POSIXct("2013-09-20 24:00:00")) 
crypto.week3 <- subset(pre.crypto2, time > as.POSIXct("2013-09-21 00:00:00") & time < as.POSIXct("2013-09-28 24:00:00")) 
crypto.week4 <- subset(pre.crypto2, time > as.POSIXct("2013-09-29 00:00:00") & time < as.POSIXct("2013-10-03 24:00:00")) 

# roll mean abundance #
pre.crypto2$daily.mean <- rollapply(data=pre.crypto2$abundance, width=24, FUN=mean, na.rm=T, fill=NA)*24
crypto$daily.mean <- rollapply(data=crypto$abundance, width=24, FUN=mean, na.rm=T, fill=NA)*24
crypto.week1$daily.mean <- rollapply(data=crypto.week1$abundance, width=24, FUN=mean, na.rm=T, fill=NA)*24
crypto.week2$daily.mean <- rollapply(data=crypto.week2$abundance, width=24, FUN=mean, na.rm=T, fill=NA)*24
crypto.week3$daily.mean <- rollapply(data=crypto.week3$abundance, width=24, FUN=mean, na.rm=T, fill=NA)*24
crypto.week4$daily.mean <- rollapply(data=crypto.week4$abundance, width=24, FUN=mean, na.rm=T, fill=NA)*24





#### setting up salinity #### 

pre.sal <- read.csv("/Users/francois/CMOP/auxillary_data/salinityCMOP_6")
pre.sal2 <- as.data.frame(pre.sal, row.names=NULL)
pre.sal2$time <- as.POSIXct(strptime(pre.sal2$time.YYYY.MM.DD.hh.mm.ss.PST., "%Y/%m/%d %H:%M:%S"), tz="GMT")
sal <- subset(pre.sal2, time > as.POSIXct("2013-09-10 16:50:00") & time < as.POSIXct("2013-10-03 24:00:00"))
sal.w1 <- subset(pre.sal2, time > as.POSIXct("2013-09-10 16:50:00") & time < as.POSIXct("2013-09-14 24:00:00"))
sal.w2 <- subset(pre.sal2, time > as.POSIXct("2013-09-15 00:00:00") & time < as.POSIXct("2013-09-21 24:00:00"))
sal.w3 <- subset(pre.sal2, time > as.POSIXct("2013-09-22 00:00:00") & time < as.POSIXct("2013-09-28 24:00:00"))
sal.w4 <- subset(pre.sal2, time > as.POSIXct("2013-09-29 00:00:00") & time < as.POSIXct("2013-10-03 24:00:00"))



#### setting up temperature ####

pre.temp <- read.csv("/Users/francois/CMOP/auxillary_data/water_tempCMOP_6")
pre.temp2 <- as.data.frame(pre.temp, row.names=NULL)
pre.temp2$time <- as.POSIXct(strptime(pre.temp2$time.YYYY.MM.DD.hh.mm.ss.PST., "%Y/%m/%d %H:%M:%S"), tz="GMT")
temp <- subset(pre.temp2, time > as.POSIXct("2013-09-10 16:50:00") & time < as.POSIXct("2013-10-03 24:00:00"))



#### setting up binned data ####

yay <- read.csv("/Users/francois/CMOP/CMOP_field/model/crypto_HD_CMOP_6V3.binned.csv")

yay$daily.GRmean <- rollapply(data=yay$h.dr.mean, width=24, FUN=mean, na.rm=T, fill=NA)*24
yay$daily.GRsd <- rollapply(data=yay$h.dr.sd, width=24, FUN=mean, na.rm=T, fill=NA)*24

yay$time <- as.POSIXct(yay$h.time,tz='GMT', origin="1970-01-01")
yay2 <- subset(yay, time > as.POSIXct("2013-09-10 16:50:00") & time < as.POSIXct("2013-09-20 00:00:00")) 
dr.w1 <- subset(yay, time > as.POSIXct("2013-09-10 16:50:00") & time < as.POSIXct("2013-09-14 24:00:00"))
dr.w2 <- subset(yay, time > as.POSIXct("2013-09-15 00:00:00") & time < as.POSIXct("2013-09-21 24:00:00"))
dr.w3 <- subset(yay, time > as.POSIXct("2013-09-22 00:00:00") & time < as.POSIXct("2013-09-28 24:00:00"))
dr.w4 <- subset(yay, time > as.POSIXct("2013-09-29 00:00:00") & time < as.POSIXct("2013-10-03 24:00:00"))


#### setting up individual days of binned data ###

day1 <- subset(yay, time > as.POSIXct("2013-09-11 00:00:00") & time < as.POSIXct("2013-09-12 00:00:00"))
day2 <- subset(yay, time > as.POSIXct("2013-09-12 00:00:00") & time < as.POSIXct("2013-09-13 00:00:00"))
day3 <- subset(yay, time > as.POSIXct("2013-09-13 00:00:00") & time < as.POSIXct("2013-09-14 00:00:00"))
day4 <- subset(yay, time > as.POSIXct("2013-09-14 00:00:00") & time < as.POSIXct("2013-09-15 00:00:00"))
day5 <- subset(yay, time > as.POSIXct("2013-09-15 00:00:00") & time < as.POSIXct("2013-09-16 00:00:00"))
day6 <- subset(yay, time > as.POSIXct("2013-09-16 00:00:00") & time < as.POSIXct("2013-09-17 00:00:00"))
day7 <- subset(yay, time > as.POSIXct("2013-09-17 00:00:00") & time < as.POSIXct("2013-09-18 00:00:00"))
day8 <- subset(yay, time > as.POSIXct("2013-09-18 00:00:00") & time < as.POSIXct("2013-09-19 00:00:00"))
day9 <- subset(yay, time > as.POSIXct("2013-09-19 00:00:00") & time < as.POSIXct("2013-09-20 00:00:00"))
day10 <- subset(yay, time > as.POSIXct("2013-09-20 00:00:00") & time < as.POSIXct("2013-09-21 00:00:00"))
day11 <- subset(yay, time > as.POSIXct("2013-09-21 00:00:00") & time < as.POSIXct("2013-09-22 00:00:00"))
day12 <- subset(yay, time > as.POSIXct("2013-09-22 00:00:00") & time < as.POSIXct("2013-09-23 00:00:00"))
day13 <- subset(yay, time > as.POSIXct("2013-09-23 00:00:00") & time < as.POSIXct("2013-09-24 00:00:00"))
day14 <- subset(yay, time > as.POSIXct("2013-09-24 00:00:00") & time < as.POSIXct("2013-09-25 00:00:00"))
day15 <- subset(yay, time > as.POSIXct("2013-09-25 00:00:00") & time < as.POSIXct("2013-09-26 00:00:00"))
day16 <- subset(yay, time > as.POSIXct("2013-09-26 00:00:00") & time < as.POSIXct("2013-09-27 00:00:00"))
day17 <- subset(yay, time > as.POSIXct("2013-09-27 00:00:00") & time < as.POSIXct("2013-09-28 00:00:00"))
day18 <- subset(yay, time > as.POSIXct("2013-09-28 00:00:00") & time < as.POSIXct("2013-09-29 00:00:00"))
day19 <- subset(yay, time > as.POSIXct("2013-09-29 00:00:00") & time < as.POSIXct("2013-09-30 00:00:00"))
day20 <- subset(yay, time > as.POSIXct("2013-09-30 00:00:00") & time < as.POSIXct("2013-10-01 00:00:00"))
day21 <- subset(yay, time > as.POSIXct("2013-10-01 00:00:00") & time < as.POSIXct("2013-10-02 00:00:00"))
day22 <- subset(yay, time > as.POSIXct("2013-10-02 00:00:00") & time < as.POSIXct("2013-10-03 00:00:00"))
day23 <- subset(yay, time > as.POSIXct("2013-10-03 00:00:00") & time < as.POSIXct("2013-10-04 00:00:00"))


d1m <- mean(day1$daily.GRmean, na.rm=T)
d2m <- mean(day2$daily.GRmean, na.rm=T)
d3m <- mean(day3$daily.GRmean, na.rm=T)
d4m <- mean(day4$daily.GRmean, na.rm=T)
d5m <- mean(day5$daily.GRmean, na.rm=T)  #NA
d6m <- mean(day6$daily.GRmean, na.rm=T)  #NA
d7m <- mean(day7$daily.GRmean, na.rm=T)
d8m <- mean(day8$daily.GRmean, na.rm=T)
d9m <- mean(day9$daily.GRmean, na.rm=T)
d10m <- mean(day10$daily.GRmean, na.rm=T) #NA
d11m <- mean(day11$daily.GRmean, na.rm=T) #NA
d12m <- mean(day12$daily.GRmean, na.rm=T) #NA
d13m <- mean(day13$daily.GRmean, na.rm=T)
d14m <- mean(day14$daily.GRmean, na.rm=T)  
d15m <- mean(day15$daily.GRmean, na.rm=T)  
d16m <- mean(day16$daily.GRmean, na.rm=T)
d17m <- mean(day17$daily.GRmean, na.rm=T)
d18m <- mean(day18$daily.GRmean, na.rm=T) #NA
d19m <- mean(day19$daily.GRmean, na.rm=T) #NA
d20m <- mean(day20$daily.GRmean, na.rm=T) #NA
d21m <- mean(day21$daily.GRmean, na.rm=T)
d22m <- mean(day22$daily.GRmean, na.rm=T)
d23m <- mean(day23$daily.GRmean, na.rm=T) #NA

dm <- c(d1m, d2m, d3m, d7m, d8m, d9m, d13m, d14m, d15m, d16m, d17m, d21m, d22m)



#### setting up PAR data ####

in.dir <- out.dir <- "/Users/francois/CMOP/CMOP_field"
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

Par5 <- subset(Par4, time > as.POSIXct("2013-09-10 16:50:00") & time < as.POSIXct("2013-09-20 00:00:00"))
Par.all <- subset(Par4, time > as.POSIXct("2013-09-10 16:50:00") & time < as.POSIXct("2013-10-04 00:00:00"))
Par.w1 <- subset(Par4, time > as.POSIXct("2013-09-10 16:50:00") & time < as.POSIXct("2013-09-14 24:00:00"))
Par.w2 <- subset(Par4, time > as.POSIXct("2013-09-15 00:00:00") & time < as.POSIXct("2013-09-21 24:00:00"))
Par.w3 <- subset(Par4, time > as.POSIXct("2013-09-22 00:00:00") & time < as.POSIXct("2013-09-28 24:00:00"))
Par.w4 <- subset(Par4, time > as.POSIXct("2013-09-29 00:00:00") & time < as.POSIXct("2013-10-03 24:00:00"))


#### setting up nutrient data ####

pre.flu <- read.csv("/Users/francois/CMOP/auxillary_data/Ribalet_nutrients2.csv")
pre.flu2 <- as.data.frame(pre.flu, row.names=NULL)
pre.flu2$time2 <- as.POSIXct(strptime(pre.flu2$time, "%Y/%m/%d %H:%M:%S"), tz="GMT")
pre.flu3 <- subset(pre.flu2, time2 > as.POSIXct("2013-09-10 16:50:00") & time2 < as.POSIXct("2013-09-20 00:00:00")) 
flu.w1 <- subset(pre.flu2, time2 > as.POSIXct("2013-09-10 16:50:00") & time2 < as.POSIXct("2013-09-14 24:00:00")) 
flu.w2 <- subset(pre.flu2, time2 > as.POSIXct("2013-09-15 00:00:00") & time2 < as.POSIXct("2013-09-21 24:00:00")) 
flu.w3 <- subset(pre.flu2, time2 > as.POSIXct("2013-09-22 00:00:00") & time2 < as.POSIXct("2013-09-28 24:00:00")) 
flu.w4 <- subset(pre.flu2, time2 > as.POSIXct("2013-09-29 00:00:00") & time2 < as.POSIXct("2013-10-03 24:00:00")) 



#picking out nitrate for div vs n and making data frame
n <- c(12.7, 13.3, 13.2, 7.1, 8.6, 5.9, 3.5, 5.0, 8.7, 10.3, 9.7, 11.6, 13.1) #nitrate
ph <- c(0.9, 1.1, 0.9, 0.9, 1.1, 0.6, 0.5, 0.6, 0.7, 0.7, 1.0, 0.4, 0.6) #phosphate
a <- c(5.0, 6.5, 5.1, 4.6, 5.3, 13.5, 2.3, 2.5, 3.6, 2.5, 8.1, 5.2, 5.6) #ammonium
cmop <- data.frame(dm, n, ph, a)


#### adding mesodinium counts ####

pre.meso <- read.csv("/Users/francois/CMOP/mesodinium/meso.csv")
meso <- as.data.frame(pre.meso, row.names=NULL)
meso$time2 <- as.POSIXct(strptime(meso$datetime, "%m/%d/%Y %H:%M:%S"), tz="GMT")
meso2 <- subset(meso, time2 > as.POSIXct("2013-09-10 16:50:00") & time2 < as.POSIXct("2013-09-20 00:00:00"))



#### TX qPCR data ####

pre.tx <- read.csv("/Users/francois/CMOP/pics_misc/tx_qPCR.csv")
tx <- as.data.frame(pre.tx, row.names=NULL)
tx$time2 <- as.POSIXct(strptime(tx$time, "%m/%d/%Y %H:%M:%S"), tz="GMT")




##########################################################################################################################




########################
#### aux data plots ####
########################


# full TC #
par(mar=c(5,12,4,12))
plot(sal$time, sal$water_salinity, lwd=2, pch=16, xlab="", ylab="salinity (psu?)", cex.lab=1, type="n")
points(smooth.spline(as.POSIXct(sal$time, origin="1970-01-01", tz='GMT'), sal$water_salinity, spar=0.5), lwd=2, col="cyan4", pch=16, xlab="", ylab="", axes=F)
par(new=T)
plot(sal$time, sal$water_temperature, lwd=2, pch=16, cex=1, xlab="", ylab="", cex.lab=1,  axes=F, col="lightblue", type="n")
points(smooth.spline(as.POSIXct(sal$time, origin="1970-01-01", tz='GMT'), sal$water_temperature, spar=0.5 ), lwd=2, pch=16, cex=1, xlab="", ylab="", axes=F, col="lightblue")
axis(2, line=4.5)
mtext(2, text="water temperature (degrees C)", line=6.5, col="lightblue")
par(new=T)
plot(pre.flu2$time2, pre.flu$Nitrate, lwd=2, pch=16, cex=1.5, xlab="", ylab="", axes=F, col="red")
axis(4)
mtext(4, text="nitrate (uM)", line=2, col="red")
par(new=T)
plot(Par.all$time, Par.all$par.max, lwd=2, pch=16, cex=1.5, xlab="", ylab="", axes=F, col="orange")
axis(4, line=3.5)
mtext(4, text="max PAR", line=5.5, col="orange")




# # par(mar=c(5,12,4,12))
# plot(sal$time, sal$water_salinity, lwd=2, pch=16, xlab="", ylab="salinity (psu?)", cex.lab=1, type="n")
# points(smooth.spline(as.POSIXct(sal$time, origin="1970-01-01", tz='GMT'), sal$water_salinity, spar=0.5), lwd=2, col="cyan4", pch=16, xlab="", ylab="", axes=F)
# par(new=T)
# plot(sal$time, sal$water_temperature, lwd=2, pch=16, cex=1, xlab="", ylab="", cex.lab=1,  axes=F, col="lightblue", type="n")
# points(smooth.spline(as.POSIXct(sal$time, origin="1970-01-01", tz='GMT'), sal$water_temperature, spar=0.5 ), lwd=2, pch=16, cex=1, xlab="", ylab="", axes=F, col="lightblue")
# axis(2, line=4.5)
# mtext(2, text="water temperature (degrees C)", line=6.5, col="lightblue")
# par(new=T)
# plot(pre.flu2$time2, pre.flu$Nitrate, lwd=2, pch=16, cex=1, xlab="", ylab="", axes=F, col="red")
# axis(4)
# mtext(4, text="nitrate (uM)", line=2, col="red")
# par(new=T)
# plot(Par2$time, Par2$par, lwd=2, pch=16, cex=1, xlab="", ylab="", axes=F, col="orange", type="n")
# points(smooth.spline(as.POSIXct(Par2$time, origin="1970-01-01", tz='GMT'), Par2$par, spar=0.5), lwd=2, pch=16, cex=1, xlab="", ylab="", axes=F, col="orange")
# axis(4, line=3.5)
# mtext(4, text="max PAR", line=5.5, col="orange")


# week 1 #
par(mar=c(5,12,4,12))
plot(sal.w1$time, sal.w1$water_salinity, lwd=2, pch=16, xlab="", ylab="salinity (psu?)", cex.lab=1, type="n", xaxt="n")
axis.POSIXct(1, sal.w1$time, format="%D")
points(smooth.spline(as.POSIXct(sal.w1$time, origin="1970-01-01", tz='GMT'), sal.w1$water_salinity, spar=0.5), lwd=2, col="cyan4", pch=16, xlab="", ylab="", axes=F)
par(new=T)
plot(sal.w1$time, sal.w1$water_temperature, lwd=2, pch=16, cex=1, xlab="", ylab="", cex.lab=1,  axes=F, col="lightblue", type="n")
points(smooth.spline(as.POSIXct(sal.w1$time, origin="1970-01-01", tz='GMT'), sal.w1$water_temperature, spar=0.5 ), lwd=2, pch=16, cex=1, xlab="", ylab="", axes=F, col="lightblue")
axis(2, line=4.5)
mtext(2, text="water temperature (degrees C)", line=6.5, col="lightblue")
par(new=T)
plot(flu.w1$time2, flu.w1$Nitrate, lwd=2, pch=16, cex=1.5, xlab="", ylab="", axes=F, col="red")
axis(4)
mtext(4, text="nitrate (uM)", line=2, col="red")
par(new=T)
plot(Par.w1$time, Par.w1$par.max, lwd=2, pch=16, cex=1.5, xlab="", ylab="", axes=F, col="orange")
axis(4, line=3.5)
mtext(4, text="max PAR", line=5.5, col="orange")


# week 2 #
par(mar=c(5,12,4,12))
plot(sal.w2$time, sal.w2$water_salinity, lwd=2, pch=16, xlab="", ylab="salinity (psu?)", cex.lab=1, type="n", xaxt="n")
axis.POSIXct(1, sal.w2$time, format="%D")
points(smooth.spline(as.POSIXct(sal.w2$time, origin="1970-01-01", tz='GMT'), sal.w2$water_salinity, spar=0.5), lwd=2, col="cyan4", pch=16, xlab="", ylab="", axes=F)
par(new=T)
plot(sal.w2$time, sal.w2$water_temperature, lwd=2, pch=16, cex=1, xlab="", ylab="", cex.lab=1,  axes=F, col="lightblue", type="n")
points(smooth.spline(as.POSIXct(sal.w2$time, origin="1970-01-01", tz='GMT'), sal.w2$water_temperature, spar=0.5 ), lwd=2, pch=16, cex=1, xlab="", ylab="", axes=F, col="lightblue")
axis(2, line=4.5)
mtext(2, text="water temperature (degrees C)", line=6.5, col="lightblue")
par(new=T)
plot(flu.w2$time2, flu.w2$Nitrate, lwd=2, pch=16, cex=1.5, xlab="", ylab="", axes=F, col="red")
axis(4)
mtext(4, text="nitrate (uM)", line=2, col="red")
par(new=T)
plot(Par.w2$time, Par.w2$par.max, lwd=2, pch=16, cex=1.5, xlab="", ylab="", axes=F, col="orange")
axis(4, line=3.5)
mtext(4, text="max PAR", line=5.5, col="orange")


# week 3 #
par(mar=c(5,12,4,12))
plot(sal.w3$time, sal.w3$water_salinity, lwd=2, pch=16, xlab="", ylab="salinity (psu?)", cex.lab=1, type="n", xaxt="n")
axis.POSIXct(1, sal.w3$time, format="%D")
points(smooth.spline(as.POSIXct(sal.w3$time, origin="1970-01-01", tz='GMT'), sal.w3$water_salinity, spar=0.5), lwd=2, col="cyan4", pch=16, xlab="", ylab="", axes=F)
par(new=T)
plot(sal.w3$time, sal.w3$water_temperature, lwd=2, pch=16, cex=1, xlab="", ylab="", cex.lab=1,  axes=F, col="lightblue", type="n")
points(smooth.spline(as.POSIXct(sal.w3$time, origin="1970-01-01", tz='GMT'), sal.w3$water_temperature, spar=0.5 ), lwd=2, pch=16, cex=1, xlab="", ylab="", axes=F, col="lightblue")
axis(2, line=4.5)
mtext(2, text="water temperature (degrees C)", line=6.5, col="lightblue")
par(new=T)
plot(flu.w3$time2, flu.w3$Nitrate, lwd=2, pch=16, cex=1.5, xlab="", ylab="", axes=F, col="red")
axis(4)
mtext(4, text="nitrate (uM)", line=2, col="red")
par(new=T)
plot(Par.w3$time, Par.w3$par.max, lwd=2, pch=16, cex=1.5, xlab="", ylab="", axes=F, col="orange")
axis(4, line=3.5)
mtext(4, text="max PAR", line=5.5, col="orange")


# week 4 #
par(mar=c(5,12,4,12))
plot(sal.w4$time, sal.w4$water_salinity, lwd=2, pch=16, xlab="", ylab="salinity (psu?)", cex.lab=1, type="n", xaxt="n")
axis.POSIXct(1, sal.w4$time, format="%D")
points(smooth.spline(as.POSIXct(sal.w4$time, origin="1970-01-01", tz='GMT'), sal.w4$water_salinity, spar=0.5), lwd=2, col="cyan4", pch=16, xlab="", ylab="", axes=F)
par(new=T)
plot(sal.w4$time, sal.w4$water_temperature, lwd=2, pch=16, cex=1, xlab="", ylab="", cex.lab=1,  axes=F, col="lightblue", type="n")
points(smooth.spline(as.POSIXct(sal.w4$time, origin="1970-01-01", tz='GMT'), sal.w4$water_temperature, spar=0.5 ), lwd=2, pch=16, cex=1, xlab="", ylab="", axes=F, col="lightblue")
axis(2, line=4.5)
mtext(2, text="water temperature (degrees C)", line=6.5, col="lightblue")
par(new=T)
plot(flu.w4$time2, flu.w4$Nitrate, lwd=2, pch=16, cex=1.5, xlab="", ylab="", axes=F, col="red")
axis(4)
mtext(4, text="nitrate (uM)", line=2, col="red")
par(new=T)
plot(Par.w4$time, Par.w4$par.max, lwd=2, pch=16, cex=1.5, xlab="", ylab="", axes=F, col="orange")
axis(4, line=3.5)
mtext(4, text="max PAR", line=5.5, col="orange")




########################
#### abundance plot ####
########################

#full TC 
par(mai=c(1,1.5,1,1))
plot(pre.crypto2$time, pre.crypto2$abundance, lwd=2, pch=16, xlab="", ylab="abundance (10^6 cells/L)", cex.lab=2)

#full TC- log scale
par(mai=c(1,1.5,1,1))
plot(pre.crypto2$time, pre.crypto2$abundance, lwd=2, pch=16, xlab="", ylab="abundance (10^6 cells/L)- log scale", cex.lab=2, ylog=T, log="y")

#gap.plot(pre.crypto2$time, pre.crypto2$abundance, gap = c(5,17), lwd=2, pch=16, xlab="time", ylab="abundance (10^6 cells/L)", ylim=c(0,20))

#full TC- daily mean log scale
par(mai=c(1,1.5,1,1))
plot(pre.crypto2$time, pre.crypto2$daily.mean, lwd=2, pch=16, xlab="", ylab="daily mean abundance (10^6 cells/L)- log scale", cex.lab=2, ylog=T, log="y")

#full TC- daily mean
par(mai=c(1,1.5,1,1))
plot(pre.crypto2$time, pre.crypto2$daily.mean, lwd=2, pch=16, xlab="", ylab="daily mean abundance (10^6 cells/L)", cex.lab=2)


################################
#### one day abundance plot ####
################################

#plot(crypto$time, crypto$abundance, lwd=2, pch=16, xlab="", ylab="abundance (10^6 cells/L)", cex.lab=1.5)
#points(smooth.spline(as.POSIXct(crypto$time, origin="1970-01-01", tz='GMT'), crypto$abundance, spar=0.5), lwd=2, pch=16, xlab="", ylab="", type="l", cex=5, axes=F)

###########################
#### weekly abundances ####
###########################

#week 1
par(mai=c(1,1.5,1,1))
plot(crypto.week1$time, crypto.week1$abundance, xaxt="n", xlab="", lwd=2, pch=16, ylab="abundance (10^6 cells/L)", cex.lab=2)
axis.POSIXct(1, crypto.week1$time, format="%D")

#week 1 log scale
par(mai=c(1,1.5,1,1))
plot(crypto.week1$time, crypto.week1$abundance, xaxt="n", xlab="", lwd=2, pch=16, ylab="abundance (10^6 cells/L)- log scale", cex.lab=2, ylog=T, log="y")
axis.POSIXct(1, crypto.week1$time, format="%D")

#week 1 daily mean
par(mai=c(1,1.5,1,1))
plot(crypto.week1$time, crypto.week1$daily.mean, xaxt="n", xlab="", lwd=2, pch=16, ylab="daily mean abundance (10^6 cells/L)", cex.lab=2)
axis.POSIXct(1, crypto.week1$time, format="%D")

#week 1 daily mean log scale 
par(mai=c(1,1.5,1,1))
plot(crypto.week1$time, crypto.week1$daily.mean, xaxt="n", xlab="", lwd=2, pch=16, ylab="daily mean abundance (10^6 cells/L)- log scale", cex.lab=2, ylog=T, log="y")
axis.POSIXct(1, crypto.week1$time, format="%D")


#week 2
par(mai=c(1,1.5,1,1))
plot(crypto.week2$time, crypto.week2$abundance, xaxt="n", xlab="", lwd=2, pch=16, ylab="abundance (10^6 cells/L)", cex.lab=2)
axis.POSIXct(1, crypto.week2$time, format="%D")

#week 2 log scale
par(mai=c(1,1.5,1,1))
plot(crypto.week2$time, crypto.week2$abundance, xaxt="n", xlab="", lwd=2, pch=16, ylab="abundance (10^6 cells/L)- log scale", cex.lab=2, ylog=T, log="y")
axis.POSIXct(1, crypto.week2$time, format="%D")

#week 2 daily mean
par(mai=c(1,1.5,1,1))
plot(crypto.week2$time, crypto.week2$daily.mean, xaxt="n", xlab="", lwd=2, pch=16, ylab="daily mean abundance (10^6 cells/L)", cex.lab=2)
axis.POSIXct(1, crypto.week2$time, format="%D")

#week 2 daily mean log scale
par(mai=c(1,1.5,1,1))
plot(crypto.week2$time, crypto.week2$daily.mean, xaxt="n", xlab="", lwd=2, pch=16, ylab="daily mean abundance (10^6 cells/L)- log scale", cex.lab=2, ylog=T, log="y")
axis.POSIXct(1, crypto.week2$time, format="%D")


#week 3
par(mai=c(1,1.5,1,1))
plot(crypto.week3$time, crypto.week3$abundance, xaxt="n", xlab="", lwd=2, pch=16, ylab="abundance (10^6 cells/L)", cex.lab=2)
axis.POSIXct(1, crypto.week3$time, format="%D")

#week 3 log scale
par(mai=c(1,1.5,1,1))
plot(crypto.week3$time, crypto.week3$abundance, xaxt="n", xlab="", lwd=2, pch=16, ylab="abundance (10^6 cells/L)- log scale", cex.lab=2, ylog=T, log="y")
axis.POSIXct(1, crypto.week3$time, format="%D")

#week 3 daily mean 
par(mai=c(1,1.5,1,1))
plot(crypto.week3$time, crypto.week3$daily.mean, xaxt="n", xlab="", lwd=2, pch=16, ylab="daily mean abundance (10^6 cells/L)", cex.lab=2)
axis.POSIXct(1, crypto.week3$time, format="%D")

#week 3 daily mean log scale
par(mai=c(1,1.5,1,1))
plot(crypto.week3$time, crypto.week3$daily.mean, xaxt="n", xlab="", lwd=2, pch=16, ylab="daily mean abundance (10^6 cells/L)- log scale", cex.lab=2, ylog=T, log="y")
axis.POSIXct(1, crypto.week3$time, format="%D")

#week 4
par(mai=c(1,1.5,1,1))
plot(crypto.week4$time, crypto.week4$abundance, xaxt="n", xlab="", lwd=2, pch=16, ylab="abundance (10^6 cells/L)", cex.lab=2)
axis.POSIXct(1, crypto.week4$time, format="%D")

#week 4 log scale
par(mai=c(1,1.5,1,1))
plot(crypto.week4$time, crypto.week4$abundance, xaxt="n", xlab="", lwd=2, pch=16, ylab="abundance (10^6 cells/L)- log scale", cex.lab=2, ylog=T, log="y")
axis.POSIXct(1, crypto.week4$time, format="%D")

#week 4 daily mean 
par(mai=c(1,1.5,1,1))
plot(crypto.week4$time, crypto.week4$daily.mean, xaxt="n", xlab="", lwd=2, pch=16, ylab="daily mean abundance (10^6 cells/L)", cex.lab=2)
axis.POSIXct(1, crypto.week4$time, format="%D")

#week 4 daily mean log scale
par(mai=c(1,1.5,1,1))
plot(crypto.week4$time, crypto.week4$daily.mean, xaxt="n", xlab="", lwd=2, pch=16, ylab="daily mean abundance (10^6 cells/L)- log scale", cex.lab=2, ylog=T, log="y")
axis.POSIXct(1, crypto.week4$time, format="%D")


#####################################
#### salinity vs. abundance plot ####
#####################################

par(mai=c(1,1.5,1,1))
plot(crypto$time, crypto$abundance, type="n", ylab="abundance (10^6 cells/L)", xlab="", cex.lab=1.5)
points(smooth.spline(as.POSIXct(crypto$time, origin="1970-01-01", tz='GMT'), crypto$abundance, spar=0.5), lwd=2, pch=16, xlab="", ylab="", type="l", cex=5)
par(new=T)
plot(sal$time, sal$water_salinity, xlab="", ylab="", axes=F, type="n")
points(smooth.spline(as.POSIXct(sal$time, origin="1970-01-01", tz='GMT'), sal$water_salinity, spar=0.5), lwd=2, col="cyan4", pch=16, xlab="", ylab="", axes=F)
#type="l", cex=2, lty=2
axis(4)
mtext("salinity", side=4, line=3, cex=1.5)
legend(1380100000, 0.35, c("crypto abundance", "salinity"), lty=c(1,1), lwd=c(2.5,2.5), col=c("darkred", "darkblue"))
#legend not working probably due to time issue 



###########################
#### new sal vs. abund ####
###########################



plot(crypto$time, crypto$abundance, ylab="abundance (10^6 cells/L)", xlab="", cex.lab=1.5, pch=16)
#points(smooth.spline(as.POSIXct(crypto$time, origin="1970-01-01", tz='GMT'), crypto$abundance, spar=0.5), lwd=2, pch=16, xlab="", ylab="", type="l", cex=5, axes=F)
par(new=T)
plot(flu$time, flu$water_salinity, xlab="", ylab="", axes=F, type="n")
points(smooth.spline(as.POSIXct(flu$time, origin="1970-01-01", tz='GMT'), flu$water_salinity, spar=0.5), lwd=2, col="cyan4", pch=16, xlab="", ylab="", axes=F)
#type="l", cex=2, lty=2
axis(4)
mtext("salinity", side=4, line=3, cex=1.5)
legend(1380100000, 0.35, c("crypto abundance", "salinity"), lty=c(1,1), lwd=c(2.5,2.5), col=c("darkred", "darkblue"))
#legend not working probably due to time issue 




#######################
#### div rate plot ####
#######################

#TC
par(mai=c(1,1.5,1,1))
plotCI(as.POSIXct(yay$h.time, origin="1970-01-01", tz='GMT'), yay$daily.GRmean, uiw= yay$daily.GRsd, sfrac=0, pch=16, 	xlab="", ylab="mean daily division rate", cex.lab=1.7)

#week 1
par(mai=c(1,1.5,1,1))
plotCI(as.POSIXct(dr.w1$h.time, origin="1970-01-01", tz='GMT'), dr.w1$daily.GRmean, uiw= dr.w1$daily.GRsd, sfrac=0, pch=16, 	xlab="", ylab="mean daily division rate", cex.lab=1.7)

#week2
par(mai=c(1,1.5,1,1))
plotCI(as.POSIXct(dr.w2$h.time, origin="1970-01-01", tz='GMT'), dr.w2$daily.GRmean, uiw= dr.w2$daily.GRsd, sfrac=0, pch=16, 	xlab="", ylab="mean daily division rate", cex.lab=1.7)

#week3
par(mai=c(1,1.5,1,1))
plotCI(as.POSIXct(dr.w3$h.time, origin="1970-01-01", tz='GMT'), dr.w3$daily.GRmean, uiw= dr.w3$daily.GRsd, sfrac=0, pch=16, 	xlab="", ylab="mean daily division rate", cex.lab=1.7)


#week4
par(mai=c(1,1.5,1,1))
plotCI(as.POSIXct(dr.w4$h.time, origin="1970-01-01", tz='GMT'), dr.w4$daily.GRmean, uiw= dr.w4$daily.GRsd, sfrac=0, pch=16, 	xlab="", ylab="mean daily division rate", cex.lab=1.7)





#####################################
#### div rate vs. abundance plot ####
#####################################

par(mai=c(1,1.5,1,1))
plotCI(as.POSIXct(yay$h.time, origin="1970-01-01", tz='GMT'), yay$daily.GRmean, uiw= yay$daily.GRsd, sfrac=0, pch=16, 	xlab="", ylab="mean daily division rate", cex.lab=1.7)
#ylim=c(0,20)
par(new=T)
plot(pre.crypto2$time, pre.crypto2$abundance, lwd=2, pch=16, xlab="", ylab="", cex.lab=2,  axes=F, cex=.6, col="darkred", ylim=c(0,1))
axis(4)
mtext("abundance (10^6 cells/L)", side=4, lin=3, cex=1.7)



par(mai=c(1,1.5,1,1))
plotCI(as.POSIXct(yay2$h.time, origin="1970-01-01", tz='GMT'), yay2$daily.GRmean, uiw= yay2$daily.GRsd, sfrac=0, pch=16, 	xlab="", ylab="mean daily division rate", cex.lab=1.7)
#ylim=c(0,20)
par(new=T)
plot(crypto$time, crypto$daily.mean, lwd=2, pch=16, xlab="", ylab="", cex.lab=2,  axes=F, cex=.75, col="darkred")
axis(4)
mtext("mean daily abundance (10^6 cells/L)", side=4, lin=3, cex=1.7)



###############################
#### div rate vs. PAR plot ####
###############################


par(mai=c(1,1,1,1))
plotCI(as.POSIXct(yay2$h.time, origin="1970-01-01", tz='GMT'), yay2$daily.GRmean, uiw= yay2$daily.GRsd, sfrac=0, pch=16, 	xlab="", ylab="mean daily division rate", cex.main=2, cex.lab=1.5, xaxt="n")
#axis(2)
#mtext("mean daily division rate", side=2, line=3, cex=1.5)

	par(new=T)
	plot(Par5$time2, Par5$par.max, col="darkblue", pch=16, axes=F, type="o", xlab="", ylab="", cex.lab=1.5)	
	axis(4)
	mtext("PAR", side=4, line=3, cex=1.5)
	
plot(Par2$time, Par2$par, col="darkblue", pch=16, xlab="", cex.lab=1.5, ylab="", type="o", yaxt="n")
axis(4)
mtext("PAR", side=4, line=3, cex=1.5)


################################
#### div rate vs. nutrients ####
################################

par(mai=c(1,1.5,1,1))
plotCI(as.POSIXct(yay2$h.time, origin="1970-01-01", tz='GMT'), yay2$daily.GRmean, uiw= yay2$daily.GRsd, sfrac=0, pch=16, 	xlab="", ylab="mean daily division rate", cex.lab=1.7)
#ylim=c(0,20)
par(new=T)
plot(pre.flu3$time2, pre.flu3$Nitrate, lwd=2, pch=16, cex=1, xlab="", ylab="", cex.lab=2,  axes=F, col="darkred", type="o")
axis(4)
mtext("Nitrate", side=4, lin=3, cex=1.7)



###########################
#### div rate vs. meso ####
###########################

par(mai=c(1,1.5,1,1))
plotCI(as.POSIXct(yay$h.time, origin="1970-01-01", tz='GMT'), yay$daily.GRmean, uiw= yay$daily.GRsd, sfrac=0, pch=16, 	xlab="", ylab="mean daily division rate", cex.lab=1.7)
#ylim=c(0,20)
par(new=T)
plot(meso$time2, meso$particles_mL, lwd=2, pch=16, cex=1, xlab="", ylab="", cex.lab=2,  axes=F, col="darkred", type="o")
axis(4)
mtext("Mesodinium counts", side=4, lin=3, cex=1.7)



############################
#### abundance vs. meso ####
############################

par(mai=c(1,1.5,1,1))
plot(pre.crypto2$time, pre.crypto2$abundance, lwd=2, pch=16, xlab="", ylab="abundance (10^6 cells/L)", cex.lab=2, ylim=c(0,1))
par(new=T)
plot(meso$time2, meso$particles_mL, lwd=2, pch=16, cex=1, xlab="", ylab="", cex.lab=2,  axes=F, col="darkred", type="o")
axis(4)
mtext("Mesodinium counts", side=4, lin=3, cex=1.7)


############################
#### nutrients vs. meso ####
############################

par(mai=c(1,1.5,1,1))
plot(meso$time2, meso$particles_mL, lwd=2, pch=16, cex=1, xlab="", ylab="", cex.lab=2, col="darkred", type="o")
#ylim=c(0,20)
par(new=T)
plot(pre.flu3$time2, pre.flu3$Ammonium, lwd=2, pch=16, cex=1, xlab="", ylab="", cex.lab=2,  axes=F, col="blue", type="o")
axis(4)
mtext("Ammonium", side=4, lin=3, cex=1.7)


###########################
#### N vs. div no time ####
###########################

par(mai=c(1,1,1,1))
plot(cmop$n, cmop$dm, pch=16, xlab="nitrate", ylab="mean daily division rate", cex.lab=1.7, cex=1.5, col="darkred")
res=lm(cmop$dm~cmop$n)
abline(res)




#############
#### map ####
#############

map("worldHires", "Canada", xlim=c(-124.5,-123), ylim=c(45.9,46.5), col="lightcyan", fill=T)
map("worldHires","usa", xlim=c(-124.5,-123), ylim=c(45.9,46.5), col="grey", fill=T, add=T)  
lat<-c(46.21)
lon<-c(-123.91)
points(lon, lat, pch=18, col="red", cex=1.5)
map.scale()

#bathymetry 
blues <- c("lightsteelblue4", "lightsteelblue3", "lightsteelblue2", "lightsteelblue1")
CMOP_bathy <- getNOAA.bathy(lon1= -124.5, lon2= -123, lat1= 45.9, lat2= 46.5, resolution= 1)
plot(CMOP_bathy, image=T, land=T, lwd =0.1, bpal= list(c(0, max(CMOP_bathy), "grey"), c(min(CMOP_bathy), 0, blues)))
plot(CMOP_bathy, deep=0, shallow= 0, step=0, lwd=0.4, add=T)
#scaleBathy(CMOP_bathy, deg=0.1, x="bottomleft", insert=5)




######################
#### plotting fsc ####
######################

#smooth spline
par(mai=c(1,1,1,1))
plot(stat$time, stat$fsc_small, lwd=2, pch=16, xlab="", ylab="forward scatter small", cex.lab=1, type="n", xaxt="n", ylim=c(50, 250))
axis.POSIXct(1, stat$time, format="%D")
points(smooth.spline(as.POSIXct(stat$time, origin="1970-01-01", tz='GMT'), stat$fsc_small, spar=0.5), lwd=2, col="black", pch=16, xlab="", ylab="", axes=F)


par(mai=c(1,1,1,1))
plot(stat$time, stat$fsc_small, lwd=2, pch=16, xlab="", ylab="forward scatter small")


##############################
#### plotting tx and qPCR ####
##############################

#tx percent
plot(tx$time2, tx$percent2, lwd=2, pch=16, cex=2, col= "red", xlab="", ylab="percent Teleaulax of total cryptophytes")

#tx percent vs meso
par(mai=c(1,1.5,1,1))
plot(tx$time2, tx$percent2, lwd=2, pch=16, cex=2, xlab="", ylab="percent Teleaulax of total cryptophytes", cex.lab=2, ylim=c(0,0.5))
par(new=T)
plot(meso$time2, meso$particles_mL, lwd=2, pch=16, cex=1, xlab="", ylab="", cex.lab=2,  axes=F, col="red", type="o")
axis(4)
mtext("Mesodinium counts", side=4, lin=3, cex=1.7)

#tx number vs meso
par(mai=c(1,1.5,1,1))
plot(tx$time2, tx$tx_cop, lwd=2, pch=16, cex=2, xlab="", ylab="Teleaulax qPCR copy number", cex.lab=2)
par(new=T)
plot(meso$time2, meso$particles_mL, lwd=2, pch=16, cex=1, xlab="", ylab="", cex.lab=2,  axes=F, col="red", type="o")
axis(4)
mtext("Mesodinium counts", side=4, lin=3, cex=1.7)


#qPCR
par(mai=c(1,1.5,1,1))
plot(pre.crypto2$time, pre.crypto2$daily.mean, lwd=2, pch=16, xlab="", ylab="daily mean abundance (10^6 cells/L)- log scale", cex.lab=2, ylog=T, log="y")
par(new=T)
plot(tx$time2, tx$cryp_cop, lwd=2, pch=16, cex=2, xlab="", ylab="", axes=F, col="red", cex.lab=2)
axis(4)
mtext("qPCR crypto copy number", side=4, lin=3, cex=1.7)



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




par(mai=c(1,1.5,1,1))

plot(as.POSIXct(cc$time, origin="1970-01-01"), cc$div, ylim=c(0,0.05), sfrac=0, lwd=2, pch=16, cex=1.5,ylab=NA, xlab=NA)

plotCI(m$time, m$div.ave, m$div.sd, col=2,add=T, sfrac=0, lwd=2, pch=16,cex=1.5)
mtext(substitute(paste("Division (h"^{-1},")")), side=2, line=3, cex=1.5)
mtext("time", side=1, line=3, cex=1.5)

