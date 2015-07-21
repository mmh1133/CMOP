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
library(lmodel2)


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
crypto.week2 <- subset(pre.crypto2, time > as.POSIXct("2013-09-13 24:00:00") & time < as.POSIXct("2013-09-20 24:00:00")) 
crypto.week3 <- subset(pre.crypto2, time > as.POSIXct("2013-09-21 00:00:00") & time < as.POSIXct("2013-09-28 24:00:00")) 
crypto.week4 <- subset(pre.crypto2, time > as.POSIXct("2013-09-29 00:00:00") & time < as.POSIXct("2013-10-03 24:00:00")) 

# roll mean abundance #
pre.crypto2$daily.mean <- rollapply(data=pre.crypto2$abundance, width=24, FUN=mean, na.rm=T, fill=NA)*24
crypto$daily.mean <- rollapply(data=crypto$abundance, width=24, FUN=mean, na.rm=T, fill=NA)*24
crypto.week1$daily.mean <- rollapply(data=crypto.week1$abundance, width=24, FUN=mean, na.rm=T, fill=NA)*24
crypto.week2$daily.mean <- rollapply(data=crypto.week2$abundance, width=24, FUN=mean, na.rm=T, fill=NA)*24
crypto.week3$daily.mean <- rollapply(data=crypto.week3$abundance, width=24, FUN=mean, na.rm=T, fill=NA)*24
crypto.week4$daily.mean <- rollapply(data=crypto.week4$abundance, width=24, FUN=mean, na.rm=T, fill=NA)*24

# roll mean standard deviation #
#pre.crypto2$sd <- rollapply(data=pre.crypto2$abundance, width=24, FUN=sd, na.rm=T, fill=NA)*24
#crypto.week1$sd <- rollapply(data=crypto.week1$abundance, width=24, FUN=sd, na.rm=T, fill=NA)*24

# eliminating nas for smooth.spline #
crypto.week1.ss <- crypto.week1
crypto.week1.ss2<- crypto.week1.ss[complete.cases(crypto.week1.ss[,11]),]
crypto.week2.ss <- crypto.week2
crypto.week2.ss2<- crypto.week2.ss[complete.cases(crypto.week2.ss[,11]),]
crypto.week3.ss <- crypto.week3
crypto.week3.ss2<- crypto.week3.ss[complete.cases(crypto.week3.ss[,11]),]
crypto.week4.ss <- crypto.week4
crypto.week4.ss2<- crypto.week4.ss[complete.cases(crypto.week4.ss[,11]),]


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
yay$daily.GRse <- rollapply(data=yay$h.dr.sd, width=24, FUN=mean, na.rm=T, fill=NA)*24/sqrt(24)

yay$time <- as.POSIXct(yay$h.time,tz='GMT', origin="1970-01-01")
yay2 <- subset(yay, time > as.POSIXct("2013-09-10 16:50:00") & time < as.POSIXct("2013-09-20 00:00:00")) 
dr.w1 <- subset(yay, time > as.POSIXct("2013-09-10 16:50:00",tz='GMT', origin="1970-01-01") & time < as.POSIXct("2013-09-13 24:00:00",tz='GMT', origin="1970-01-01"))
dr.w2 <- subset(yay, time > as.POSIXct("2013-09-15 00:00:00",tz='GMT', origin="1970-01-01") & time < as.POSIXct("2013-09-20 24:00:00",tz='GMT', origin="1970-01-01"))
dr.w3 <- subset(yay, time > as.POSIXct("2013-09-22 00:00:00",tz='GMT', origin="1970-01-01") & time < as.POSIXct("2013-09-28 24:00:00",tz='GMT', origin="1970-01-01"))
dr.w4 <- subset(yay, time > as.POSIXct("2013-09-29 00:00:00",tz='GMT', origin="1970-01-01") & time < as.POSIXct("2013-10-03 24:00:00",tz='GMT', origin="1970-01-01"))


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

Par.w1.all <- subset(Par2, time > as.POSIXct("2013-09-10 16:50:00") & time < as.POSIXct("2013-09-14 24:00:00"))
Par.w2.all <- subset(Par2, time > as.POSIXct("2013-09-15 00:00:00") & time < as.POSIXct("2013-09-21 24:00:00"))
Par.w3.all <- subset(Par2, time > as.POSIXct("2013-09-22 00:00:00") & time < as.POSIXct("2013-09-28 24:00:00"))
Par.w4.all <- subset(Par2, time > as.POSIXct("2013-09-29 00:00:00") & time < as.POSIXct("2013-10-03 23:58:00"))

Par2$daily.mean <- rollapply(data=Par2$par, width=24, FUN=mean, na.rm=T, fill=NA)*24

# setting up individual days of par data
day1.p <- subset(Par2, time > as.POSIXct("2013-09-11 00:00:00") & time < as.POSIXct("2013-09-12 00:00:00"))
day2.p <- subset(Par2, time > as.POSIXct("2013-09-12 00:00:00") & time < as.POSIXct("2013-09-13 00:00:00"))
day3.p <- subset(Par2, time > as.POSIXct("2013-09-13 00:00:00") & time < as.POSIXct("2013-09-14 00:00:00"))
day4.p <- subset(Par2, time > as.POSIXct("2013-09-14 00:00:00") & time < as.POSIXct("2013-09-15 00:00:00"))
day5.p <- subset(Par2, time > as.POSIXct("2013-09-15 00:00:00") & time < as.POSIXct("2013-09-16 00:00:00"))
day6.p <- subset(Par2, time > as.POSIXct("2013-09-16 00:00:00") & time < as.POSIXct("2013-09-17 00:00:00"))
day7.p <- subset(Par2, time > as.POSIXct("2013-09-17 00:00:00") & time < as.POSIXct("2013-09-18 00:00:00"))
day8.p <- subset(Par2, time > as.POSIXct("2013-09-18 00:00:00") & time < as.POSIXct("2013-09-19 00:00:00"))
day9.p <- subset(Par2, time > as.POSIXct("2013-09-19 00:00:00") & time < as.POSIXct("2013-09-20 00:00:00"))
day10.p <- subset(Par2, time > as.POSIXct("2013-09-20 00:00:00") & time < as.POSIXct("2013-09-21 00:00:00"))
day11.p <- subset(Par2, time > as.POSIXct("2013-09-21 00:00:00") & time < as.POSIXct("2013-09-22 00:00:00"))
day12.p <- subset(Par2, time > as.POSIXct("2013-09-22 00:00:00") & time < as.POSIXct("2013-09-23 00:00:00"))
day13.p <- subset(Par2, time > as.POSIXct("2013-09-23 00:00:00") & time < as.POSIXct("2013-09-24 00:00:00"))
day14.p <- subset(Par2, time > as.POSIXct("2013-09-24 00:00:00") & time < as.POSIXct("2013-09-25 00:00:00"))
day15.p <- subset(Par2, time > as.POSIXct("2013-09-25 00:00:00") & time < as.POSIXct("2013-09-26 00:00:00"))
day16.p <- subset(Par2, time > as.POSIXct("2013-09-26 00:00:00") & time < as.POSIXct("2013-09-27 00:00:00"))
day17.p <- subset(Par2, time > as.POSIXct("2013-09-27 00:00:00") & time < as.POSIXct("2013-09-28 00:00:00"))
day18.p <- subset(Par2, time > as.POSIXct("2013-09-28 00:00:00") & time < as.POSIXct("2013-09-29 00:00:00"))
day19.p <- subset(Par2, time > as.POSIXct("2013-09-29 00:00:00") & time < as.POSIXct("2013-09-30 00:00:00"))
day20.p <- subset(Par2, time > as.POSIXct("2013-09-30 00:00:00") & time < as.POSIXct("2013-10-01 00:00:00"))
day21.p <- subset(Par2, time > as.POSIXct("2013-10-01 00:00:00") & time < as.POSIXct("2013-10-02 00:00:00"))
day22.p <- subset(Par2, time > as.POSIXct("2013-10-02 00:00:00") & time < as.POSIXct("2013-10-03 00:00:00"))
day23.p <- subset(Par2, time > as.POSIXct("2013-10-03 00:00:00") & time < as.POSIXct("2013-10-04 00:00:00"))

d1m.p <- mean(day1.p$par, na.rm=T)
d2m.p <- mean(day2.p$par, na.rm=T)
d3m.p <- mean(day3.p$par, na.rm=T)
d4m.p <- mean(day4.p$par, na.rm=T)
d5m.p <- mean(day5.p$par, na.rm=T)  #NA
d6m.p <- mean(day6.p$par, na.rm=T)  #NA
d7m.p <- mean(day7.p$par, na.rm=T)
d8m.p <- mean(day8.p$par, na.rm=T)
d9m.p <- mean(day9.p$par, na.rm=T)
d10m.p <- mean(day10.p$par, na.rm=T) #NA
d11m.p <- mean(day11.p$par, na.rm=T) #NA
d12m.p <- mean(day12.p$par, na.rm=T) #NA
d13m.p <- mean(day13.p$par, na.rm=T)
d14m.p <- mean(day14.p$par, na.rm=T)  
d15m.p <- mean(day15.p$par, na.rm=T)  
d16m.p <- mean(day16.p$par, na.rm=T)
d17m.p <- mean(day17.p$par, na.rm=T)
d18m.p <- mean(day18.p$par, na.rm=T) #NA
d19m.p <- mean(day19.p$par, na.rm=T) #NA
d20m.p <- mean(day20.p$par, na.rm=T) #NA
d21m.p <- mean(day21.p$par, na.rm=T)
d22m.p <- mean(day22.p$par, na.rm=T)
d23m.p <- mean(day23.p$par, na.rm=T) #NA

par.cum <- c(d1m.p, d2m.p, d3m.p, d7m.p, d8m.p, d9m.p, d13m.p, d14m.p, d15m.p, d16m.p, d17m.p, d21m.p, d22m.p)

#### setting up nutrient data ####

pre.flu <- read.csv("/Users/francois/CMOP/auxillary_data/Ribalet_nutrients2.csv")
pre.flu2 <- as.data.frame(pre.flu, row.names=NULL)
pre.flu2$time2 <- as.POSIXct(strptime(pre.flu2$time, "%Y/%m/%d %H:%M:%S"), tz="GMT")
pre.flu2 <- subset(pre.flu2, time2 > as.POSIXct("2013-09-10 16:50:00") & time2 < as.POSIXct("2013-10-03 00:00:00")) 
pre.flu3 <- subset(pre.flu2, time2 > as.POSIXct("2013-09-10 16:50:00") & time2 < as.POSIXct("2013-09-20 00:00:00")) 
flu.w1 <- subset(pre.flu2, time2 > as.POSIXct("2013-09-10 16:50:00") & time2 < as.POSIXct("2013-09-14 24:00:00")) 
flu.w2 <- subset(pre.flu2, time2 > as.POSIXct("2013-09-15 00:00:00") & time2 < as.POSIXct("2013-09-21 24:00:00")) 
flu.w3 <- subset(pre.flu2, time2 > as.POSIXct("2013-09-22 00:00:00") & time2 < as.POSIXct("2013-09-28 24:00:00")) 
flu.w4 <- subset(pre.flu2, time2 > as.POSIXct("2013-09-29 00:00:00") & time2 < as.POSIXct("2013-10-03 24:00:00")) 



#picking out nitrate for div vs n and making data frame
n <- c(12.7, 13.3, 13.2, 7.1, 8.6, 5.9, 3.5, 5.0, 8.7, 10.3, 9.7, 11.6, 13.1) #nitrate
ph <- c(0.9, 1.1, 0.9, 0.9, 1.1, 0.6, 0.5, 0.6, 0.7, 0.7, 1.0, 0.4, 0.6) #phosphate
a <- c(5.0, 6.5, 5.1, 4.6, 5.3, 13.5, 2.3, 2.5, 3.6, 2.5, 8.1, 5.2, 5.6) #ammonium
cmop <- data.frame(dm, n, ph, a, par.cum)


#### adding mesodinium counts ####

pre.meso <- read.csv("/Users/francois/CMOP/mesodinium/meso.csv")
meso <- as.data.frame(pre.meso, row.names=NULL)
meso$time2 <- as.POSIXct(strptime(meso$datetime, "%m/%d/%Y %H:%M:%S"), tz="GMT")
meso2 <- subset(meso, time2 > as.POSIXct("2013-09-10 16:50:00") & time2 < as.POSIXct("2013-09-20 00:00:00"))
meso3 <- subset(meso, time2 > as.POSIXct("2013-09-10 00:00:00") & time2 < as.POSIXct("2013-09-20 00:00:00"))


#### TX qPCR data ####

pre.tx <- read.csv("/Users/francois/CMOP/pics_misc/tx_qPCR.csv")
tx <- as.data.frame(pre.tx, row.names=NULL)
tx$time2 <- as.POSIXct(strptime(tx$time, "%m/%d/%Y %H:%M:%S"), tz="GMT")





##########################################################################################################################




########################
#### aux data plots ####
########################

par(mfrow=c(3,1), mar=c(2,9,2.5,8)+0.8, pty="m")

# TS #
#par(mar=c(5,12,4,12))
plot(sal$time, sal$water_salinity, lwd=2, pch=16, xlab="", ylab="", cex=0.2, las=1, cex.axis=2)
points(smooth.spline(as.POSIXct(sal$time, origin="1970-01-01", tz='GMT'), sal$water_salinity, spar=0.5), lwd=2, col="black", pch=16, xlab="", ylab="", axes=F)
mtext(2, text="salinity (psu)", line=3.7, cex=1.2)
par(new=T)
plot(sal$time, sal$water_temperature, lwd=2, pch=16, xlab="", ylab="", cex.lab=1,  axes=F, col="darkgrey", cex=0.2, las=1, cex.axis=2)
points(smooth.spline(as.POSIXct(sal$time, origin="1970-01-01", tz='GMT'), sal$water_temperature, spar=0.5 ), lwd=2, pch=16, cex=1, xlab="", ylab="", axes=F, col="darkgrey")
axis(4, cex.axis=2)
mtext(4, text="water temperature (degrees C)", line=3, cex=1.2)
mtext("A", side=3, cex=2, adj=0)
legend("topright", c("salinity", "temperature"), lty=c(1,1), lwd=c(3,3), col=c("black", "darkgrey"), cex=1.5)

# PAR #
#par(mar=c(5,12,4,12))
smooth <- smooth.spline(as.POSIXct(Par2$time, origin="1970-01-01", tz='GMT'), Par2$par, spar=0.2 )
id <- which(smooth$y < 0)
smooth$y[id] <- 0
smooth$new.time <- as.POSIXct(smooth$x, origin="1970-01-01", tz='GMT')
smoothie <-data.frame(smooth$x, smooth$y, smooth$new.time)
par.smooth1 <- subset(smoothie, smooth.new.time > as.POSIXct("2013-09-11 00:00:00") & smooth.new.time < as.POSIXct("2013-09-28 13:00:00"))
par.smooth2 <- subset(smoothie, smooth.new.time > as.POSIXct("2013-09-29 12:30:00") & smooth.new.time < as.POSIXct("2013-10-04 07:00:00"))
plot(as.POSIXct(par.smooth1$smooth.x, origin="1970-01-01", tz='GMT'), par.smooth1$smooth.y, lwd=2, pch=16, cex=1, xlab="", ylab="", col="black",  cex.axis=2, las=1, type="l", xlim=c(as.POSIXct("2013-09-11 00:00:00"), as.POSIXct("2013-10-04 07:00:00")))
par(new=T)
plot(as.POSIXct(par.smooth2$smooth.x, origin="1970-01-01", tz='GMT'), par.smooth2$smooth.y, lwd=2, pch=16, cex=1, xlab="", ylab="", col="black",  cex.axis=2, las=1, type="l", axes=F, xlim=c(as.POSIXct("2013-09-11 00:00:00"), as.POSIXct("2013-10-04 07:00:00")))
mtext(2, text="PAR", line=4.3, cex=1.2)
mtext("B", side=3, cex=2, adj=0)

# nutrients #
#par(mar=c(5,12,4,12))
plot(pre.flu2$time2, pre.flu2$Nitrate, lwd=2, pch=16, cex=1.5, xlab="", ylab="", cex.lab=1.7, ylim=c(0,22), las=1, cex.axis=2, cex.lab=1.2)
mtext(substitute(paste("nitrate (", mu,"M)")), side=2, cex=1.2, line=3)
par(new=T)
plot(pre.flu2$time2, pre.flu2$Ammonium, lwd=2, pch=0, cex=1, xlab="", ylab="", axes=F, ylim=c(0,100), las=1, cex.axis=2)
axis(2, line=5.2, cex.axis=2)
mtext(substitute(paste("ammonium (", mu, "M)")), side=2, line=7.8, cex=1.2)
par(new=T)
plot(pre.flu2$time2, pre.flu2$Phosphate, lwd=2, pch=4, cex=1, xlab="", ylab="", axes=F, ylim=c(0, 2), las=1, cex.axis=2)
axis(4, cex.axis=2)
mtext(substitute(paste("phosphate (", mu, "M)")),side=4, line=3, cex=1.2)
legend("topright", c("nitrate", "ammonium", "phosphate"), pch=c(16, 0, 4), cex=1.5)
mtext("C", side=3, cex=2, adj=0)



# full TC #
# par(mar=c(5,12,4,12))
# plot(sal$time, sal$water_salinity, lwd=2, pch=16, xlab="", ylab="salinity (psu?)", cex.lab=1, type="n")
# points(smooth.spline(as.POSIXct(sal$time, origin="1970-01-01", tz='GMT'), sal$water_salinity, spar=0.5), lwd=2, col="cyan4", pch=16, xlab="", ylab="", axes=F)
# par(new=T)
# plot(sal$time, sal$water_temperature, lwd=2, pch=16, cex=1, xlab="", ylab="", cex.lab=1,  axes=F, col="lightblue", type="n")
# points(smooth.spline(as.POSIXct(sal$time, origin="1970-01-01", tz='GMT'), sal$water_temperature, spar=0.5 ), lwd=2, pch=16, cex=1, xlab="", ylab="", axes=F, col="lightblue")
# axis(2, line=4.5)
# mtext(2, text="water temperature (degrees C)", line=6.5, col="lightblue")
# par(new=T)
# plot(pre.flu2$time2, pre.flu$Nitrate, lwd=2, pch=16, cex=1.5, xlab="", ylab="", axes=F, col="red")
# axis(4)
# mtext(4, text="nitrate (uM)", line=2, col="red")
# par(new=T)
# plot(Par2$time, Par2$par, lwd=2, pch=16, cex=1, xlab="", ylab="", axes=F, col="orange", type="l")
# axis(4, line=3.5)
# mtext(4, text="PAR", line=5.5, col="orange")

#smooth spline
# par(mar=c(5,12,4,12))
# plot(sal$time, sal$water_salinity, lwd=2, pch=16, xlab="", ylab="salinity (psu?)", cex.lab=1, type="n")
# points(smooth.spline(as.POSIXct(sal$time, origin="1970-01-01", tz='GMT'), sal$water_salinity, spar=0.5), lwd=2, col="cyan4", pch=16, xlab="", ylab="", axes=F)
# par(new=T)
# plot(sal$time, sal$water_temperature, lwd=2, pch=16, cex=1, xlab="", ylab="", cex.lab=1,  axes=F, col="lightblue", type="n")
# points(smooth.spline(as.POSIXct(sal$time, origin="1970-01-01", tz='GMT'), sal$water_temperature, spar=0.5 ), lwd=2, pch=16, cex=1, xlab="", ylab="", axes=F, col="lightblue")
# axis(2, line=4.5)
# mtext(2, text="water temperature (degrees C)", line=6.5, col="lightblue")
# par(new=T)
# plot(pre.flu2$time2, pre.flu$Nitrate, lwd=2, pch=16, cex=1.5, xlab="", ylab="", axes=F, col="red")
# axis(4)
# mtext(4, text="nitrate (uM)", line=2, col="red")
# par(new=T)
# #plot(Par2$time, Par2$par, lwd=2, pch=16, cex=1, xlab="", ylab="", axes=F, col="orange", type="n")
# smooth <- smooth.spline(as.POSIXct(Par2$time, origin="1970-01-01", tz='GMT'), Par2$par, spar=0.2 )
# id <- which(smooth$y < 0)
# smooth$y[id] <- 0
# plot(smooth$x, smooth$y, lwd=2, pch=16, cex=1, xlab="", ylab="", axes=F, col="orange")
# axis(4, line=3.5)
# mtext(4, text="PAR", line=5.5, col="orange")


#max par
# par(mar=c(5,12,4,12))
# plot(sal$time, sal$water_salinity, lwd=2, pch=16, xlab="", ylab="salinity (psu?)", cex.lab=1, type="n")
# points(smooth.spline(as.POSIXct(sal$time, origin="1970-01-01", tz='GMT'), sal$water_salinity, spar=0.5), lwd=2, col="cyan4", pch=16, xlab="", ylab="", axes=F)
# par(new=T)
# plot(sal$time, sal$water_temperature, lwd=2, pch=16, cex=1, xlab="", ylab="", cex.lab=1,  axes=F, col="lightblue", type="n")
# points(smooth.spline(as.POSIXct(sal$time, origin="1970-01-01", tz='GMT'), sal$water_temperature, spar=0.5 ), lwd=2, pch=16, cex=1, xlab="", ylab="", axes=F, col="lightblue")
# axis(2, line=4.5)
# mtext(2, text="water temperature (degrees C)", line=6.5, col="lightblue")
# par(new=T)
# plot(pre.flu2$time2, pre.flu$Nitrate, lwd=2, pch=16, cex=1.5, xlab="", ylab="", axes=F, col="red")
# axis(4)
# mtext(4, text="nitrate (uM)", line=2, col="red")
# par(new=T)
# plot(Par.all$time, Par.all$par.max, lwd=2, pch=16, cex=1.5, xlab="", ylab="", axes=F, col="orange")
# axis(4, line=3.5)
# mtext(4, text="PAR", line=5.5, col="orange")


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
# par(mar=c(5,12,4,12))
# plot(sal.w1$time, sal.w1$water_salinity, lwd=2, pch=16, xlab="", ylab="salinity (psu?)", cex.lab=1, type="n", xaxt="n")
# axis.POSIXct(1, sal.w1$time, format="%D")
# points(smooth.spline(as.POSIXct(sal.w1$time, origin="1970-01-01", tz='GMT'), sal.w1$water_salinity, spar=0.5), lwd=2, col="cyan4", pch=16, xlab="", ylab="", axes=F)
# par(new=T)
# plot(sal.w1$time, sal.w1$water_temperature, lwd=2, pch=16, cex=1, xlab="", ylab="", cex.lab=1,  axes=F, col="lightblue", type="n")
# points(smooth.spline(as.POSIXct(sal.w1$time, origin="1970-01-01", tz='GMT'), sal.w1$water_temperature, spar=0.5 ), lwd=2, pch=16, cex=1, xlab="", ylab="", axes=F, col="lightblue")
# axis(2, line=4.5)
# mtext(2, text="water temperature (degrees C)", line=6.5, col="lightblue")
# par(new=T)
# plot(flu.w1$time2, flu.w1$Nitrate, lwd=2, pch=16, cex=1.5, xlab="", ylab="", axes=F, col="red")
# axis(4)
# mtext(4, text="nitrate (uM)", line=2, col="red")
# par(new=T)
# plot(Par.w1$time, Par.w1$par.max, lwd=2, pch=16, cex=1.5, xlab="", ylab="", axes=F, col="orange")
# axis(4, line=3.5)
# mtext(4, text="max PAR", line=5.5, col="orange")


# week 2 #
# par(mar=c(5,12,4,12))
# plot(sal.w2$time, sal.w2$water_salinity, lwd=2, pch=16, xlab="", ylab="salinity (psu?)", cex.lab=1, type="n", xaxt="n")
# axis.POSIXct(1, sal.w2$time, format="%D")
# points(smooth.spline(as.POSIXct(sal.w2$time, origin="1970-01-01", tz='GMT'), sal.w2$water_salinity, spar=0.5), lwd=2, col="cyan4", pch=16, xlab="", ylab="", axes=F)
# par(new=T)
# plot(sal.w2$time, sal.w2$water_temperature, lwd=2, pch=16, cex=1, xlab="", ylab="", cex.lab=1,  axes=F, col="lightblue", type="n")
# points(smooth.spline(as.POSIXct(sal.w2$time, origin="1970-01-01", tz='GMT'), sal.w2$water_temperature, spar=0.5 ), lwd=2, pch=16, cex=1, xlab="", ylab="", axes=F, col="lightblue")
# axis(2, line=4.5)
# mtext(2, text="water temperature (degrees C)", line=6.5, col="lightblue")
# par(new=T)
# plot(flu.w2$time2, flu.w2$Nitrate, lwd=2, pch=16, cex=1.5, xlab="", ylab="", axes=F, col="red")
# axis(4)
# mtext(4, text="nitrate (uM)", line=2, col="red")
# par(new=T)
# plot(Par.w2$time, Par.w2$par.max, lwd=2, pch=16, cex=1.5, xlab="", ylab="", axes=F, col="orange")
# axis(4, line=3.5)
# mtext(4, text="max PAR", line=5.5, col="orange")


# week 3 #
# par(mar=c(5,12,4,12))
# plot(sal.w3$time, sal.w3$water_salinity, lwd=2, pch=16, xlab="", ylab="salinity (psu?)", cex.lab=1, type="n", xaxt="n")
# axis.POSIXct(1, sal.w3$time, format="%D")
# points(smooth.spline(as.POSIXct(sal.w3$time, origin="1970-01-01", tz='GMT'), sal.w3$water_salinity, spar=0.5), lwd=2, col="cyan4", pch=16, xlab="", ylab="", axes=F)
# par(new=T)
# plot(sal.w3$time, sal.w3$water_temperature, lwd=2, pch=16, cex=1, xlab="", ylab="", cex.lab=1,  axes=F, col="lightblue", type="n")
# points(smooth.spline(as.POSIXct(sal.w3$time, origin="1970-01-01", tz='GMT'), sal.w3$water_temperature, spar=0.5 ), lwd=2, pch=16, cex=1, xlab="", ylab="", axes=F, col="lightblue")
# axis(2, line=4.5)
# mtext(2, text="water temperature (degrees C)", line=6.5, col="lightblue")
# par(new=T)
# plot(flu.w3$time2, flu.w3$Nitrate, lwd=2, pch=16, cex=1.5, xlab="", ylab="", axes=F, col="red")
# axis(4)
# mtext(4, text="nitrate (uM)", line=2, col="red")
# par(new=T)
# plot(Par.w3$time, Par.w3$par.max, lwd=2, pch=16, cex=1.5, xlab="", ylab="", axes=F, col="orange")
# axis(4, line=3.5)
# mtext(4, text="max PAR", line=5.5, col="orange")


# week 4 #
# par(mar=c(5,12,4,12))
# plot(sal.w4$time, sal.w4$water_salinity, lwd=2, pch=16, xlab="", ylab="salinity (psu?)", cex.lab=1, type="n", xaxt="n")
# axis.POSIXct(1, sal.w4$time, format="%D")
# points(smooth.spline(as.POSIXct(sal.w4$time, origin="1970-01-01", tz='GMT'), sal.w4$water_salinity, spar=0.5), lwd=2, col="cyan4", pch=16, xlab="", ylab="", axes=F)
# par(new=T)
# plot(sal.w4$time, sal.w4$water_temperature, lwd=2, pch=16, cex=1, xlab="", ylab="", cex.lab=1,  axes=F, col="lightblue", type="n")
# points(smooth.spline(as.POSIXct(sal.w4$time, origin="1970-01-01", tz='GMT'), sal.w4$water_temperature, spar=0.5 ), lwd=2, pch=16, cex=1, xlab="", ylab="", axes=F, col="lightblue")
# axis(2, line=4.5)
# mtext(2, text="water temperature (degrees C)", line=6.5, col="lightblue")
# par(new=T)
# plot(flu.w4$time2, flu.w4$Nitrate, lwd=2, pch=16, cex=1.5, xlab="", ylab="", axes=F, col="red")
# axis(4)
# mtext(4, text="nitrate (uM)", line=2, col="red")
# par(new=T)
# plot(Par.w4$time, Par.w4$par.max, lwd=2, pch=16, cex=1.5, xlab="", ylab="", axes=F, col="orange")
# axis(4, line=3.5)
# mtext(4, text="max PAR", line=5.5, col="orange")




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

#full TC- daily mean + sd
par(mai=c(1,1.5,1,1))
plotCI(pre.crypto2$time, pre.crypto2$daily.mean, uiw= pre.crypto2$sd, sfrac=0, pch=16, 	xlab="", ylab="mean daily abundance (10^6 cells/L)", cex.lab=1.7)


################################
#### one day abundance plot ####
################################

#plot(crypto$time, crypto$abundance, lwd=2, pch=16, xlab="", ylab="abundance (10^6 cells/L)", cex.lab=1.5)
#points(smooth.spline(as.POSIXct(crypto$time, origin="1970-01-01", tz='GMT'), crypto$abundance, spar=0.5), lwd=2, pch=16, xlab="", ylab="", type="l", cex=5, axes=F)

###########################
#### weekly abundances ####
###########################

#week 1
# par(mai=c(1,1.5,1,1))
# plot(crypto.week1$time, crypto.week1$abundance, xaxt="n", xlab="", lwd=2, pch=16, ylab="abundance (10^6 cells/L)", cex.lab=2, ylim=c(-25,30))
# axis.POSIXct(1, crypto.week1$time, at=seq(min(crypto.week1$time, na.rm=T), max(crypto.week1$time, na.rm=T), by=60*60*12), format="%c", cex=.5)

# lines(crypto.week1.na$time, crypto.week1.na$abundance+crypto.week1$sd, lwd=1, col='grey')
# lines(crypto.week1.na$time, crypto.week1.na$abundance-crypto.week1$sd, lwd=1, col='grey')

#index <- which(diff(crypto.week1.na$time) > 182)
#crypto.week1.na[index,'abundance'] <- NA

#quartz("Quartz", width=15, height=12)
par(mfrow=c(4,1), mar=c(2,8,2.5,2)+0.8, pty="m")
#par(mfrow=c(4,1), pty="m")

#substitute(paste("abundance 10"^{6}, " cells L"^{-1}))

#week 1 log scale
#par(mai=c(1,1.5,1,1))
plot(crypto.week1$time, crypto.week1$abundance, xaxt="n", xlab="", lwd=2, pch=16, ylab= "", cex.lab=1.5, log="y", col="darkgrey", las=1, cex.axis=1.2)
points(smooth.spline(crypto.week1.ss2$time, crypto.week1.ss2$abundance, spar=0.5), lwd=2, col="black", pch=16, xlab="", ylab="", axes=F, cex=0.75)
axis.POSIXct(1, crypto.week1$time, at=seq(min(crypto.week1$time, na.rm=T), max(crypto.week1$time, na.rm=T), by=60*60*6), format="%m-%d %H:%M", cex.axis=1.5)
mtext("A", side=3, cex=2, line=0, adj=0)
mtext(substitute(paste("abundance 10"^{6}, " cells L"^{-1})), side=2, cex=2.5, outer=T, line=-4)

#lines(crypto.week1.na$time, crypto.week1.na$abundance+crypto.week1$sd, lwd=1, col='grey')
#lines(crypto.week1.na$time, crypto.week1.na$abundance-crypto.week1$sd, lwd=1, col='grey')


rect(as.POSIXct("2013-09-10 23:51:00", origin="1970-01-01", tz='GMT'), 0.000000001, as.POSIXct("2013-09-11 06:13:00", origin="1970-01-01", tz='GMT'), 49.0, density=NULL, col=adjustcolor("black", alpha=0.07), border=NA)
rect(as.POSIXct("2013-09-11 11:52:00", origin="1970-01-01", tz='GMT'), 0.000000001, as.POSIXct("2013-09-11 17:41:00", origin="1970-01-01", tz='GMT'), 49.0, density=NULL, col=adjustcolor("black", alpha=0.07), border=NA)
rect(as.POSIXct("2013-09-12 00:55:00", origin="1970-01-01", tz='GMT'), 0.000000001, as.POSIXct("2013-09-12 07:24:00", origin="1970-01-01", tz='GMT'), 49.0, density=NULL, col=adjustcolor("black", alpha=0.07), border=NA)
rect(as.POSIXct("2013-09-12 12:44:00", origin="1970-01-01", tz='GMT'), 0.000000001, as.POSIXct("2013-09-12 18:43:00", origin="1970-01-01", tz='GMT'), 49.0, density=NULL, col=adjustcolor("black", alpha=0.07), border=NA)
rect(as.POSIXct("2013-09-13 02:11:00", origin="1970-01-01", tz='GMT'), 0.000000001, as.POSIXct("2013-09-13 08:41:00", origin="1970-01-01", tz='GMT'), 49.0, density=NULL, col=adjustcolor("black", alpha=0.07), border=NA)
rect(as.POSIXct("2013-09-13 14:05:00", origin="1970-01-01", tz='GMT'), 0.000000001, as.POSIXct("2013-09-13 19:58:00", origin="1970-01-01", tz='GMT'), 49.0, density=NULL, col=adjustcolor("black", alpha=0.07), border=NA)




#week 1 daily mean
# par(mai=c(1,1.5,1,1))
# plot(crypto.week1$time, crypto.week1$daily.mean, xaxt="n", xlab="", lwd=2, pch=16, ylab="daily mean abundance (10^6 cells/L)", cex.lab=2, ylim=c(0, 150))
# axis.POSIXct(1, crypto.week1$time, at=seq(min(crypto.week1$time, na.rm=T), max(crypto.week1$time, na.rm=T), by=60*60*12), format="%H")
# lines(crypto.week1.na$time, crypto.week1.na$daily.mean+crypto.week1$sd, lwd=1, col='grey')
# lines(crypto.week1.na$time, crypto.week1.na$daily.mean-crypto.week1$sd, lwd=1, col='grey')

# index <- which(diff(crypto.week1.na$time) > 182)
# crypto.week1.na[index,'daily.mean'] <- NA



#week 1 daily mean log scale 
# par(mai=c(1,1.5,1,1))
# plot(crypto.week1$time, crypto.week1$daily.mean, xaxt="n", xlab="", lwd=2, pch=16, ylab="daily mean abundance (10^6 cells/L)- log scale", cex.lab=2, ylog=T, log="y")
# axis.POSIXct(1, crypto.week1$time, format="%D")

#week 1 sd
# par(mai=c(1,1.5,1,1))
# plotCI(crypto.week1$time, crypto.week1$daily.mean, uiw= crypto.week1$sd, sfrac=0, pch=16, 	xlab="", ylab="mean daily abundance (10^6 cells/L)", cex.lab=1.7, xaxt="n")
# axis.POSIXct(1, crypto.week1$time, format="%D")


#week 2
# par(mai=c(1,1.5,1,1))
# plot(crypto.week2$time, crypto.week2$abundance, xaxt="n", xlab="", lwd=2, pch=16, ylab="abundance (10^6 cells/L)", cex.lab=2)
# axis.POSIXct(1, crypto.week2$time, format="%D")

#week 2 log scale
#par(mai=c(1,1.5,1,1))
plot(crypto.week2$time, crypto.week2$abundance, xaxt="n", xlab="", lwd=2, pch=16, ylab="", cex.lab=1.5, log="y", col="darkgrey", las=1, cex.axis=1.2)
points(smooth.spline(crypto.week2.ss2$time, crypto.week2.ss2$abundance, spar=0.5), lwd=2, col="black", pch=16, xlab="", ylab="", axes=F, cex=0.75)
axis.POSIXct(1, crypto.week2$time, at=seq(as.POSIXct("2013-09-16 20:00:00", origin="1970-01-01", tz='GMT'), max(crypto.week2$time, na.rm=T), by=60*60*6), format="%m-%d %H:%M", cex.axis=1.5)
mtext("B", side=3, cex=2, adj=0)

rect(as.POSIXct("2013-09-16 17:43:00", origin="1970-01-01", tz='GMT'), 0.000000001, as.POSIXct("2013-09-16 23:36:00", origin="1970-01-01", tz='GMT'), 49.0, density=NULL, col=adjustcolor("black", alpha=0.07), border=NA)
rect(as.POSIXct("2013-09-17 06:22:00", origin="1970-01-01", tz='GMT'), 0.000000001, as.POSIXct("2013-09-17 12:32:00", origin="1970-01-01", tz='GMT'), 49.0, density=NULL, col=adjustcolor("black", alpha=0.07), border=NA)
rect(as.POSIXct("2013-09-17 18:38:00", origin="1970-01-01", tz='GMT'), 0.000000001, as.POSIXct("2013-09-18 00:32:00", origin="1970-01-01", tz='GMT'), 49.0, density=NULL, col=adjustcolor("black", alpha=0.07), border=NA)
rect(as.POSIXct("2013-09-18 07:06:00", origin="1970-01-01", tz='GMT'), 0.000000001, as.POSIXct("2013-09-18 13:13:00", origin="1970-01-01", tz='GMT'), 49.0, density=NULL, col=adjustcolor("black", alpha=0.07), border=NA)
rect(as.POSIXct("2013-09-18 19:29:00", origin="1970-01-01", tz='GMT'), 0.000000001, as.POSIXct("2013-09-19 01:23:00", origin="1970-01-01", tz='GMT'), 49.0, density=NULL, col=adjustcolor("black", alpha=0.07), border=NA)
rect(as.POSIXct("2013-09-19 07:48:00", origin="1970-01-01", tz='GMT'), 0.000000001, as.POSIXct("2013-09-19 13:52:00", origin="1970-01-01", tz='GMT'), 49.0, density=NULL, col=adjustcolor("black", alpha=0.07), border=NA)
rect(as.POSIXct("2013-09-19 20:16:00", origin="1970-01-01", tz='GMT'), 0.000000001, as.POSIXct("2013-09-20 02:12:00", origin="1970-01-01", tz='GMT'), 49.0, density=NULL, col=adjustcolor("black", alpha=0.07), border=NA)




#week 2 daily mean
# par(mai=c(1,1.5,1,1))
# plot(crypto.week2$time, crypto.week2$daily.mean, xaxt="n", xlab="", lwd=2, pch=16, ylab="daily mean abundance (10^6 cells/L)", cex.lab=2)
# axis.POSIXct(1, crypto.week2$time, format="%D")

#week 2 daily mean log scale
# par(mai=c(1,1.5,1,1))
# plot(crypto.week2$time, crypto.week2$daily.mean, xaxt="n", xlab="", lwd=2, pch=16, ylab="daily mean abundance (10^6 cells/L)- log scale", cex.lab=2, ylog=T, log="y")
# axis.POSIXct(1, crypto.week2$time, format="%D")




#week 3
# par(mai=c(1,1.5,1,1))
# plot(crypto.week3$time, crypto.week3$abundance, xaxt="n", xlab="", lwd=2, pch=16, ylab="abundance (10^6 cells/L)", cex.lab=2)
# axis.POSIXct(1, crypto.week3$time, format="%D")

#week 3 log scale
#par(mai=c(1,1.5,1,1))
plot(crypto.week3$time, crypto.week3$abundance, xaxt="n", xlab="", lwd=2, pch=16, ylab="", cex.lab=1.5, ylog=T, log="y", col="darkgrey", las=1, cex.axis=1.2)
points(smooth.spline(crypto.week3.ss2$time, crypto.week3.ss2$abundance, spar=0.5), lwd=2, col="black", pch=16, xlab="", ylab="", axes=F, cex=0.75)
axis.POSIXct(1, crypto.week3$time, at=seq(min(crypto.week3$time, na.rm=T), max(crypto.week3$time, na.rm=T), by=60*60*6), format="%m-%d %H:%M", cex.axis=1.5)
mtext("C", side=3, cex=2, adj=0)

rect(as.POSIXct("2013-09-23 23:07:00", origin="1970-01-01", tz='GMT'), 0.000000001, as.POSIXct("2013-09-24 05:22:00", origin="1970-01-01", tz='GMT'), 49.0, density=NULL, col=adjustcolor("black", alpha=0.07), border=NA)
rect(as.POSIXct("2013-09-24 10:57:00", origin="1970-01-01", tz='GMT'), 0.000000001, as.POSIXct("2013-09-24 16:47:00", origin="1970-01-01", tz='GMT'), 49.0, density=NULL, col=adjustcolor("black", alpha=0.07), border=NA)
rect(as.POSIXct("2013-09-24 23:52:00", origin="1970-01-01", tz='GMT'), 0.000000001, as.POSIXct("2013-09-25 06:16:00", origin="1970-01-01", tz='GMT'), 49.0, density=NULL, col=adjustcolor("black", alpha=0.07), border=NA)
rect(as.POSIXct("2013-09-25 11:40:00", origin="1970-01-01", tz='GMT'), 0.000000001, as.POSIXct("2013-09-25 17:28:00", origin="1970-01-01", tz='GMT'), 49.0, density=NULL, col=adjustcolor("black", alpha=0.07), border=NA)
rect(as.POSIXct("2013-09-26 00:43:00", origin="1970-01-01", tz='GMT'), 0.000000001, as.POSIXct("2013-09-26 07:16:00", origin="1970-01-01", tz='GMT'), 49.0, density=NULL, col=adjustcolor("black", alpha=0.07), border=NA)
rect(as.POSIXct("2013-09-26 12:33:00", origin="1970-01-01", tz='GMT'), 0.000000001, as.POSIXct("2013-09-26 18:18:00", origin="1970-01-01", tz='GMT'), 49.0, density=NULL, col=adjustcolor("black", alpha=0.07), border=NA)
rect(as.POSIXct("2013-09-16 17:43:00", origin="1970-01-01", tz='GMT'), 0.000000001, as.POSIXct("2013-09-16 23:36:00", origin="1970-01-01", tz='GMT'), 49.0, density=NULL, col=adjustcolor("black", alpha=0.07), border=NA)
rect(as.POSIXct("2013-09-27 01:45:00", origin="1970-01-01", tz='GMT'), 0.000000001, as.POSIXct("2013-09-27 07:24:00", origin="1970-01-01", tz='GMT'), 49.0, density=NULL, col=adjustcolor("black", alpha=0.07), border=NA)


 
 
#week 3 daily mean 
# par(mai=c(1,1.5,1,1))
# plot(crypto.week3$time, crypto.week3$daily.mean, xaxt="n", xlab="", lwd=2, pch=16, ylab="daily mean abundance (10^6 cells/L)", cex.lab=2)
# axis.POSIXct(1, crypto.week3$time, format="%D")

#week 3 daily mean log scale
# par(mai=c(1,1.5,1,1))
# plot(crypto.week3$time, crypto.week3$daily.mean, xaxt="n", xlab="", lwd=2, pch=16, ylab="daily mean abundance (10^6 cells/L)- log scale", cex.lab=2, ylog=T, log="y")
# axis.POSIXct(1, crypto.week3$time, format="%D")

#week 4
# par(mai=c(1,1.5,1,1))
# plot(crypto.week4$time, crypto.week4$abundance, xaxt="n", xlab="", lwd=2, pch=16, ylab="abundance (10^6 cells/L)", cex.lab=2)
# axis.POSIXct(1, crypto.week4$time, format="%D")

#week 4 log scale
#par(mai=c(1,1.5,1,1))
plot(crypto.week4$time, crypto.week4$abundance, xaxt="n", xlab="", lwd=2, pch=16, ylab="", cex.lab=1.5, ylog=T, log="y", col="darkgrey", las=1, cex.axis=1.2)
points(smooth.spline(crypto.week4.ss2$time, crypto.week4.ss2$abundance, spar=0.5), lwd=2, col="black", pch=16, xlab="", ylab="", axes=F, cex=0.75)
axis.POSIXct(1, crypto.week4$time, at=seq(as.POSIXct("2013-09-30 18:50:00", origin="1970-01-01", tz='GMT'), max(crypto.week4$time, na.rm=T), by=60*60*6), format="%m-%d %H:%M", cex.axis=1.5)
mtext("D", side=3, cex=2, adj=0)

rect(as.POSIXct("2013-09-30 17:03:00", origin="1970-01-01", tz='GMT'), 0.000000001, as.POSIXct("2013-09-30 22:53:00", origin="1970-01-01", tz='GMT'), 49.0, density=NULL, col=adjustcolor("black", alpha=0.07), border=NA)
rect(as.POSIXct("2013-10-01 05:32:00", origin="1970-01-01", tz='GMT'), 0.000000001, as.POSIXct("2013-10-01 11:48:00", origin="1970-01-01", tz='GMT'), 49.0, density=NULL, col=adjustcolor("black", alpha=0.07), border=NA)
rect(as.POSIXct("2013-10-01 17:52:00", origin="1970-01-01", tz='GMT'), 0.000000001, as.POSIXct("2013-10-01 22:45:00", origin="1970-01-01", tz='GMT'), 49.0, density=NULL, col=adjustcolor("black", alpha=0.07), border=NA)
rect(as.POSIXct("2013-10-02 06:12:00", origin="1970-01-01", tz='GMT'), 0.000000001, as.POSIXct("2013-10-02 12:23:00", origin="1970-01-01", tz='GMT'), 49.0, density=NULL, col=adjustcolor("black", alpha=0.07), border=NA)
rect(as.POSIXct("2013-10-02 18:37:00", origin="1970-01-01", tz='GMT'), 0.000000001, as.POSIXct("2013-10-03 00:32:00", origin="1970-01-01", tz='GMT'), 49.0, density=NULL, col=adjustcolor("black", alpha=0.07), border=NA)



#week 4 daily mean 
# par(mai=c(1,1.5,1,1))
# plot(crypto.week4$time, crypto.week4$daily.mean, xaxt="n", xlab="", lwd=2, pch=16, ylab="daily mean abundance (10^6 cells/L)", cex.lab=2)
# axis.POSIXct(1, crypto.week4$time, format="%D")

#week 4 daily mean log scale
# par(mai=c(1,1.5,1,1))
# plot(crypto.week4$time, crypto.week4$daily.mean, xaxt="n", xlab="", lwd=2, pch=16, ylab="daily mean abundance (10^6 cells/L)- log scale", cex.lab=2, ylog=T, log="y")
# axis.POSIXct(1, crypto.week4$time, format="%D")


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

par(mfrow=c(4,1), mar=c(2,8,2.5,2)+0.8, pty="m", cex.axis=2)

#TC
#par(mai=c(1,1.5,1,1))
#plotCI(as.POSIXct(yay$h.time, origin="1970-01-01", tz='GMT'), yay$daily.GRmean, uiw= yay$daily.GRse, sfrac=0, pch=16, 	xlab="", ylab="mean daily division rate", cex.lab=1.7)

#week 1
#par(mai=c(1,1.5,1,1))
plotCI(dr.w1$time, dr.w1$daily.GRmean, uiw= dr.w1$daily.GRse, sfrac=0, pch=NA, 	xlab="", ylab="", cex.lab=1.7, xaxt="n", las=1, ylim=c(0.7,2.05), xlim=c(as.POSIXct("2013-09-11 00:00:00", origin="1970-01-01", tz='GMT'), as.POSIXct("2013-09-14 02:00:00", origin="1970-01-01", tz='GMT')))
axis.POSIXct(1, dr.w1$time, at=seq(as.POSIXct("2013-09-11 00:00:00", origin="1970-01-01", tz='GMT'), as.POSIXct("2013-09-14 00:00:00", origin="1970-01-01", tz='GMT'), by=60*60*12), format="%m-%d %H:%M", cex.axis=1.5)
par(new=T)
plot(dr.w1$time, dr.w1$daily.GRmean, xlab="", ylab="", axes=F, type="l", ylim=c(0.7,2.05), xlim=c(as.POSIXct("2013-09-11 00:00:00", origin="1970-01-01", tz='GMT'), as.POSIXct("2013-09-14 02:00:00", origin="1970-01-01", tz='GMT')))
mtext("A", side=3, cex=2, line=0, adj=0)
mtext(text="mean daily division rate", side=2, cex=2.5, outer=T, line=-4)


rect(as.POSIXct("2013-09-10 23:51:00", origin="1970-01-01", tz='GMT'), 0.000000001, as.POSIXct("2013-09-11 06:13:00", origin="1970-01-01", tz='GMT'), 49.0, density=NULL, col=adjustcolor("black", alpha=0.07), border=NA)
rect(as.POSIXct("2013-09-11 11:52:00", origin="1970-01-01", tz='GMT'), 0.000000001, as.POSIXct("2013-09-11 17:41:00", origin="1970-01-01", tz='GMT'), 49.0, density=NULL, col=adjustcolor("black", alpha=0.07), border=NA)
rect(as.POSIXct("2013-09-12 00:55:00", origin="1970-01-01", tz='GMT'), 0.000000001, as.POSIXct("2013-09-12 07:24:00", origin="1970-01-01", tz='GMT'), 49.0, density=NULL, col=adjustcolor("black", alpha=0.07), border=NA)
rect(as.POSIXct("2013-09-12 12:44:00", origin="1970-01-01", tz='GMT'), 0.000000001, as.POSIXct("2013-09-12 18:43:00", origin="1970-01-01", tz='GMT'), 49.0, density=NULL, col=adjustcolor("black", alpha=0.07), border=NA)
rect(as.POSIXct("2013-09-13 02:11:00", origin="1970-01-01", tz='GMT'), 0.000000001, as.POSIXct("2013-09-13 08:41:00", origin="1970-01-01", tz='GMT'), 49.0, density=NULL, col=adjustcolor("black", alpha=0.07), border=NA)
rect(as.POSIXct("2013-09-13 14:05:00", origin="1970-01-01", tz='GMT'), 0.000000001, as.POSIXct("2013-09-13 19:58:00", origin="1970-01-01", tz='GMT'), 49.0, density=NULL, col=adjustcolor("black", alpha=0.07), border=NA)


#week2
#par(mai=c(1,1.5,1,1))
plotCI(dr.w2$time, dr.w2$daily.GRmean, uiw= dr.w2$daily.GRse, sfrac=0, pch=NA, 	xlab="", ylab="", cex.lab=1.7, xaxt="n", las=1, ylim=c(0,1.2),  xlim=c(as.POSIXct("2013-09-17 00:00:00", origin="1970-01-01", tz='GMT'), as.POSIXct("2013-09-19 12:00:00", origin="1970-01-01", tz='GMT')))
axis.POSIXct(1, dr.w2$time, at=seq(as.POSIXct("2013-09-17 07:00:00", origin="1970-01-01", tz='GMT'), max(dr.w2$time, na.rm=T), by=60*60*12), format="%m-%d %H:%M", cex.axis=1.5)
par(new=T)
plot(dr.w2$time, dr.w2$daily.GRmean, xlab="", ylab="", axes=F, type="l", ylim=c(0,1.2),  xlim=c(as.POSIXct("2013-09-17 00:00:00", origin="1970-01-01", tz='GMT'), as.POSIXct("2013-09-19 12:00:00", origin="1970-01-01", tz='GMT')))
mtext("B", side=3, cex=2, line=0, adj=0)


rect(as.POSIXct("2013-09-16 17:43:00", origin="1970-01-01", tz='GMT'), -0.5, as.POSIXct("2013-09-16 23:36:00", origin="1970-01-01", tz='GMT'), 49.0, density=NULL, col=adjustcolor("black", alpha=0.07), border=NA)
rect(as.POSIXct("2013-09-17 06:22:00", origin="1970-01-01", tz='GMT'), -0.5, as.POSIXct("2013-09-17 12:32:00", origin="1970-01-01", tz='GMT'), 49.0, density=NULL, col=adjustcolor("black", alpha=0.07), border=NA)
rect(as.POSIXct("2013-09-17 18:38:00", origin="1970-01-01", tz='GMT'), -0.5, as.POSIXct("2013-09-18 00:32:00", origin="1970-01-01", tz='GMT'), 49.0, density=NULL, col=adjustcolor("black", alpha=0.07), border=NA)
rect(as.POSIXct("2013-09-18 07:06:00", origin="1970-01-01", tz='GMT'), -0.5, as.POSIXct("2013-09-18 13:13:00", origin="1970-01-01", tz='GMT'), 49.0, density=NULL, col=adjustcolor("black", alpha=0.07), border=NA)
rect(as.POSIXct("2013-09-18 19:29:00", origin="1970-01-01", tz='GMT'), -0.5, as.POSIXct("2013-09-19 01:23:00", origin="1970-01-01", tz='GMT'), 49.0, density=NULL, col=adjustcolor("black", alpha=0.07), border=NA)
rect(as.POSIXct("2013-09-19 07:48:00", origin="1970-01-01", tz='GMT'), -0.5, as.POSIXct("2013-09-19 13:52:00", origin="1970-01-01", tz='GMT'), 49.0, density=NULL, col=adjustcolor("black", alpha=0.07), border=NA)
rect(as.POSIXct("2013-09-19 20:16:00", origin="1970-01-01", tz='GMT'), -0.5, as.POSIXct("2013-09-20 02:12:00", origin="1970-01-01", tz='GMT'), 49.0, density=NULL, col=adjustcolor("black", alpha=0.07), border=NA)


#week3
#par(mai=c(1,1.5,1,1))
plotCI(dr.w3$time, dr.w3$daily.GRmean, uiw= dr.w3$daily.GRse, sfrac=0, pch=NA, 	xlab="", ylab="", cex.lab=1.7, xaxt="n", las=1, ylim=c(0,7))
axis.POSIXct(1, dr.w3$time, at=seq(as.POSIXct("2013-09-22 07:00:00", origin="1970-01-01", tz='GMT'), max(dr.w3$time, na.rm=T), by=60*60*12), format="%m-%d %H:%M", cex.axis=1.5)
par(new=T)
plot(dr.w3$time, dr.w3$daily.GRmean, xlab="", ylab="", axes=F, type="l", ylim=c(0,7))
mtext("C", side=3, cex=2, line=0, adj=0)


rect(as.POSIXct("2013-09-23 23:07:00", origin="1970-01-01", tz='GMT'), 0.000000001, as.POSIXct("2013-09-24 05:22:00", origin="1970-01-01", tz='GMT'), 49.0, density=NULL, col=adjustcolor("black", alpha=0.07), border=NA)
rect(as.POSIXct("2013-09-24 10:57:00", origin="1970-01-01", tz='GMT'), 0.000000001, as.POSIXct("2013-09-24 16:47:00", origin="1970-01-01", tz='GMT'), 49.0, density=NULL, col=adjustcolor("black", alpha=0.07), border=NA)
rect(as.POSIXct("2013-09-24 23:52:00", origin="1970-01-01", tz='GMT'), 0.000000001, as.POSIXct("2013-09-25 06:16:00", origin="1970-01-01", tz='GMT'), 49.0, density=NULL, col=adjustcolor("black", alpha=0.07), border=NA)
rect(as.POSIXct("2013-09-25 11:40:00", origin="1970-01-01", tz='GMT'), 0.000000001, as.POSIXct("2013-09-25 17:28:00", origin="1970-01-01", tz='GMT'), 49.0, density=NULL, col=adjustcolor("black", alpha=0.07), border=NA)
rect(as.POSIXct("2013-09-26 00:43:00", origin="1970-01-01", tz='GMT'), 0.000000001, as.POSIXct("2013-09-26 07:16:00", origin="1970-01-01", tz='GMT'), 49.0, density=NULL, col=adjustcolor("black", alpha=0.07), border=NA)
rect(as.POSIXct("2013-09-26 12:33:00", origin="1970-01-01", tz='GMT'), 0.000000001, as.POSIXct("2013-09-26 18:18:00", origin="1970-01-01", tz='GMT'), 49.0, density=NULL, col=adjustcolor("black", alpha=0.07), border=NA)
rect(as.POSIXct("2013-09-16 17:43:00", origin="1970-01-01", tz='GMT'), 0.000000001, as.POSIXct("2013-09-16 23:36:00", origin="1970-01-01", tz='GMT'), 49.0, density=NULL, col=adjustcolor("black", alpha=0.07), border=NA)
rect(as.POSIXct("2013-09-27 01:45:00", origin="1970-01-01", tz='GMT'), 0.000000001, as.POSIXct("2013-09-27 07:24:00", origin="1970-01-01", tz='GMT'), 49.0, density=NULL, col=adjustcolor("black", alpha=0.07), border=NA)



#week4
#par(mai=c(1,1.5,1,1))
plotCI(dr.w4$time,dr.w4$daily.GRmean, uiw= dr.w4$daily.GRse, sfrac=0, pch=NA, 	xlab="", ylab="", cex.lab=1.7, xaxt="n", las=1, ylim=c(0,12) )
axis.POSIXct(1, dr.w4$time, at=seq(as.POSIXct("2013-09-29 11:00:00", origin="1970-01-01", tz='GMT'), max(dr.w4$time, na.rm=T), by=60*60*12), format="%m-%d %H:%M", cex.axis=1.5)
par(new=T)
plot(dr.w4$time, dr.w4$daily.GRmean, xlab="", ylab="", axes=F, type="l", ylim=c(0,12))
mtext("D", side=3, cex=2, line=0, adj=0)

rect(as.POSIXct("2013-09-30 17:03:00", origin="1970-01-01", tz='GMT'), 0.000000001, as.POSIXct("2013-09-30 22:53:00", origin="1970-01-01", tz='GMT'), 49.0, density=NULL, col=adjustcolor("black", alpha=0.07), border=NA)
rect(as.POSIXct("2013-10-01 05:32:00", origin="1970-01-01", tz='GMT'), 0.000000001, as.POSIXct("2013-10-01 11:48:00", origin="1970-01-01", tz='GMT'), 49.0, density=NULL, col=adjustcolor("black", alpha=0.07), border=NA)
rect(as.POSIXct("2013-10-01 17:52:00", origin="1970-01-01", tz='GMT'), 0.000000001, as.POSIXct("2013-10-01 22:45:00", origin="1970-01-01", tz='GMT'), 49.0, density=NULL, col=adjustcolor("black", alpha=0.07), border=NA)
rect(as.POSIXct("2013-10-02 06:12:00", origin="1970-01-01", tz='GMT'), 0.000000001, as.POSIXct("2013-10-02 12:23:00", origin="1970-01-01", tz='GMT'), 49.0, density=NULL, col=adjustcolor("black", alpha=0.07), border=NA)
rect(as.POSIXct("2013-10-02 18:37:00", origin="1970-01-01", tz='GMT'), 0.000000001, as.POSIXct("2013-10-03 00:32:00", origin="1970-01-01", tz='GMT'), 49.0, density=NULL, col=adjustcolor("black", alpha=0.07), border=NA)



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

# par(mai=c(1,1.5,1,1))
# plotCI(as.POSIXct(yay2$h.time, origin="1970-01-01", tz='GMT'), yay2$daily.GRmean, uiw= yay2$daily.GRsd, sfrac=0, pch=16, 	xlab="", ylab="mean daily division rate", cex.lab=1.7)
# #ylim=c(0,20)
# par(new=T)
# plot(pre.flu3$time2, pre.flu3$Nitrate, lwd=2, pch=16, cex=1, xlab="", ylab="", cex.lab=2,  axes=F, col="darkred", type="o")
# axis(4)
# mtext("Nitrate", side=4, lin=3, cex=1.7)



###########################
#### div rate vs. meso ####
###########################

par(mai=c(1,1.5,1,1))
plotCI(as.POSIXct(yay$h.time, origin="1970-01-01", tz='GMT'), yay$daily.GRmean, uiw= yay$daily.GRsd, sfrac=0, pch=16, 	xlab="", ylab="mean daily division rate", cex.lab=1.7)
#ylim=c(0,20)
par(new=T)
plot(meso$time2, meso$particles_mL, lwd=6, pch=16, xlab="", ylab="", cex.lab=2,  axes=F, col="red", type="h")
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


###################################
#### nutrients vs. div no time ####
###################################
quartz("Quartz", width=10, height=10)
par(mfrow=c(2,2), mar=c(1,0.5,0.5,0.2)+3.7, pty="s", las=1, cex.axis=2)

#nitrate
#par(mai=c(1,1,1,7))
plot(cmop$n, cmop$dm, pch=16, xlab="nitrate", ylab="mean daily division rate", cex.lab=1.7, cex=1.5)
#res1=lm(cmop$dm~cmop$n)
res1 <- lmodel2(cmop$dm~cmop$n, data=cmop, nperm=0, range.x="relative", range.y="relative")
par(new=T)
plot(res1, "MA", ylab="", xlab="", main="", col="black")
text(5.5,8, substitute(paste("R"^{2}, "=0.017")), cex=2)
mtext("A", side=3, cex=2, adj=0)

#phosphate
# par(mai=c(1,1,1,7))
plot(cmop$ph, cmop$dm, pch=16, xlab="phosphate", ylab="", cex.lab=1.7, cex=1.5)
#res2=lm(cmop$dm~cmop$ph)
res2 <- lmodel2(cmop$dm~cmop$ph, data=cmop, nperm=0, range.x="relative", range.y="relative")
par(new=T)
plot(res2, "MA", ylab="", xlab="", main="", col="black")
text(0.95,8, substitute(paste("R"^{2}, "=0.267")), cex=2)
mtext("B", side=3, cex=2, adj=0)

#ammonium
# par(mai=c(1,1,1,7))
plot(cmop$a, cmop$dm, pch=16, xlab="ammonia", ylab="mean daily division rate", cex.lab=1.7, cex=1.5)
#res3=lm(cmop$dm~cmop$a)
res3 <- lmodel2(cmop$dm~cmop$a, data=cmop, nperm=0, range.x="relative", range.y="relative")
par(new=T)
plot(res3, "MA", ylab="", xlab="", main="", col="black")
text(11,8, substitute(paste("R"^{2}, "=0.076")), cex=2)
mtext("C", side=3, cex=2, adj=0)

#par(mai=c(1,1,1,7))
plot(cmop$par.cum, cmop$dm, pch=16, xlab="PAR", ylab="", cex.lab=1.7, cex=1.5)
#res4=lm(cmop$dm~cmop$par.cum)
res4 <- lmodel2(cmop$dm~cmop$par.cum, data=cmop, nperm=0, range.x="relative", range.y="relative")
par(new=T)
plot(res4, "MA", ylab="", xlab="", main="", col="black")
text(140,8, substitute(paste("R"^{2}, "=0.214")), cex=2)
mtext("D", side=3, cex=2, adj=0)


#substitute(paste("abundance 10"^{6}, " cells L"^{-1}))


#############
#### map ####
#############

#estuary map
map("worldHires", "Canada", xlim=c(-124.5,-123.15), ylim=c(45.9,46.5), col="lightcyan", fill=T)
map("worldHires","usa", xlim=c(-124.5,-123.15), ylim=c(45.9,46.5), col="grey", fill=T, add=T)  
lat<-c(46.21)
lon<-c(-123.91)
points(lon, lat, pch=18, col="red", cex=1.5)
map.scale(-123.5, 46, ratio=F)
text(-123.85, 46.23, "SATURN03", cex=1.5)
text(-124.3, 46.4, "Columbia River \nEstuary", cex=3)

#washington/oregon map
map("usa", xlim=c(-127,-120), ylim=c(44,50), col="grey", fill=T)  

map("state", add=T)
lat<-c(46.21)
lon<-c(-123.91)
points(lon, lat, pch=18, col="red", cex=2)
map.scale(-122.5, 44.3, ratio=F)
text(-125, 46.4, "Pacific \nOcean", cex=2)
text(-122.65, 45, "Oregon", cex=1.5)
text(-122.5, 46.5, "Washington", cex=1.5)


#bathymetry 
# blues <- c("lightsteelblue4", "lightsteelblue3", "lightsteelblue2", "lightsteelblue1")
# CMOP_bathy <- getNOAA.bathy(lon1= -124.5, lon2= -123, lat1= 45.9, lat2= 46.5, resolution= 1)
# plot(CMOP_bathy, image=T, land=T, lwd =0.1, bpal= list(c(0, max(CMOP_bathy), "grey"), c(min(CMOP_bathy), 0, blues)))
# plot(CMOP_bathy, deep=0, shallow= 0, step=0, lwd=0.4, add=T)
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
plot(tx$time2, tx$percent2, lwd=2, pch=16, cex=2, xlab="", ylab="percent Teleaulax of total cryptophytes", cex.lab=2, ylim=c(0,0.5), xlim=c(as.POSIXct("2013-09-10 00:00:00"), as.POSIXct("2013-10-03 24:00:00")))
par(new=T)
plot(meso$time2, meso$particles_mL, lwd=2, pch=4, cex=1, xlab="", ylab="", cex.lab=2,  axes=F, xlim=c(as.POSIXct("2013-09-10 00:00:00"), as.POSIXct("2013-10-03 24:00:00")))
axis(4)
mtext("Mesodinium counts", side=4, lin=3, cex=1.7)
legend("topright", c("Mesodinium counts", "percent Teleaulax of total cryptophytes"), pch=c(4,16))

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

# par(pty='m')
# plotCI(as.POSIXct(cc$time, origin="1970-01-01"), cc$div, cc$div.se, ylim=c(0,0.05), sfrac=0, lwd=2, pch=16, cex=1,ylab=NA, xlab=NA)
# plotCI(m$time, m$div.ave, m$div.sd, col=2,add=T, sfrac=0, lwd=2, pch=16,cex=1)
# mtext(substitute(paste("Division (h"^{-1},")")), side=2, line=3, cex=1)
# mtext("time", side=1, line=3, cex=1)

m$time2 <- as.POSIXct(m$time, origin="1970-01-01", tz="GMT" )
cc$time2 <- as.POSIXct(cc$time, origin="1970-01-01", tz="GMT" )

m.time.new <- subset(m, time2 > as.POSIXct("2014-09-22 19:00:00", origin="1970-01-01", tz='GMT') & time2 < as.POSIXct("2014-09-23 23:00:00", origin="1970-01-01", tz='GMT'))

m.full <- m.time.new$div.ave
remove <- c(1, 3, 5, 7, 9, 11, 14, 16, 18, 20, 22, 24, 26)
m.new <- m.full[-remove]
cc.new <- cc$div
both <- data.frame(m.new, cc.new)


# begin plot #
quartz("Quartz", width=15, height=7)
par(mfrow=c(1,2), mar=c(4,7,1,2)+1, pty="m")

plot(as.POSIXct(cc$time, origin="1970-01-01"), cc$div, ylim=c(0,0.05), sfrac=0, lwd=2, pch=16, cex=1.5,ylab=NA, xlab=NA, cex.axis=1.5, las=1, xaxt="n")

plotCI(m$time, m$div.ave, m$div.sd, col="darkgrey",add=T, sfrac=0, lwd=2, pch=16,cex=1.5, las=1)
axis.POSIXct(1, m$time2, at=seq(as.POSIXct("2014-09-22 20:00:00", origin="1970-01-01", tz='GMT'), max(m$time2, na.rm=T), by=60*60*4), format="%H:%M", cex.axis=1.5)

mtext(substitute(paste("division (h"^{-1},")")), side=2, line=5, cex=1.8)
legend("topleft", c("cell cycle", "model"), lty=c(1,1), lwd=c(3,3), col=c("black", "darkgrey"), cex=1.2)



plot(both$m.new, both$cc.new, pch=16, cex=1.5, ylab=NA, xlab=NA, las=1, cex.axis=1.5)
mtext(substitute(paste("division (h"^{-1},")")), side=2, line=5, cex=1.8)
mtext(substitute(paste("division (h"^{-1},")")), side=1, line=4, cex=1.8)
res5 <- lmodel2(both$cc.new~both$m.new, data=both, nperm=0, range.x="relative", range.y="relative")
par(new=T)
plot(res5, "MA", ylab="", xlab="", main="", col="black", axes=F)
text(0.018,0.04, substitute(paste("R"^{2}, "=0.597")), cex=2)

tiff(filename="/Users/francois/CMOP/manuscript/third_draft_figures/cc_m")
dev.off()






