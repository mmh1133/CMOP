library(popcycle)
set.evt.location("/Volumes/seaflow/CMOP_6")
set.project.location("~/CMOP_2013_f2")
set.cruise.id("CMOP_6")
cruise <-"CMOP_6"

library(rgl)
library(ggplot2)
library(zoo)
library(plotrix)
library(ade4)
library(vegan)
library(gclus)
library(ape)




#### setting up binned data ####

yay <- read.csv("/Users/francois/CMOP/CMOP_field/model/crypto_HD_CMOP_6V3.binned.csv")

yay$daily.GRmean <- rollapply(data=yay$h.dr.mean, width=24, FUN=mean, na.rm=T, fill=NA)*24
yay$daily.GRsd <- rollapply(data=yay$h.dr.sd, width=24, FUN=mean, na.rm=T, fill=NA)*24

yay$time <- as.POSIXct(yay$h.time,tz='GMT', origin="1970-01-01")
yay2 <- subset(yay, time > as.POSIXct("2013-09-10 16:50:00") & time < as.POSIXct("2013-09-20 00:00:00")) 

day1 <- subset(yay2, time > as.POSIXct("2013-09-11 00:00:00") & time < as.POSIXct("2013-09-12 00:00:00"))
day2 <- subset(yay2, time > as.POSIXct("2013-09-12 00:00:00") & time < as.POSIXct("2013-09-13 00:00:00"))
day3 <- subset(yay2, time > as.POSIXct("2013-09-13 00:00:00") & time < as.POSIXct("2013-09-14 00:00:00"))
day4 <- subset(yay2, time > as.POSIXct("2013-09-14 00:00:00") & time < as.POSIXct("2013-09-15 00:00:00"))
day5 <- subset(yay2, time > as.POSIXct("2013-09-15 00:00:00") & time < as.POSIXct("2013-09-16 00:00:00"))
day6 <- subset(yay2, time > as.POSIXct("2013-09-16 00:00:00") & time < as.POSIXct("2013-09-17 00:00:00"))
day7 <- subset(yay2, time > as.POSIXct("2013-09-17 00:00:00") & time < as.POSIXct("2013-09-18 00:00:00"))
day8 <- subset(yay2, time > as.POSIXct("2013-09-18 00:00:00") & time < as.POSIXct("2013-09-19 00:00:00"))
day9 <- subset(yay2, time > as.POSIXct("2013-09-19 00:00:00") & time < as.POSIXct("2013-09-20 00:00:00"))


d1m <- mean(day1$daily.GRmean, na.rm=T)
d2m <- mean(day2$daily.GRmean, na.rm=T)
d3m <- mean(day3$daily.GRmean, na.rm=T)
d4m <- mean(day4$daily.GRmean, na.rm=T)
d5m <- mean(day5$daily.GRmean, na.rm=T)  #NA
d6m <- mean(day6$daily.GRmean, na.rm=T)  #NA
d7m <- mean(day7$daily.GRmean, na.rm=T)
d8m <- mean(day8$daily.GRmean, na.rm=T)
d9m <- mean(day9$daily.GRmean, na.rm=T)

dm <- c(d1m, d2m, d3m, d7m, d8m, d9m)

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


 p <- c(533.739, 272.192, 386.239, 722.297, 626.298, 661.172)
 
 


#### setting up nutrient data ####

pre.flu <- read.csv("/Users/francois/CMOP/auxillary_data/Ribalet_nutrients2.csv")
pre.flu2 <- as.data.frame(pre.flu, row.names=NULL)
pre.flu2$time2 <- as.POSIXct(strptime(pre.flu2$time, "%Y/%m/%d %H:%M:%S"), tz="GMT")
pre.flu3 <- subset(pre.flu2, time2 > as.POSIXct("2013-09-10 16:50:00") & time2 < as.POSIXct("2013-09-20 00:00:00")) 

n <- c(12.7, 13.3, 13.2, 7.1, 8.6, 5.9)
ph <- c(0.9, 1.1, 0.9, 0.9, 1.1, 0.6)



#### combinding vectors into a data frame ####

cmop <- data.frame(dm, p, n, ph)


#########################
#### actual PCA time ####
#########################

pca.cmop <- rda(cmop, scale=T)
summary(pca.cmop)

par(mfrow=c(1,2))
#cleanplot.pca(pca.cmop, point=T)
biplot(pca.cmop)


par(mai=c(1,1,1,1))
plot(cmop$n, cmop$dm, pch=16, xlab="nitrate", ylab="mean daily division rate", cex.lab=1.7, cex=1.5, col="darkred")


res=lm(cmop$dm~cmop$n)
abline(res)

