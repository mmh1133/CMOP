user <- '/Users/mariaham/CMOP'
user <- '~/Documents/DATA/SeaFlow/CMOP/CMOP_git'
stat <- read.csv(paste0(user, "/CMOP_field/model/crypto_HD_CMOP_6.binned.csv"))
stat$h.time <- as.POSIXct(stat$h.time,origin='1970-01-01',tz='GMT')


 
###############
### FIGURE 1 ###
###############
library(maps)
library(mapdata)
library(maptools)
library(scales)
library(mapproj)

png("Figure1.png", width=114, height=114, pointsize=8, res=600, units="mm")

#estuary map
par(mfrow=c(2,1), mar=c(0,0,0,0), oma=c(0,0,0,0))
map("worldHires", 'usa', xlim=c(-140, -50), ylim=c(25,50),col="grey", interior=F, fill=T)  
lat<-c(46.21)
lon<-c(-123.91)
#points(lon, lat, pch=16, col="red", cex=3)
polygon(c(-125,-122.5,-122.5,-125), c(45,45,47,47), border='black', lwd=1.5)

#washington/oregon map
map("worldHires", xlim=c(-124.5,-123.15), ylim=c(45.9,46.5), col="lightgrey", fill=T)  
lat<-c(46.21)
lon<-c(-123.91)
points(lon, lat, pch=16, col="black", cex=3)
map.scale(-123, 46, ratio=F)
text(-124.2, 46.2, "Pacific \nOcean", cex=1.5)
text(-123.5, 46.4, "Columbia River \nEstuary", cex=1.5)
box(col='black', lwd=1.5)

dev.off()







##################
### HYDROLOGY ###
##################

i <- min(stat$h.time, na.rm=T)
f <- max(stat$h.time, na.rm=T)

sal <- read.csv(paste0(user, "/auxillary_data/salinityCMOP_6"))
    sal$time <- as.POSIXct(strptime(sal$time.YYYY.MM.DD.hh.mm.ss.PST., "%Y/%m/%d %H:%M:%S"), tz="GMT")
    sal <- subset(sal, time > i & time < f)
    id <- which(diff(sal$time) > 60*60*3)
        sal.LPF <- smooth.spline(sal$time, sal$water_salinity, spar=0.002)
        sal.LPF$y[id] <- NA
        temp.LPF <- smooth.spline(sal$time, sal$water_temperature, spar=0.002)
        temp.LPF$y[id] <- NA      
par <- read.csv(paste0(user, "/CMOP_field/Par_CMOP_6"))
    par$time <- as.POSIXct(par$time, format="%Y/%m/%d %H:%M:%S",  tz= "GMT")
    par <- subset(par, time > i & time < f)
    par.LPF <- smooth.spline(as.POSIXct(par$time, origin="1970-01-01", tz='GMT'), par$par, spar=0.05)
    par.LPF$y[which(par.LPF$y < 40)] <- 0
    par.LPF$y[which(diff(par.LPF$x)>8000)] <- NA
nut <- read.csv(paste0(user, "/auxillary_data/Ribalet_nutrients2.csv"))
    nut$time <- as.POSIXct(strptime(nut$time, "%Y/%m/%d %H:%M:%S"), tz="GMT")
    nut <- subset(nut, time > i & time < f)
    #nut[which(diff(nut$time)>1),"time"] <- NA
    nut$DIN <- nut$Nitrate+nut$Nitrite+nut$Ammonium

ph <- read.csv(paste0(user, "/auxillary_data/pHCMOP_6"))
    ph$time <- as.POSIXct(strptime(ph$time.YYYY.MM.DD.hh.mm.ss.PST., "%Y/%m/%d %H:%M:%S"), tz="GMT")
    ph <- subset(ph, time > i & time < f)
        id <- which(diff(ph$time) > 60*60*3)
        ph.LPF <- smooth.spline(as.POSIXct(ph$time, origin="1970-01-01", tz='GMT'), ph$ph, spar=0.05)
        ph.LPF$y[id] <- NA

### PLOT ABUNDANCE vs SALINITY
# time.template <- seq(i, f, by=60*60)
# time.res <- cut(crypto$h.time,time.template)
# hourly.abun.mean <-  tapply(crypto$h2.conc.mean, time.res, function(x) mean(x, na.rm=T))

# time.res <- cut(sal$time,time.template)
# hourly.sal.mean <-  tapply(sal$water_salinity, time.res, function(x) mean(x, na.rm=T))
# id <- unique(c(which(is.na(hourly.sal.mean)),which(is.na(hourly.abun.mean))))

# plot(hourly.sal.mean, hourly.abun.mean, log='y')
# lines(smooth.spline(hourly.sal.mean[-id], hourly.abun.mean[-id], spar=0.99), col=2)






png("Figure2.png", width=114*2, height=114*1.5, pointsize=8, res=600, units="mm")

par(mfrow=c(3,1), mar=c(3,2,1,2), pty="m", cex=1.2, oma=c(1,3,1,3))

plot(sal.LPF$x, sal.LPF$y,  xlab="", ylab="", xlim=c(i,f), type='l', xaxt='n', yaxt='n', lwd=1.5, ylim=c(0,30))
axis(2, at=c(0, 30, 15), las=1)
axis.POSIXct(1, at=seq(i, f, by=60*60*24*6), labels=c(1,7,14,21))
mtext("salinity (psu)",side=2, cex=1.2, line=3)
par(new=T)
plot(temp.LPF$x, temp.LPF$y,  xlab="", ylab="", xlim=c(i,f), type='l', xaxt='n', yaxt='n', col='darkgrey', lwd=1.5, ylim=c(13,21))
axis(4, at=c(13,17,21),las=1)
mtext("A", side=3, cex=2, adj=0)
mtext(expression(paste("temperature (",degree,"C)")),side=4, cex=1.2, line=3)

plot(par.LPF$x, par.LPF$y,  xlab="", ylab="", xlim=c(i,f), type='l', xaxt='n', yaxt='n', lwd=1.5)
axis(2, at=c(0, 300, 600), las=1)
axis.POSIXct(1, at=seq(i, f, by=60*60*24*6), labels=c(1,7,14,21))
mtext(substitute(paste("PAR (",mu, "E m"^{-1},"s"^{-1},')')),side=2, cex=1.2, line=3)
mtext("B", side=3, cex=2, adj=0)
par(new=T)
plot(ph.LPF$x, ph.LPF$y,  xlab="", ylab="", xlim=c(i,f), type='l', xaxt='n', yaxt='n', col='darkgrey', lwd=1.5)
axis(4, at=c(7.8, 8.1, 8.4),las=1)
mtext('pH',side=4, cex=1.2, line=3)

plot(nut$time, nut$DIN,  xlab="", ylab="", xlim=c(i,f), type='o', xaxt='n', yaxt='n', lwd=1.5, ylim=c(5,35))
axis(2, at=c(5, 20,35), las=1)
mtext(substitute(paste("DIN (",mu, "M)")),side=2, cex=1.2, line=3)

par(new=T)
plot(nut$time, nut$Phosphate,  xlab="", ylab="", xlim=c(i,f), type='o', pch=16, xaxt='n', yaxt='n', lwd=1.5, ylim=c(0,1.6))
axis(4, at=c(0, 0.8,1.6), las=1)
axis.POSIXct(1, at=seq(i, f, by=60*60*24*6), labels=c(1,7,14,21))
mtext(substitute(paste("phosphate (",mu, "M)")),side=4, cex=1.2, line=3)
mtext("C", side=3, cex=2, adj=0)
mtext("time (d)", side=1, cex=1.2, outer=T, line=-1)

dev.off()



###############################
### PH / DIN CORRELATION ###
###############################
nut <- read.csv(paste0(user, "/auxillary_data/Ribalet_nutrients2.csv"))
    nut$time <- as.POSIXct(strptime(nut$time, "%Y/%m/%d %H:%M:%S"), tz="GMT")
    nut <- subset(nut, time > i & time < f)
    #nut[which(diff(nut$time)>1),"time"] <- NA
    nut$DIN <- nut$Nitrate+nut$Nitrite+nut$Ammonium

    time.template <- seq(i, f, by=60*60*24)

id <- findInterval(nut$time,time.template, all.inside=T)
id <- c(2:4,7:11,14:18,21,22)
time.res <- cut(ph$time, time.template)
daily.ph <-  tapply(ph$ph, time.res, function(x) mean(x, na.rm=T))[id]

data <- data.frame(cbind(DIN=nut$DIN, P04 =nut$Phosphate, PH=daily.ph))

DIN.ph <- lmodel2(PH ~ DIN, data,"relative", "relative", 99)
P04.ph <- lmodel2(PH ~ P04, data,"relative", "relative", 99)

png("Figure3.png", width=114*2, height=114*2, pointsize=8, res=600, units="mm")

par(mfrow=c(2,2), mar=c(3,2,2,2), pty="s", cex=1.2, oma=c(1,3,1,0))

plot(data[,c(1,3)],yaxt='n',ylim=c(7.8,8.4),xlim=c(5,35), yaxt='n', xaxt='n', xlab=NA, ylab=NA)
abline(b=DIN.ph$regression.results[4,3],a=DIN.ph$regression.results[4,2], lty=2)
axis(2, at=c(7.8,8.1,8.4),las=1)
axis(1, at=c(5,20,35))
text(y=8.3,x=31.5,substitute(paste("R"^{2}, "=0.34")), cex=1)
#mtext("A", side=3, cex=2, adj=0)
mtext("pH",side=2, cex=1.2, line=3)
mtext(substitute(paste("DIN (",mu, "M)")),side=1, cex=1.2, line=2.5)

# plot(data[,c(2,3)],yaxt='n',ylim=c(7.8,8.4),xlim=c(0,1.6), yaxt='n', xaxt='n', xlab=NA, ylab=NA)
# abline(b=P04.ph$regression.results[4,3],a=P04.ph$regression.results[4,2], lty=2)
# axis(2, at=c(7.8,8.1,8.4),las=1)
# axis(1, at=c(0,0.8,1.6))
# text(y=8.3,x=1.4, substitute(paste("R"^{2}, "=0.33")), cex=1)
# mtext("B", side=3, cex=2, adj=0)
# mtext(substitute(paste("Phosphate (",mu, "M)")),side=1, cex=1.2, line=2.5)

dev.off()







###################
### ABUNDANCES ###
###################

library(plotrix)
library(zoo)

stat$h.dr.mean <- as.numeric(c('NA', na.approx(stat$h.dr.mean)))
stat$h.dr.sd <- as.numeric(c('NA','NA', na.approx(stat$h.dr.sd), 'NA'))
crypto <- subset(stat, h.time > as.POSIXct("2013-09-10 16:50:00") & h.time < as.POSIXct("2013-10-03 24:00:00"))
crypto.week1 <- subset(crypto ,  h.time < as.POSIXct("2013-09-13 16:00:00",tz='GMT'))
crypto.week2 <- subset(crypto , h.time > as.POSIXct("2013-09-16 18:00:00", tz='GMT') & h.time < as.POSIXct("2013-09-20 1:00:00", tz='GMT')) 
crypto.week3 <- subset(crypto , h.time > as.POSIXct("2013-09-23 22:00:00", tz='GMT') & h.time < as.POSIXct("2013-09-27 10:00:00", tz='GMT')) 
crypto.week4 <- subset(crypto , h.time > as.POSIXct("2013-09-30 18:00:00", tz='GMT') & h.time < as.POSIXct("2013-10-03 24:00:00", tz='GMT')) 


meso <- read.csv(paste0(user,"/mesodinium/Meso.csv"))
meso$Time <- as.POSIXct(meso$Time, format="%m/%d/%Y %H:%M") 
meso$Meso <- meso$Meso/1000

png("Figure4.png", width=114*2, height=114*1.5, pointsize=8, res=600, units="mm")
par(mfrow=c(4,1), mar=c(3,2,1,2), pty="m", cex=1.2, oma=c(1,3,1,3))

#week 1 
df <- crypto.week1
i <- min(df$h.time, na.rm=T)
f <- i + 60*60*24*3.5
plotCI(df$h.time, df$h2.conc.mean, uiw=df$h2.conc.sd, sfrac=0, xaxt='n',xlab="", lwd=2, pch=16, ylab= "", cex.lab=1.5, log="y", col="darkgrey", las=1, ylim=c(0.010, 4), xlim=c(i,f), yaxt='n')
lines(df$h.time, df$h2.conc.mean)
axis(2, at=c(0.02,0.2,2), las=1)
axis(1, at=seq(i, f, by=60*60*24), labels=seq(1,4,by=1))
rect(as.POSIXct("2013-09-10 23:51:00", origin="1970-01-01", tz='GMT'), 0.000000001, as.POSIXct("2013-09-11 06:13:00", origin="1970-01-01", tz='GMT'), 60.0, density=NULL, col=adjustcolor("black", alpha=0.15), border=NA)
rect(as.POSIXct("2013-09-11 11:52:00", origin="1970-01-01", tz='GMT'), 0.000000001, as.POSIXct("2013-09-11 17:41:00", origin="1970-01-01", tz='GMT'), 60.0, density=NULL, col=adjustcolor("black", alpha=0.15), border=NA)
rect(as.POSIXct("2013-09-12 00:55:00", origin="1970-01-01", tz='GMT'), 0.000000001, as.POSIXct("2013-09-12 07:24:00", origin="1970-01-01", tz='GMT'), 60.0, density=NULL, col=adjustcolor("black", alpha=0.15), border=NA)
rect(as.POSIXct("2013-09-12 12:44:00", origin="1970-01-01", tz='GMT'), 0.000000001, as.POSIXct("2013-09-12 18:43:00", origin="1970-01-01", tz='GMT'), 60.0, density=NULL, col=adjustcolor("black", alpha=0.15), border=NA)
rect(as.POSIXct("2013-09-13 02:11:00", origin="1970-01-01", tz='GMT'), 0.000000001, as.POSIXct("2013-09-13 08:41:00", origin="1970-01-01", tz='GMT'), 60.0, density=NULL, col=adjustcolor("black", alpha=0.15), border=NA)
rect(as.POSIXct("2013-09-13 14:05:00", origin="1970-01-01", tz='GMT'), 0.000000001, as.POSIXct("2013-09-13 19:58:00", origin="1970-01-01", tz='GMT'), 60.0, density=NULL, col=adjustcolor("black", alpha=0.15), border=NA)
rect(as.POSIXct("2013-09-14 03:28:00", origin="1970-01-01", tz='GMT'), 0.000000001, as.POSIXct("2013-09-14 09:54:00", origin="1970-01-01", tz='GMT'), 60.0, density=NULL, col=adjustcolor("black", alpha=0.15), border=NA)
rect(as.POSIXct("2013-09-14 15:29:00", origin="1970-01-01", tz='GMT'), 0.000000001, as.POSIXct("2013-09-14 21:18:00", origin="1970-01-01", tz='GMT'), 60.0, density=NULL, col=adjustcolor("black", alpha=0.15), border=NA)
mtext("A", side=3, cex=2, line=0, adj=0)
mtext(substitute(paste("abundance (10"^{6}, " cells L"^{-1},')')), side=2, cex=1.2, outer=T, line=1)
mtext("time (d)", side=1, cex=1.2, outer=T, line=-1)
points(meso$Time, meso$Meso, pch=16, cex=1.5)

#week 2 
df <- crypto.week2
i <- min(df$h.time, na.rm=T)
f <- i + 60*60*24*3.5
plotCI(df$h.time, df$h2.conc.mean, uiw=df$h2.conc.sd, sfrac=0, xaxt='n',xlab="", lwd=2, pch=16, ylab= "", cex.lab=1.5, log="y", col="darkgrey", las=1, ylim=c(0.010, 4), xlim=c(i,f), yaxt='n')
lines(df$h.time, df$h2.conc.mean)
axis(2, at=c(0.02,0.2,2), las=1)
axis(1, at=seq(i, f, by=60*60*24), labels=seq(1,4,by=1)+6)
mtext("B", side=3, cex=2, adj=0)
rect(as.POSIXct("2013-09-16 17:43:00", origin="1970-01-01", tz='GMT'), 0.000000001, as.POSIXct("2013-09-16 23:36:00", origin="1970-01-01", tz='GMT'), 60.0, density=NULL, col=adjustcolor("black", alpha=0.15), border=NA)
rect(as.POSIXct("2013-09-17 06:22:00", origin="1970-01-01", tz='GMT'), 0.000000001, as.POSIXct("2013-09-17 12:32:00", origin="1970-01-01", tz='GMT'), 60.0, density=NULL, col=adjustcolor("black", alpha=0.15), border=NA)
rect(as.POSIXct("2013-09-17 18:38:00", origin="1970-01-01", tz='GMT'), 0.000000001, as.POSIXct("2013-09-18 00:32:00", origin="1970-01-01", tz='GMT'), 60.0, density=NULL, col=adjustcolor("black", alpha=0.15), border=NA)
rect(as.POSIXct("2013-09-18 07:06:00", origin="1970-01-01", tz='GMT'), 0.000000001, as.POSIXct("2013-09-18 13:13:00", origin="1970-01-01", tz='GMT'), 60.0, density=NULL, col=adjustcolor("black", alpha=0.15), border=NA)
rect(as.POSIXct("2013-09-18 19:29:00", origin="1970-01-01", tz='GMT'), 0.000000001, as.POSIXct("2013-09-19 01:23:00", origin="1970-01-01", tz='GMT'), 60.0, density=NULL, col=adjustcolor("black", alpha=0.15), border=NA)
rect(as.POSIXct("2013-09-19 07:48:00", origin="1970-01-01", tz='GMT'), 0.000000001, as.POSIXct("2013-09-19 13:52:00", origin="1970-01-01", tz='GMT'), 60.0, density=NULL, col=adjustcolor("black", alpha=0.15), border=NA)
rect(as.POSIXct("2013-09-19 20:16:00", origin="1970-01-01", tz='GMT'), 0.000000001, as.POSIXct("2013-09-20 02:12:00", origin="1970-01-01", tz='GMT'), 60.0, density=NULL, col=adjustcolor("black", alpha=0.15), border=NA)
points(meso$Time, meso$Meso, pch=16, cex=1.5)


#week 3 
df <- crypto.week3
i <- min(df$h.time, na.rm=T)
f <- i + 60*60*24*3.5
plotCI(df$h.time, df$h2.conc.mean, uiw=df$h2.conc.sd, sfrac=0, xaxt='n',xlab="", lwd=2, pch=16, ylab= "", cex.lab=1.5, log="y", col="darkgrey", las=1, ylim=c(0.010, 4), xlim=c(i,f), yaxt='n')
lines(df$h.time, df$h2.conc.mean)
axis(2, at=c(0.02,0.2,2), las=1)
axis(1, at=seq(i, f, by=60*60*24), labels=seq(1,4,by=1)+13)
mtext("C", side=3, cex=2, adj=0)
rect(as.POSIXct("2013-09-23 23:07:00", origin="1970-01-01", tz='GMT'), 0.000000001, as.POSIXct("2013-09-24 05:22:00", origin="1970-01-01", tz='GMT'), 60.0, density=NULL, col=adjustcolor("black", alpha=0.15), border=NA)
rect(as.POSIXct("2013-09-24 10:57:00", origin="1970-01-01", tz='GMT'), 0.000000001, as.POSIXct("2013-09-24 16:47:00", origin="1970-01-01", tz='GMT'), 60.0, density=NULL, col=adjustcolor("black", alpha=0.15), border=NA)
rect(as.POSIXct("2013-09-24 23:52:00", origin="1970-01-01", tz='GMT'), 0.000000001, as.POSIXct("2013-09-25 06:16:00", origin="1970-01-01", tz='GMT'), 60.0, density=NULL, col=adjustcolor("black", alpha=0.15), border=NA)
rect(as.POSIXct("2013-09-25 11:40:00", origin="1970-01-01", tz='GMT'), 0.000000001, as.POSIXct("2013-09-25 17:28:00", origin="1970-01-01", tz='GMT'), 60.0, density=NULL, col=adjustcolor("black", alpha=0.15), border=NA)
rect(as.POSIXct("2013-09-26 00:43:00", origin="1970-01-01", tz='GMT'), 0.000000001, as.POSIXct("2013-09-26 07:16:00", origin="1970-01-01", tz='GMT'), 60.0, density=NULL, col=adjustcolor("black", alpha=0.15), border=NA)
rect(as.POSIXct("2013-09-26 12:33:00", origin="1970-01-01", tz='GMT'), 0.000000001, as.POSIXct("2013-09-26 18:18:00", origin="1970-01-01", tz='GMT'), 60.0, density=NULL, col=adjustcolor("black", alpha=0.15), border=NA)
rect(as.POSIXct("2013-09-16 17:43:00", origin="1970-01-01", tz='GMT'), 0.000000001, as.POSIXct("2013-09-16 23:36:00", origin="1970-01-01", tz='GMT'), 60.0, density=NULL, col=adjustcolor("black", alpha=0.15), border=NA)
rect(as.POSIXct("2013-09-27 01:45:00", origin="1970-01-01", tz='GMT'), 0.000000001, as.POSIXct("2013-09-27 07:24:00", origin="1970-01-01", tz='GMT'), 60.0, density=NULL, col=adjustcolor("black", alpha=0.15), border=NA)
points(meso$Time, meso$Meso, pch=16, cex=1.5)


#week 4 
df <- crypto.week4
i <- min(df$h.time, na.rm=T)
f <- i + 60*60*24*3.5
plotCI(df$h.time, df$h2.conc.mean, uiw=df$h2.conc.sd, sfrac=0, xaxt='n',xlab="", lwd=2, pch=16, ylab= "", cex.lab=1.5, log="y", col="darkgrey", las=1, ylim=c(0.010, 4), xlim=c(i,f), yaxt='n')
lines(df$h.time, df$h2.conc.mean)
axis(2, at=c(0.02,0.2,2), las=1)
axis(1, at=seq(i, f, by=60*60*24), labels=seq(1,4,by=1)+20)
mtext("D", side=3, cex=2, adj=0)
rect(as.POSIXct("2013-09-30 17:03:00", origin="1970-01-01", tz='GMT'), 0.000000001, as.POSIXct("2013-09-30 22:53:00", origin="1970-01-01", tz='GMT'), 60.0, density=NULL, col=adjustcolor("black", alpha=0.15), border=NA)
rect(as.POSIXct("2013-10-01 05:32:00", origin="1970-01-01", tz='GMT'), 0.000000001, as.POSIXct("2013-10-01 11:48:00", origin="1970-01-01", tz='GMT'), 60.0, density=NULL, col=adjustcolor("black", alpha=0.15), border=NA)
rect(as.POSIXct("2013-10-01 17:52:00", origin="1970-01-01", tz='GMT'), 0.000000001, as.POSIXct("2013-10-01 23:45:00", origin="1970-01-01", tz='GMT'), 60.0, density=NULL, col=adjustcolor("black", alpha=0.15), border=NA)
rect(as.POSIXct("2013-10-02 06:12:00", origin="1970-01-01", tz='GMT'), 0.000000001, as.POSIXct("2013-10-02 12:23:00", origin="1970-01-01", tz='GMT'), 60.0, density=NULL, col=adjustcolor("black", alpha=0.15), border=NA)
rect(as.POSIXct("2013-10-02 18:37:00", origin="1970-01-01", tz='GMT'), 0.000000001, as.POSIXct("2013-10-03 00:32:00", origin="1970-01-01", tz='GMT'), 60.0, density=NULL, col=adjustcolor("black", alpha=0.15), border=NA)
rect(as.POSIXct("2013-10-03 06:50:00", origin="1970-01-01", tz='GMT'), 0.000000001, as.POSIXct("2013-10-03 12:54:00", origin="1970-01-01", tz='GMT'), 60.0, density=NULL, col=adjustcolor("black", alpha=0.15), border=NA)
rect(as.POSIXct("2013-10-03 19:18:00", origin="1970-01-01", tz='GMT'), 0.000000001, as.POSIXct("2013-10-04 01:16:00", origin="1970-01-01", tz='GMT'), 60.0, density=NULL, col=adjustcolor("black", alpha=0.15), border=NA)
rect(as.POSIXct("2013-10-04 07:26:00", origin="1970-01-01", tz='GMT'), 0.000000001, as.POSIXct("2013-10-04 13:25:00", origin="1970-01-01", tz='GMT'), 60.0, density=NULL, col=adjustcolor("black", alpha=0.15), border=NA)
points(meso$Time, meso$Meso, pch=16, cex=1.5)


dev.off()



###############################
### ABUNDANCE CORRELATION ###
###############################
library(lmodel2)

meso <- read.csv(paste0(user,"/mesodinium/Meso.csv"))
meso$Time <- as.POSIXct(meso$Time, format="%m/%d/%Y %H:%M")
meso$Meso <- meso$Meso/1000
id <- c(41,56,142,164,10,215,237,322, 341, 357, 377, 401,452,479)
data <- data.frame(cbind(meso=meso$Meso, crypto=crypto$h2.conc.mean[id]))#*crypto$h.dr.mean[id]))
reg <- lmodel2(meso ~ crypto, data,"relative", "relative", 99)
reg.log <- lmodel2(crypto ~ meso, log10(data),"interval", "interval", 99)


png("Figure5.png", width=114*2, height=114*1.5, pointsize=8, res=600, units="mm")

par(mfrow=c(2,2), mar=c(3,2,1,2), pty="s", cex=1.2, oma=c(1,3,1,0))
plot(data[,c(1,2)],  xlab=NA, ylab=NA, yaxt='n', xaxt='n',asp=1, log='xy', xlim=c(0.02, 2),ylim=c(0.02, 2))
axis(2, at=c(0.02,0.2,2), las=1)
axis(1, at=c(0.02,0.2,2))
abline(b=reg.log$regression.results[4,3],a=reg.log$regression.results[4,2], lty=2)
text(1,0.03,substitute(paste("R"^{2}, "=0.51")), cex=1)
mtext(substitute(paste("TLC (10"^{6}, " cells L"^{-1},')')), side=2, cex=1.2,  line=3)
mtext(substitute(paste("                 (10"^{6}, " cells L"^{-1},')')), side=1, cex=1.2,  line=3)
mtext("M. major                      ", side=1, cex=1.2,  line=2.83, font=3)
points(log(data[1,c(1,2)]), col=2)

dev.off()





#####################
### CORRELATIONS ###
#####################

library(lmodel2)


i <- min(stat$h.time, na.rm=T)
f <- max(stat$h.time, na.rm=T)

nut <- read.csv(paste0(user, "/auxillary_data/Ribalet_nutrients2.csv"))
    nut$time <- as.POSIXct(strptime(nut$time, "%Y/%m/%d %H:%M:%S"), tz="GMT")
    nut <- subset(nut, time > i & time < f)
    #nut[which(diff(nut$time)>1),"time"] <- NA
    nut$DIN <- nut$Nitrate+nut$Nitrite+nut$Ammonium
    nut <- nut[-c(4,5,8,9,12,16),]

id <- c(1,2,3,9,10,14,15,17,20,21)

time.template <- seq(i, f, by=60*60*24)
time.res <- cut(crypto$h.time,time.template)
daily.abun.mean <-  tapply(crypto$h2.conc.mean, time.res, function(x) mean(x, na.rm=T))
daily.abun.sd <-  tapply(crypto$h2.conc.sd, time.res, function(x) mean(x, na.rm=T))
daily.dr.mean <-   tapply(crypto$h.dr.mean, time.res, function(x) mean(x, na.rm=T))*24
daily.dr.sd <-   tapply(crypto$h.dr.sd, time.res, function(x) mean(x, na.rm=T))*24
daily.prod.mean <- c(daily.dr.mean * daily.abun.mean)[id] 
daily.prod.sd <- c(daily.dr.sd * daily.abun.sd)[id] 
daily.dr.mean <- daily.dr.mean[id] 
daily.dr.se <- daily.dr.sd[id] / sqrt(24)
daily.abun.mean <- daily.abun.mean[id] 

time.res <- cut(par$time,time.template)
daily.par <- tapply(par$par, time.res, function(x) mean(x, na.rm=T))[id] 

time.res <- cut(sal$time, time.template)
daily.temp <-  tapply(sal$water_temperature, time.res, function(x) mean(x, na.rm=T))[id] 
daily.sal <-  tapply(sal$water_salinity, time.res, function(x) mean(x, na.rm=T))[id] 

time.res <- cut(ph$time, time.template)
daily.ph <-  tapply(ph$ph, time.res, function(x) mean(x, na.rm=T))[id] 



data <- data.frame(cbind(DIN=nut$DIN, P04 =nut$Phosphate, TEMP=daily.temp, SAL=daily.sal, 
    PH=daily.ph, PAR=daily.par, PROD=daily.prod.mean, DR=daily.dr.mean,
    PROD.sd = daily.prod.sd , DR.se = daily.dr.se,N=daily.abun.mean))
time <- as.POSIXct(rownames(data), tz='GMT')

DIN.P <- lmodel2(PROD ~ DIN, data,"relative", "relative", 99)
P04.P <- lmodel2(PROD ~ P04, data,"relative", "relative", 99)
DIN.DR <- lmodel2(DR ~ DIN, data,"relative", "relative", 99)
P04.DR <- lmodel2(DR ~ P04, data,"relative", "relative", 99)
sal.DR <- lmodel2(DR ~ SAL, data,"relative", "relative", 99)
temp.DR <- lmodel2(DR ~ TEMP, data,"relative", "relative", 99)
ph.DR <- lmodel2(DR ~ PH, data,"relative", "relative", 99)
ph.P <- lmodel2(PROD ~ PH, data,"relative", "relative", 99)
DIN.ph <- lmodel2(PH ~ DIN, data,"relative", "relative", 99)
P04.ph <- lmodel2(PH ~ P04, data,"relative", "relative", 99)
DIN.N <- lmodel2(DIN ~ N, data,"relative", "relative", 99)


png("Figure6.png", width=114*2, height=114*1.5, pointsize=8, res=600, units="mm")
par(mfrow=c(2,1), mar=c(3,2,1,2), pty="m", cex=1.2, oma=c(1,3,1,3))
plotCI(time, data$DR, uiw=data$DR.se, sfrac=0, xlab="", lwd=2, pch=16, ylab= "", cex.lab=1.5, col="darkgrey", las=1, yaxt='n',xlim=c(i,f), xaxt='n')
lines(time, data$DR)
axis(2, at=seq(0,2.5,0.5), las=1)
axis.POSIXct(1, at=seq(min(time), max(time), by=60*60*24*6), labels=c(1,7,14,21))
mtext(substitute(paste("division (d"^{-1},')')), side=2, cex=1.2, line=3)
mtext("time (d)", side=1, cex=1.2, line=2)# par(new=T)
# plotCI(time, data$PROD, uiw=data$PROD.sd, sfrac=0, xlab="", lwd=2, pch=1, ylab= "", cex.lab=1.5, col="darkgrey", las=1, yaxt='n',xlim=c(i,f), xaxt='n')
# lines(time, data$PROD, col='darkgrey')
# axis(4, at=c(0,0.4,0.8), las=1)
# mtext(substitute(paste("production (10"^{6},"cell L"^{-1},"d"^{-1},')')), side=4, cex=1.2, line=3)
dev.off()








png("Figure7.png", width=114*2, height=114*2, pointsize=8, res=600, units="mm")

par(mfrow=c(2,2), mar=c(3,2,2,3), pty="s", cex=1.2, oma=c(1,3,1,0))

plot(data[,c(5,7)], ylim=c(0,0.8), yaxt='n', xaxt='n', xlab=NA, ylab=NA, xlim=c(7.8,8.4))
axis(2, at=c(0,0.4,0.8), las=1)
axis(1, at=c(7.8, 8.1, 8.4))
abline(b=ph.P$regression.results[4,3],a=ph.P$regression.results[4,2], lty=2)
text(8.3,0.7,substitute(paste("R"^{2}, "=0.53")), cex=1)
#text(10,0.6,"p < 0.01", cex=0.75)
mtext(substitute(paste("production (10"^{6},"cell L"^{-1},"d"^{-1},')')), side=2, cex=1.2, line=3)
mtext("pH",side=1, cex=1.2, line=2.5)
mtext("A", side=3, cex=2, adj=0)

plot(data[,c(1,7)], ylim=c(0,0.8), yaxt='n', xaxt='n', xlab=NA, ylab=NA, xlim=c(5,35))
axis(2, at=c(0,0.4,0.8), las=1)
axis(1, at=c(5,20,35))
abline(b=DIN.P$regression.results[4,3],a=DIN.P$regression.results[4,2], lty=2)
text(10,0.7,substitute(paste("R"^{2}, "=0.58")), cex=1)
#text(10,0.6,"p < 0.01", cex=0.75)
mtext(substitute(paste("DIN (",mu, "M)")),side=1, cex=1.2, line=2.5)
mtext("B", side=3, cex=2, adj=0)
#mtext(substitute(paste("production (10"^{6},"cell L"^{-1},"d"^{-1},')')), side=2, cex=1.2, line=3)

plot(data[,c(5,8)],ylim=c(0,1.6), yaxt='n',xlim=c(7.8,8.4),yaxt='n', xaxt='n', xlab=NA, ylab=NA)
axis(1, at=c(7.8,8.1,8.4))
axis(2, at=c(0,0.8,1.6),las=1)
mtext("pH",side=1, cex=1.2, line=2.5)
mtext("C", side=3, cex=2, adj=0)
abline(b=ph.DR$regression.results[4,3],a=ph.DR$regression.results[4,2], lty=2)
text(8.3,1.4,substitute(paste("R"^{2}, "=0.41")), cex=1)
mtext(substitute(paste("division (d"^{-1},')')), side=2, cex=1.2, line=3)


dev.off()

