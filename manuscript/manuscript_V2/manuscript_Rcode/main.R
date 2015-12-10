library(maps)
library(mapdata)
library(maptools)
library(scales)
library(mapproj)
library(plotrix)
library(zoo)
library(lmodel2)



user <- '/Users/mariaham/CMOP'
user <- '~/Documents/DATA/SeaFlow/CMOP/CMOP_git/manuscript/manuscript_V2/manuscript_Rcode/'
# user <- NULL # when use in DataJoy



stat <- read.csv(paste0(user, "crypto_HD_CMOP_6.binned.csv"))
    stat$h.time <- as.POSIXct(stat$h.time,origin='1970-01-01',tz='GMT')
    i <- min(stat$h.time, na.rm=T)
    f <- max(stat$h.time, na.rm=T)
    # stat$h.dr.mean <- as.numeric(c('NA', na.approx(stat$h.dr.mean)))
    # stat$h.dr.sd <- as.numeric(c('NA','NA', na.approx(stat$h.dr.sd), 'NA'))

crypto <- subset(stat, h.time > as.POSIXct("2013-09-10 16:50:00", tz='GMT') & h.time < as.POSIXct("2013-10-03 24:00:00",tz='GMT'))
    crypto.week1 <- subset(crypto ,  h.time < as.POSIXct("2013-09-13 16:00:00",tz='GMT'))
    crypto.week2 <- subset(crypto , h.time > as.POSIXct("2013-09-16 18:00:00", tz='GMT') & h.time < as.POSIXct("2013-09-20 1:00:00", tz='GMT')) 
    crypto.week3 <- subset(crypto , h.time > as.POSIXct("2013-09-23 22:00:00", tz='GMT') & h.time < as.POSIXct("2013-09-27 10:00:00", tz='GMT')) 
    crypto.week4 <- subset(crypto , h.time > as.POSIXct("2013-09-30 18:00:00", tz='GMT') & h.time < as.POSIXct("2013-10-03 24:00:00", tz='GMT')) 
    time.template <- seq(i, f+60*60*48, by=60*60*24)
    time.res <- cut(crypto$h.time,time.template)
    daily.abun.mean <-  tapply(crypto$h2.conc.mean, time.res, function(x) mean(x, na.rm=T))
    daily.abun.sd <-  tapply(crypto$h2.conc.sd, time.res, function(x) mean(x, na.rm=T))
    daily.dr.mean <-   tapply(crypto$h.dr.mean, time.res, function(x) mean(x, na.rm=T))*24
    daily.dr.sd <-   tapply(crypto$h.dr.sd, time.res, function(x) mean(x, na.rm=T))*24
    daily.prod.mean <- c(daily.dr.mean * daily.abun.mean)
    daily.prod.sd <- c(daily.dr.sd * daily.abun.sd)
    daily.dr.se <- daily.dr.sd / sqrt(24)


sal <- read.csv(paste0(user, "salinityCMOP_6"))
    sal$time <- as.POSIXct(strptime(sal$time.YYYY.MM.DD.hh.mm.ss.PST., "%Y/%m/%d %H:%M:%S"), tz="GMT")
    sal <- subset(sal, time > i & time < f)
    id <- which(diff(sal$time) > 60*60*3)
        sal.LPF <- smooth.spline(sal$time, sal$water_salinity, spar=0.002)
        sal.LPF$y[id] <- NA
        temp.LPF <- smooth.spline(sal$time, sal$water_temperature, spar=0.002)
        temp.LPF$y[id] <- NA      

par <- read.csv(paste0(user, "Par_CMOP_6"))
    par$time <- as.POSIXct(par$time, format="%Y/%m/%d %H:%M:%S",  tz= "GMT")
    par <- subset(par, time > i & time < f)
    par.LPF <- smooth.spline(as.POSIXct(par$time, origin="1970-01-01", tz='GMT'), par$par, spar=0.05)
    par.LPF$y[which(par.LPF$y < 40)] <- 0
    par.LPF$y[which(diff(par.LPF$x)>8000)] <- NA

# oxy <- read.csv(paste0(user, "oxygenCMOP_6"))
#     oxy$time <- as.POSIXct(strptime(oxy$time.YYYY.MM.DD.hh.mm.ss.PST., "%Y/%m/%d %H:%M:%S"), tz="GMT")
#     oxy<- subset(oxy, time > i & time < f)

fluo <- read.csv(paste0(user, "fvfmCMOP_6"))
    fluo$time <- as.POSIXct(strptime(fluo$time.YYYY.MM.DD.hh.mm.ss.PST., "%Y/%m/%d %H:%M:%S"), tz="GMT")
    fluo<- subset(fluo, time > i & time < f) 
  id <- which(diff(fluo$time) > 60*60*3)
        fluo.LPF <- smooth.spline(fluo$time, fluo$fo)#, spar=0.01, df=2)
        fluo.LPF$y[id] <- NA

tide <- read.csv(paste0(user, "elevationCMOP_6"))
   tide$time <- as.POSIXct(strptime(tide$time.YYYY.MM.DD.hh.mm.ss.PST., "%Y/%m/%d %H:%M:%S"), tz="GMT")
    tide <- subset(tide, time > i & time < f) 
    tide <- subset(tide, elevation > 1) 

nut <- read.csv(paste0(user, "Ribalet_nutrients2.csv"))
    nut$time <- as.POSIXct(strptime(nut$time, "%m/%d/%y %H:%M"))
    nut$DIN <- nut$Nitrate+nut$Nitrite+nut$Ammonium
    time.template3 <- seq(min(nut$time), max(nut$time), by=60*60*24)[-1]
    time.res <- cut(nut$time,time.template3)
    daily.DIN <-  tapply(nut$DIN , time.res, function(x) mean(x, na.rm=T))
        daily.DIN.sd <-  tapply(nut$DIN , time.res, function(x) sd(x, na.rm=T))
    daily.PO4 <-  tapply(nut$Phosphate , time.res, function(x) mean(x, na.rm=T))
        daily.PO4.sd <-  tapply(nut$Phosphate , time.res, function(x) sd(x, na.rm=T))
    time.nut <- tapply(nut$time , time.res, function(x) mean(x, na.rm=T))

ph <- read.csv(paste0(user, "pHCMOP_6"))
    ph$time <- as.POSIXct(strptime(ph$time.YYYY.MM.DD.hh.mm.ss.PST., "%Y/%m/%d %H:%M:%S"), tz="GMT")
    ph <- subset(ph, time > i & time < f)
        id <- which(diff(ph$time) > 60*60*3)
        ph.LPF <- smooth.spline(as.POSIXct(ph$time, origin="1970-01-01", tz='GMT'), ph$ph, spar=0.05)
        ph.LPF$y[id] <- NA

influx <- read.csv(paste0(user,"summary_V2-FR.csv"))
info <- read.csv(paste0(user,"file_names.csv"))
fcm <- merge(influx, info, by="file")
    fcm$time <- as.POSIXct(fcm$time, format="%m/%d/%y %I:%M:%S %p")#+ 8*60*60
    fcm <- fcm[order(fcm$time),]
    pop <- subset(fcm, i =='crypto' & depth=="S")[-c(1,10),]
    pop$conc <- 10^-3*pop$n/pop$vol

id <- c(1, 18, 43,  66,  144, 169, 193, 217, 314, 339, 359, 381, 396, 403,454, 478, 502)
data.influx <- data.frame(cbind(fcm=pop$conc, tlc=stat$h2.conc.mean[id]))
cor.influx <- lmodel2(tlc~ fcm, data.influx,"relative", "relative", 99)

meso <- read.csv(paste0(user,"meso.csv"))
    meso$Time <- as.POSIXct(meso$Time, format="%m/%d/%Y %H:%M", tz='')
    meso[1:2,"Time"] <- meso[1:2,"Time"] -14*60*60
    meso$Meso <- meso$Meso/1000

id <- c(25, 55, 143, 179, 217, 240, 324, 337, 359, 380, 396, 455, 483, 502)
data.field <- data.frame(cbind(meso=meso$Meso, crypto=crypto$h2.conc.mean[id]))#*crypto$h.dr.mean[id]))
reg <- lmodel2(meso ~ crypto, data.field,"relative", "relative", 99)
reg.log <- lmodel2(crypto ~ meso, log10(data.field),"interval", "interval", 99)

fsc <- read.csv(paste0(user, "FSCvsPAR.csv"))
    night.fsc <- subset(fsc , par  < 2)
    fsc[which(fsc$fsc2 == "#VALUE!"),'fsc2'] <- NA
    fsc$vol <- 10^(1.2384*log10(fsc$fsc/100)+1.003)
    fsc$vol.sd <- 10^(1.2384*log10(fsc$fsc.sd/100)+1.003)
    fsc$diam <- 2*((fsc$vol  *3)/(pi*4))^(1/3)

m <- read.csv(paste0(user,"model_output-V2.csv"))
    m$time <- as.POSIXct(m$time, origin="1970-01-01", tz="" )

cc <- read.csv(paste0(user,"RHODO_div-rate.csv"))[-1,]
    cc$time <- as.POSIXct(cc$time, origin="1970-01-01", tz="" )


id <- findInterval(cc$time,m$time)
data.cc <- data.frame(cbind(CC=cc$div, DR=m$div.ave[id], CC.sd=na.approx(cc$div.se), DR.sd=na.approx(m$div.se[id])))
c <- lmodel2(CC ~ DR, data.cc,"relative", "relative", 99)


lab <- read.delim(paste0(user,"stat.tab"))
    lab$time <- as.POSIXct(lab$time,tz='')-8*60*60
    rhodo <- subset(lab, pop == 'crypto' & flag == 0) 
    time.template2 <- seq(min(rhodo$time), max(rhodo$time), by=60*60)
    time.res <- cut(rhodo$time,time.template2)
    h.abun.mean <-  tapply(rhodo$abundance, time.res, function(x) mean(x, na.rm=T))
    h.abun.sd <-  tapply(rhodo$abundance, time.res, function(x) sd(x, na.rm=T))
    h.fsc.mean <-  tapply(rhodo$fsc_small, time.res, function(x) mean(x, na.rm=T))
    h.fsc.sd <-  tapply(rhodo$fsc_small, time.res, function(x) sd(x, na.rm=T))
    h.time <- tapply(rhodo$time, time.res, function(x) min(x, na.rm=T))

    #dark cycle: GMT 16:00-23:00 
    night.lab <- subset( rhodo , time >= as.POSIXct("2014-09-22 16:00:00")-8*60*60 &  time <= as.POSIXct("2014-09-22 23:00:00")-8*60*60)
    night2.lab <- subset( rhodo , time >= as.POSIXct("2014-09-23 16:00:00")-8*60*60 &  time <= as.POSIXct("2014-09-23 23:00:00")-8*60*60)


time.res <- cut(par$time,time.template)
daily.par <- tapply(par$par, time.res, function(x) mean(x, na.rm=T))

time.res <- cut(sal$time, time.template)
daily.temp <-  tapply(sal$water_temperature, time.res, function(x) mean(x, na.rm=T))
daily.sal <-  tapply(sal$water_salinity, time.res, function(x) mean(x, na.rm=T))

time.res <- cut(ph$time, time.template)
daily.ph <-  tapply(ph$ph, time.res, function(x) mean(x, na.rm=T))

id <- c(1,3,8,14,15,16,17,22)
data <- data.frame(cbind(time= time.template[-24], 
    DIN=daily.DIN, P04 =daily.PO4, TEMP=daily.temp, SAL=daily.sal, PH=daily.ph, PAR=daily.par, 
    PROD=daily.prod.mean, DR=daily.dr.mean,PROD.sd = daily.prod.sd, DR.se = daily.dr.se,N=daily.abun.mean))
data.dr <- data[id,]



DIN.ph <- lmodel2(PH ~ DIN, data,"relative", "relative", 99)
P04.ph <- lmodel2(PH ~ P04, data,"relative", "relative", 99)
    DIN.N <- lmodel2(DIN ~ N, data,"relative", "relative", 99)
    P04.N <- lmodel2(P04 ~ N, data,"relative", "relative", 99)
    ph.N <- lmodel2(PH ~ N, data,"relative", "relative", 99)
    DR.N <- lmodel2(DR ~ N, data,"relative", "relative", 99)

DIN.P <- lmodel2(PROD ~ DIN, data.dr,"relative", "relative", 99)
    P04.P <- lmodel2(PROD ~ P04, data.dr,"relative", "relative", 99)
DIN.DR <- lmodel2(DR ~ DIN, data.dr,"relative", "relative", 99)
P04.DR <- lmodel2(DR ~ P04, data.dr,"relative", "relative", 99)
    sal.DR <- lmodel2(DR ~ SAL, data.dr,"relative", "relative", 99)
    temp.DR <- lmodel2(DR ~ TEMP, data.dr,"relative", "relative", 99)
ph.DR <- lmodel2(DR ~ PH, data.dr,"relative", "relative", 99)
ph.P <- lmodel2(PROD ~ PH, data.dr,"relative", "relative", 99)






time.template3 <- seq(min(fluo$time), max(fluo$time), by=60*60)
time.res <- cut(fluo.LPF$x,time.template3)
h.fluo.mean <-  tapply(fluo.LPF$y, time.res, function(x) mean(x, na.rm=T))
time.res <- cut(tide$time,time.template3)
h.elevation.mean <-  tapply(tide$elevation, time.res, function(x) mean(x, na.rm=T))

data.cor <- data.frame(cbind(time=time.template3[-1], fluo=h.fluo.mean, elevation=h.elevation.mean ))

fluo.elev <- lmodel2(fluo ~ elevation, data.cor, "relative", "relative", 99)

plot(data.cor$elevation, data.cor$fluo)




###########
### MAP ###
###########


png("FigureS1.png", width=114, height=114, pointsize=8, res=600, units="mm")

    par(mfrow=c(2,1), mar=c(0,0,0,0), oma=c(0,0,0,0))
    map("worldHires", 'usa', xlim=c(-140, -50), ylim=c(25,50),col="grey", interior=F, fill=T)  
    lat<-c(46.21)
    lon<-c(-123.91)
    polygon(c(-125,-122.5,-122.5,-125), c(45,45,47,47), border='black', lwd=1.5)
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

png("Figure1.png", width=114*2, height=114*1.5, pointsize=8, res=600, units="mm")

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

    plot(fluo.LPF$x, fluo.LPF$y/1000,  xlab="", ylab="", xlim=c(i,f), type='l', xaxt='n', yaxt='n', lwd=1.5)
    axis(2, at=c(0.6, 1.2, 1.8), las=1)
    axis.POSIXct(1, at=seq(i, f, by=60*60*24*6), labels=c(1,7,14,21))
    #mtext(substitute(paste("PAR (",mu, "E m"^{-1},"s"^{-1},')')),side=2, cex=1.2, line=3)
    mtext("red fluo (rfu)",side=2, cex=1.2, line=3)
    mtext("B", side=3, cex=2, adj=0)
    par(new=T)
    plot(ph.LPF$x, ph.LPF$y,  xlab="", ylab="", xlim=c(i,f), type='l', xaxt='n', yaxt='n', col='darkgrey', lwd=1.5)
    axis(4, at=c(7.8, 8.1, 8.4),las=1)
    mtext('pH',side=4, cex=1.2, line=3)

    plotCI(time.nut, daily.DIN,  daily.DIN.sd, xlab="", ylab="", xlim=c(i,f), sfrac=0, xaxt='n', yaxt='n', lwd=1.5, ylim=c(5,35),pch=16)
    points(time.nut, daily.DIN, lwd=1.5)
    axis(2, at=c(5, 20,35), las=1)
    mtext(substitute(paste("DIN (",mu, "M)")),side=2, cex=1.2, line=3)

    par(new=T)
    plotCI(time.nut, daily.PO4,  daily.PO4.sd, sfrac=0, xlab="", ylab="", xlim=c(i,f), pch=16, xaxt='n', yaxt='n', lwd=1.5, ylim=c(0.4,1.6),col='darkgrey')
    points(time.nut, daily.PO4, lwd=1.5, col='darkgrey')
    axis(4, at=c(0.4, 1,1.6), las=1)
    axis.POSIXct(1, at=seq(i, f, by=60*60*24*6), labels=c(1,7,14,21))
    mtext(substitute(paste("DIP (",mu, "M)")),side=4, cex=1.2, line=3)
    mtext("C", side=3, cex=2, adj=0)
    mtext("time (d)", side=1, cex=1.2, outer=T, line=-1)


dev.off()




### PH / DIN CORRELATION 

png("FigureS2.png", width=114*2, height=114, pointsize=8, res=600, units="mm")

par(mfrow=c(1,2), mar=c(3,2,2,2), pty="s", cex=1.2, oma=c(1,3,1,0))

    plot(data[,c(3,6)],yaxt='n',ylim=c(7.8,8.4),xlim=c(0,1.6), yaxt='n', xaxt='n', xlab=NA, ylab=NA)
    abline(b=P04.ph$regression.results[4,3],a=P04.ph$regression.results[4,2], lty=2)
    axis(2, at=c(7.8,8.1,8.4),las=1)
    axis(1, at=c(0,0.8,1.6))
    text(y=8.3,x=1.4,substitute(paste("R"^{2}, "=0.29")), cex=1)
    mtext("pH",side=2, cex=1.2, line=3)
    mtext(substitute(paste("DIP (",mu, "M)")),side=1, cex=1.2, line=2.5)
    mtext("A", side=3, cex=2, line=0, adj=0)

    plot(data[,c(2,6)],yaxt='n',ylim=c(7.8,8.4),xlim=c(5,35), yaxt='n', xaxt='n', xlab=NA, ylab=NA)
    abline(b=DIN.ph$regression.results[4,3],a=DIN.ph$regression.results[4,2], lty=2)
    axis(2, at=c(7.8,8.1,8.4),las=1)
    axis(1, at=c(5,20,35))
    text(y=8.3,x=31.5,substitute(paste("R"^{2}, "=0.37")), cex=1)
    mtext(substitute(paste("DIN (",mu, "M)")),side=1, cex=1.2, line=2.5)
    mtext("B", side=3, cex=2, line=0, adj=0)

dev.off()




###################
### ABUNDANCES ###
###################


### INFLUX VALIDATION

png("FigureS3.png", width=114*1.5, height=114*1.5, pointsize=8, res=600, units="mm")

par(mfrow=c(2,1), mar=c(3,2,2,2), pty="m", cex=1.2, oma=c(1,3,1,3))

    plot(stat$h.time, stat$h2.conc.mean, type='l', xaxt='n', yaxt='n', ylim=c(0.01,3), log='y')
    #points(stat$h.time[id], stat$h2.conc.mean[id],col=3,pch=16)
    points(pop$time, pop$conc, col=2)
    axis(1, at=seq(min(stat$h.time, na.rm=T), max(stat$h.time, na.rm=T), by=60*60*24*6), labels=c(1,7,14,21))
    axis(2, at=c(0.02,0.2,2),las=1)
    mtext("time (d)", side=1, cex=1.2, line= 2.5)
    mtext(substitute(paste("abundance (10"^{6}, " cells L"^{-1},')')), side=2, cex=1.2,  line=3)
    mtext("A", side=3, cex=2, adj=0)

    par(pty='s')
    plot(data.influx, log='xy', xlim=c(0.02,2), xaxt='n', yaxt='n', ylim=c(0.02,2), ylab=NA, xlab=NA)
    axis(2, at=c(0.02,0.2,2),las=1)
    axis(1, at=c(0.02,0.2,2))
    mtext(substitute(paste("SeaFlow - abundance (10"^{6}, " cells L"^{-1},')')), side=2, cex=1.2,  line=3)
    mtext(substitute(paste("Influx - abundance (10"^{6}, " cells L"^{-1},')')), side=1, cex=1.2,  line=3)
    mtext("B", side=3, cex=2, adj=0)
    abline(b=cor.influx$regression.results[4,3],a=cor.influx$regression.results[4,2], lty=2)
    text(0.05,1,substitute(paste("R"^{2}, "=0.83")), cex=1)

dev.off()






png("Figure3.png", width=114*2, height=114*1.5, pointsize=8, res=600, units="mm")

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
    axis(1, at=seq(i, f, by=60*60*24), labels=seq(1,4,by=1)+14)
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
    axis(1, at=seq(i, f, by=60*60*24), labels=seq(1,4,by=1)+22)
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






### CRYPTO / MESO CORRELATION 


png("FigureS4.png", width=114, height=114, pointsize=8, res=600, units="mm")

par(mfrow=c(1,1), mar=c(3,2,1,2), pty="s", cex=1.2, oma=c(1,3,1,0))

    plot(data.field[,c(1,2)],  xlab=NA, ylab=NA, yaxt='n', xaxt='n',asp=1, xlim=c(0, 0.35),ylim=c(0, 0.35))
    axis(2, at=c(0, 0.15,0.3), las=1)
    axis(1, at=c(0, 0.15,0.3))
    abline(b=reg$regression.results[4,3],a=reg$regression.results[4,2], lty=2)
    text(0.07,0.3,substitute(paste("R"^{2}, "=0.24")), cex=1)
    mtext("T. amphioxeia                              ", side=2, cex=1.2,  line=3, font=3)
    mtext(substitute(paste("                 (10"^{6}, " cells L"^{-1},')')), side=2, cex=1.2,  line=3)
    mtext(substitute(paste("                 (10"^{6}, " cells L"^{-1},')')), side=1, cex=1.2,  line=3)
    mtext("M. major                      ", side=1, cex=1.2,  line=2.83, font=3)
    points(log(data[1,c(1,2)]), col=2)

dev.off()




#####################
### DIVISION RATES ###
#####################

### CULTURE

png("Figure4.png", width=114*2, height=114*2, pointsize=8, res=600, units="mm")

par(mfrow=c(3,1), mar=c(3,2,1,2), pty="m", cex=1.2, oma=c(1,3,1,3))

    plotCI(h.time,h.abun.mean,uiw=h.abun.sd, sfrac=0, lwd=2, col='darkgrey', xlim=c(min(cc$time, na.rm=T),max(cc$time, na.rm=T)), ylim=c(0,30), pch=NA, xaxt='n', xlab=NA, ylab=NA, yaxt='n')
    lines(h.time,h.abun.mean) 
    rect(min(night.lab$time), -1, max(night.lab$time), 500, density=NULL, col=adjustcolor("black", alpha=0.15), border=NA)
    rect(min(night2.lab$time), -1, max(night2.lab$time), 500, density=NULL, col=adjustcolor("black", alpha=0.15), border=NA)
    axis.POSIXct(1, at=seq(min(cc$time, na.rm=T), max(cc$time, na.rm=T), by=60*60*6),labels=seq(1,25, 6))
    axis(2, at=c(0,15,30), las=1)
    mtext("A", side=3, cex=2, line=0, adj=0)
    mtext(substitute(paste("abundance (10"^{6}, " cells L"^{-1},')')), side=2, cex=1.2, line=3)

    par(new=TRUE)
    plotCI(cc$time,cc$mean.f.G1*100, uiw=na.approx(cc$sd.f.G1*100), sfrac=0, lwd=2, col='red3', xlim=c(min(cc$time, na.rm=T),max(cc$time, na.rm=T)),, ylim=c(0,100), pch=NA, xaxt='n', xlab=NA, ylab=NA, yaxt='n')
    lines(cc$time,cc$mean.f.G1*100, col='red3')
    plotCI(cc$time,cc$mean.f.G2*100+cc$mean.f.S*100, uiw=na.approx(cc$sd.f.G2*100+cc$sd.f.S*100), sfrac=0, lwd=2, col='seagreen3', pch=NA,add=T)
    lines(cc$time,cc$mean.f.G2*100+cc$mean.f.S*100, col='seagreen3')
    # plotCI(cc$time,cc$mean.f.S*100, uiw=cc$sd.f.S*100, sfrac=0, lwd=2, col='darkturquoise', pch=NA,add=T)
    # lines(cc$time,cc$mean.f.S*100, col='darkturquoise')
    axis(4, at=c(0,50,100), las=1)
    mtext('Cells in G1 or S+G2 (%)', side=4, cex=1.2, line=2.5)


    plotCI(cc$time,cc$div, uiw=na.approx(cc$div.se), sfrac=0, lwd=2, col='red3', xlim=c(min(cc$time, na.rm=T),max(cc$time, na.rm=T)),, ylim=c(0,0.06), pch=NA, xaxt='n', xlab=NA, ylab=NA, yaxt='n')
    lines(cc$time,cc$div, col='red3')
    plotCI(m$time, m$div.ave,  uiw=m$div.se, col="darkgrey",sfrac=0, lwd=2, pch=NA, add=TRUE)
    lines(m$time, m$div.ave)
    rect(min(night.lab$time), -1, max(night.lab$time), 1, density=NULL, col=adjustcolor("black", alpha=0.15), border=NA)
    rect(min(night2.lab$time), -1, max(night2.lab$time), 1, density=NULL, col=adjustcolor("black", alpha=0.15), border=NA)
    axis.POSIXct(1, at=seq(min(cc$time, na.rm=T),max(cc$time, na.rm=T), by=60*60*6),labels=seq(1,25, 6))
    axis(2, at=c(0,0.03,0.06), las=1)
    mtext(substitute(paste("division (h"^{-1},")")), side=2, line=3, cex=1.2)
    mtext("B", side=3, cex=2, line=0, adj=0)
    mtext("time (h)", side=1, cex=1.2, line=2.5)

dev.off()




### CELL CYCLE / MODEL

png("FigureS5.png", width=114, height=114, pointsize=8, res=600, units="mm")

    par(pty='s')
    plotCI(data.cc[,1],data.cc[,2], uiw=data.cc[,3], xlim=c(0,0.06), ylim=c(0,0.06), xaxt='n', yaxt='n', err='x', sfrac=0, 
        col='darkgrey', lwd=2, pch=NA, xlab=NA, ylab=NA)
    plotCI(data.cc[,1],data.cc[,2], uiw=data.cc[,4], err='y', sfrac=0, col='darkgrey', lwd=2, pch=NA,add=TRUE)
    points(data.cc[,1],data.cc[,2], pch=16)
    abline(b=c$regression.results[4,3],a=c$regression.results[4,2], lty=2)
    axis(1, at=c(0,0.03,0.06))
    axis(2, at=c(0,0.03,0.06), las=1)
        text(0.0075, 0.05,substitute(paste("R"^{2}, "=0.60")), cex=1)
    mtext(substitute(paste("DNA-based division (h"^{-1},")")), side=1, line=3, cex=1.2)
    mtext(substitute(paste("Size-based division (h"^{-1},")")), side=2, line=3, cex=1.2)

dev.off()











### FIELD 

png("Figure5.png", width=114*2, height=114*2, pointsize=8, res=600, units="mm")

 par(mfrow=c(3,1), mar=c(2,2,1,2), pty="m", cex=1.2, oma=c(1,3,1,1))
  
    plotCI(fsc$time, fsc$vol, uiw=fsc$vol.sd, pch=NA, sfrac=0, col='darkgrey', yaxt='n', ylab=NA, xaxt='n', xlab=NA, lwd=1)
    abline(v=night.fsc$time, lwd=1, col=adjustcolor("black", alpha=0.15))
    lines(fsc$time, fsc$vol)
    axis(2, at=c(20,60,100), las=1)
    mtext(substitute(paste("Cell volume (", mu, "m"^{3},")")), 2, line=3 )
    axis(1, at=seq(min(fsc$time, na.rm=T), max(fsc$time, na.rm=T), by=60*60*24*6), labels=c(1,7,14,21))
    mtext("A", side=3, cex=2, line=0, adj=0)

    plotCI(data.dr$time, data.dr$DR, uiw=data.dr$DR.se, sfrac=0, xlab="", lwd=2, pch=16, ylab= "", col="darkgrey", las=1, yaxt='n', xaxt='n', xlim=range(fsc$time))
    lines(data.dr$time, data.dr$DR)
    axis(2, at=seq(0,2.5,0.5), las=1)
    axis(1, at=seq(min(data.dr$time), max(data.dr$time), by=60*60*24*6), labels=c(1,7,14,21))
    mtext(substitute(paste("division (d"^{-1},')')), side=2, cex=1.2, line=3)
    mtext("time (d)", side=1, cex=1.2, line=2)# par(new=T)
     mtext("B", side=3, cex=2, line=0, adj=0)

 dev.off()




###  DIVISION RATES


png("FigureS6.png", width=114*2, height=114*2, pointsize=8, res=600, units="mm")

par(mfrow=c(2,2), mar=c(3,2,2,3), pty="s", cex=1.2, oma=c(1,3,1,0))

    plot(data.dr[,c(3,9)], ylim=c(0,1.6), yaxt='n', xaxt='n', xlab=NA, ylab=NA, xlim=c(0,1.6))
    axis(2, at=c(0,0.8,1.6),las=1)
    axis(1, at=c(0, 0.8, 1.6))
    abline(b=P04.DR$regression.results[4,3],a=P04.DR$regression.results[4,2], lty=2)
    text(1.4,0.2,substitute(paste("R"^{2}, "=0.44")), cex=1)
    #text(10,0.6,"p < 0.01", cex=0.75)
    mtext(substitute(paste("division (d"^{-1},')')), side=2, cex=1.2, line=3)
    mtext(substitute(paste("DIP (",mu, "M)")),side=1, cex=1.2, line=2.5)
    mtext("A", side=3, cex=2, adj=0)

    plot(data.dr[,c(2,9)], ylim=c(0,1.6), yaxt='n', xaxt='n', xlab=NA, ylab=NA, xlim=c(5,35))
    axis(2, at=c(0,0.8,1.6),las=1)
    axis(1, at=c(5,20,35))
    abline(b=DIN.DR$regression.results[4,3],a=DIN.DR$regression.results[4,2], lty=2)
    text(30,0.2,substitute(paste("R"^{2}, "=0.30")), cex=1)
    #text(10,0.6,"p < 0.01", cex=0.75)
    mtext(substitute(paste("DIN (",mu, "M)")),side=1, cex=1.2, line=2.5)
    mtext("B", side=3, cex=2, adj=0)
  #  mtext(substitute(paste("division (d"^{-1},')')), side=2, cex=1.2, line=3)

    plot(data.dr[,c(6,9)],ylim=c(0,1.6), yaxt='n',xlim=c(7.8,8.4),yaxt='n', xaxt='n', xlab=NA, ylab=NA)
    axis(1, at=c(7.8,8.1,8.4))
    axis(2, at=c(0,0.8,1.6),las=1)
    mtext("pH",side=1, cex=1.2, line=2.5)
    mtext("C", side=3, cex=2, adj=0)
    abline(b=ph.DR$regression.results[4,3],a=ph.DR$regression.results[4,2], lty=2)
    text(8.3,1.4,substitute(paste("R"^{2}, "=0.41")), cex=1)
    mtext(substitute(paste("division (d"^{-1},')')), side=2, cex=1.2, line=3)


dev.off()

