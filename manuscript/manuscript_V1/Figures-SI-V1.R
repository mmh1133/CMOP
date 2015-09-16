library(plotrix)
library(zoo)
library(lmodel2)


user <- '/Users/mariaham/CMOP'
user <- '~/Documents/DATA/SeaFlow/CMOP/CMOP_git'
out.dir <- paste0(user, "/Rhodo_labExperiment/")

m <- read.csv(paste0(out.dir,"model_output-V2.csv"))
    m$time <- as.POSIXct(m$time, origin="1970-01-01", tz="" )
cc <- read.csv(paste0(out.dir,"RHODO_div-rate.csv"))[-1,]
    cc$time <- as.POSIXct(cc$time, origin="1970-01-01", tz="" )

i <- min(cc$time)
f <- max(cc$time)

stat <- read.delim(paste0(out.dir,"stat.tab"))
    stat$time <- as.POSIXct(stat$time,tz='')-8*60*60
    rhodo <- subset(stat, pop == 'crypto' & flag == 0) 

time.template <- seq(min(rhodo$time), max(rhodo$time), by=60*60)
time.res <- cut(rhodo$time,time.template)
h.abun.mean <-  na.approx(tapply(rhodo$abundance, time.res, function(x) mean(x, na.rm=T)))
h.abun.sd <-  na.approx(tapply(rhodo$abundance, time.res, function(x) sd(x, na.rm=T)))
h.fsc.mean <-  na.approx(tapply(rhodo$fsc_small, time.res, function(x) mean(x, na.rm=T)))
h.fsc.sd <-  na.approx(tapply(rhodo$fsc_small, time.res, function(x) sd(x, na.rm=T)))

#dark cycle: GMT 16:00-23:00 
night <- subset( rhodo , time >= as.POSIXct("2014-09-22 16:00:00")-8*60*60 &  time <= as.POSIXct("2014-09-22 23:00:00")-8*60*60)
night2 <- subset( rhodo , time >= as.POSIXct("2014-09-23 16:00:00")-8*60*60 &  time <= as.POSIXct("2014-09-23 23:00:00")-8*60*60)







png("FigureS2.png", width=114*1.5, height=114*1.5, pointsize=8, res=600, units="mm")

par(mfrow=c(3,1), mar=c(3,2,1,2), pty="m", cex=1.2, oma=c(1,3,1,3))
# plot(rhodo$time, rhodo$fsc_small, pch=NA)
# points(time.template,h.fsc.mean,col=2) 
# rect(min(night$time), -1, max(night$time), 500, density=NULL, col=adjustcolor("black", alpha=0.15), border=NA)
# rect(min(night2$time), -1, max(night2$time), 500, density=NULL, col=adjustcolor("black", alpha=0.15), border=NA)

plotCI(time.template[-1],h.abun.mean,uiw=h.abun.sd, sfrac=0, lwd=2, col='darkgrey', xlim=c(i,f), ylim=c(0,30), pch=NA, xaxt='n', xlab=NA, ylab=NA, yaxt='n')
lines(time.template[-1],h.abun.mean) 
rect(min(night$time), -1, max(night$time), 500, density=NULL, col=adjustcolor("black", alpha=0.15), border=NA)
rect(min(night2$time), -1, max(night2$time), 500, density=NULL, col=adjustcolor("black", alpha=0.15), border=NA)
axis.POSIXct(1, at=seq(i, f, by=60*60*6),labels=seq(1,25, 6))
axis(2, at=c(0,15,30), las=1)
mtext("A", side=3, cex=2, line=0, adj=0)
mtext(substitute(paste("abundance (10"^{6}, " cells L"^{-1},')')), side=2, cex=1.2, line=3)

par(new=TRUE)
plotCI(cc$time,cc$mean.f.G1*100, uiw=na.approx(cc$sd.f.G1*100), sfrac=0, lwd=2, col='red3', xlim=c(i,f), ylim=c(0,100), pch=NA, xaxt='n', xlab=NA, ylab=NA, yaxt='n')
lines(cc$time,cc$mean.f.G1*100, col='red3')
plotCI(cc$time,cc$mean.f.G2*100+cc$mean.f.S*100, uiw=na.approx(cc$sd.f.G2*100+cc$sd.f.S*100), sfrac=0, lwd=2, col='seagreen3', pch=NA,add=T)
lines(cc$time,cc$mean.f.G2*100+cc$mean.f.S*100, col='seagreen3')
# plotCI(cc$time,cc$mean.f.S*100, uiw=cc$sd.f.S*100, sfrac=0, lwd=2, col='darkturquoise', pch=NA,add=T)
# lines(cc$time,cc$mean.f.S*100, col='darkturquoise')
axis(4, at=c(0,50,100), las=1)
mtext('Cells in G1 or S+G2 (%)', side=4, cex=1.2, line=2.5)


plotCI(cc$time,cc$div, uiw=na.approx(cc$div.se), sfrac=0, lwd=2, col='red3', xlim=c(i,f), ylim=c(0,0.06), pch=NA, xaxt='n', xlab=NA, ylab=NA, yaxt='n')
lines(cc$time,cc$div, col='red3')
plotCI(m$time, m$div.ave,  uiw=m$div.se, col="darkgrey",sfrac=0, lwd=2, pch=NA, add=TRUE)
lines(m$time, m$div.ave)
rect(min(night$time), -1, max(night$time), 1, density=NULL, col=adjustcolor("black", alpha=0.15), border=NA)
rect(min(night2$time), -1, max(night2$time), 1, density=NULL, col=adjustcolor("black", alpha=0.15), border=NA)
axis.POSIXct(1, at=seq(i, f, by=60*60*6),labels=seq(1,25, 6))
axis(2, at=c(0,0.03,0.06), las=1)
mtext(substitute(paste("division (h"^{-1},")")), side=2, line=3, cex=1.2)
mtext("B", side=3, cex=2, line=0, adj=0)
mtext("time (h)", side=1, cex=1.2, line=2.5)

dev.off()




png("FigureS3.png", width=114, height=114, pointsize=8, res=600, units="mm")


id <- findInterval(cc$time,m$time)
data <- data.frame(cbind(CC=cc$div, DR=m$div.ave[id], CC.sd=na.approx(cc$div.se), DR.sd=na.approx(m$div.se[id])))
c <- lmodel2(CC ~ DR, data,"relative", "relative", 99)

par(pty='s')
plotCI(data[,1],data[,2], uiw=data[,3], xlim=c(0,0.06), ylim=c(0,0.06), xaxt='n', yaxt='n', err='x', sfrac=0, 
    col='darkgrey', lwd=2, pch=NA, xlab=NA, ylab=NA)
plotCI(data[,1],data[,2], uiw=data[,4], err='y', sfrac=0, col='darkgrey', lwd=2, pch=NA,add=TRUE)
points(data[,1],data[,2], pch=16)
abline(b=c$regression.results[4,3],a=c$regression.results[4,2], lty=2)
axis(1, at=c(0,0.03,0.06))
axis(2, at=c(0,0.03,0.06), las=1)
    text(0.0075, 0.05,substitute(paste("R"^{2}, "=0.60")), cex=1)
mtext(substitute(paste("DNA-based division (h"^{-1},")")), side=1, line=3, cex=1.2)
mtext(substitute(paste("Size-based division (h"^{-1},")")), side=2, line=3, cex=1.2)

dev.off()





