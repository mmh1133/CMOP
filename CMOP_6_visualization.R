library(plotrix)
library(scatterplot3d)
jet.colors <- colorRampPalette(c("blue4","royalblue4","deepskyblue3", "seagreen3", "yellow", "orangered2","darkred")) # Banas



all.data <- read.csv("~/Documents/DATA/SeaFlow/CMOP/CMOP_6/CMOP_6_all.csv")
name <- data.frame(title=c("Large Debris","Crytophytes 1", "Cryptophytes 2","PicoEuk","Synechococcus"))
rownames(name) <- c("hetero","crypto1","crypto2","other","synecho")
all.data$time <- as.POSIXct(all.data[,"time"], tz="GMT")

cytdiv <- read.csv("/Volumes/ribalet/diversity_indices/CMOP_6_diversity_indices.csv")
cytdiv$time <- as.POSIXct(cytdiv[,"time"], tz="GMT")

phyto <- 'crypto2'
start1 <- as.POSIXct("2013-09-10") ### 1st week
end1 <- as.POSIXct("2013-09-16")
p1 <- subset(all.data, pop == phyto  & flag==0 & time > start1 & time < end1)

start2 <- as.POSIXct("2013-09-17") ### 2nd week
end2 <- as.POSIXct("2013-09-19")
p2 <- subset(all.data, pop == phyto  & flag==0 & time > start2 & time < end2)

start3 <- as.POSIXct("2013-09-22") ### 3rd week
end3 <- as.POSIXct("2013-09-28")
p3 <- subset(all.data, pop == phyto  & flag==0 & time > start3 & time < end3)

start4 <- as.POSIXct("2013-09-29") ### 4th week
end4 <- as.POSIXct("2013-10-03")
p4 <- subset(all.data, pop == phyto  & flag==0 & time > start4 & time < end4)

start <- as.POSIXct("2013-09-10") ### ALL
end <- as.POSIXct("2013-10-03")
p.all <- subset(all.data, pop == phyto  & flag==0 & time > start & time < end)




size <- 1.2	
p <- p.all
xlim.t <- range(p$time)
par(mfrow=c(4,1), cex=size, mar=c(2,2,2,2), oma=c(2,1,1,1), pty='m', las=1)

	plot(all.data$time, all.data$fluo, col='black', pch=16, main="bulk RED fluorescence", ylab=NA)#,ylab=substitute(paste("Chl a (",mu,"g L"^{-1},")")), )
	abline(v=xlim.t, lwd=3, col='red')

	plot(all.data$time, all.data$pe, col='black', pch=16, main="bulk ORANGE fluorescence", ylab=NA,ylim=c(0,1))#,ylab=substitute(paste("Phycoerythrin a (",mu,"g L"^{-1},")")))
	abline(v=xlim.t, lwd=3, col='red')























par(mfrow=c(ceiling(nrow(name)+1),1), cex=size, mar=c(0.5,4.5,1,4), oma=c(2,1,1,1), pty='m', las=1)


	# plot(all.data$time, all.data$fluo, col=jet.colors(100)[cut(all.data$sal,100)], pch=16, ylab=substitute(paste("Chl a (",mu,"g L"^{-1},")")), main="bulk RED fluorescence", xaxt='n')
	# axis(1,labels=F)
	# ylim <- par('usr')[c(3,4)]
	# xlim <- par('usr')[c(1,2)]
	# color.legend(xlim[2] , ylim[1], xlim[2] + diff(xlim)/100, ylim[2], legend=pretty(range(all.data$sal, na.rm=T)), rect.col=jet.colors(100), gradient='y',align='rb',cex=size)
	#abline(v=xlim.t, lwd=3, lty=2)
	
	# p <- subset(all.data, time > xlim.t[1] & time < xlim.t[2] & fluo < 10)
	# plot(p$time, p$fluo, col=jet.colors(100)[cut(p$sal,100)],pch=16, ylab=substitute(paste("Chl a (",mu,"g L"^{-1},")")), main="bulk RED fluorescence", xaxt='n')
	# ylim <- par('usr')[c(3,4)]
	# xlim <- par('usr')[c(1,2)]
	# color.legend(xlim[2] , ylim[1], xlim[2] + diff(xlim)/100, ylim[2], legend=pretty(range(p$sal, na.rm=T)), rect.col=jet.colors(100), gradient='y',align='rb',cex=size)
	# axis(1,labels=F)
	# mtext("Salinity (psu)", 4, line=2, cex=size, las=0)
	
for(i in c("crypto1","crypto2")){ 
	# p <- subset(all.data, pop == i & flag==0)
	# para <- p$sal
	# print(i)
	# plot(p$time,p$conc, pch=16,cex=size, type='p',main=paste(names[i,'title']),col=jet.colors(100)[cut(para,100)], ylab=substitute(paste("Cell conc (10"^{6},"cells L"^{-1},")")), xaxt='n')
	# ylim <- par('usr')[c(3,4)]
	# xlim <- par('usr')[c(1,2)]
	# color.legend(xlim[2] , ylim[1], xlim[2] + diff(xlim)/100, ylim[2], legend=pretty(range(para, na.rm=T)), rect.col=jet.colors(100), gradient='y',align='rb',cex=size)
	# abline(v=xlim.t, lwd=3, lty=2)
	# if(i =="synecho") axis.POSIXct(1, p$time, format="%b %d", labels=T)
	# else axis(1,labels=F)



	p <- subset(all.data, pop == i  & flag==0 & time > start & time < end)
	print(nrow(p))
	xlim.t <- range(p$time)
para <- p$sal
	plot(p$time, p$conc, pch=16,cex=size, type='p',main=paste(name[i,'title']),col=jet.colors(100)[cut(para,100)], ylab=substitute(paste("Cell conc (10"^{6},"cells L"^{-1},")")), xlim=xlim.t, xaxt='n')
	ylim <- par('usr')[c(3,4)]
	xlim <- par('usr')[c(1,2)]
	color.legend(xlim[2] , ylim[1], xlim[2] + diff(xlim)/100, ylim[2], legend=pretty(range(para, na.rm=T)), rect.col=jet.colors(100), gradient='y',align='rb',cex=size)
	#if(i =="synecho") axis.POSIXct(1, p$time, format="%a %H:%M", labels=T)
	if(i =="synecho") axis.POSIXct(1, p$time, format="%a %d", labels=T)

	else axis.POSIXct(1, p$time, format="%a %H:%M", labels=F)
	mtext("Salinity (psu)", 4, line=2, cex=size, las=0)

	
}


par(mfrow=c(4,1), cex=size, mar=c(3,5,2,2), oma=c(2,1,1,1), pty='m', las=1)
p <- subset(all.data, time > xlim.t[1] & time < xlim.t[2] & fluo < 10)
plot(p$time, p$fluo, pch=16, main="Fluorimeter",ylab="bulk RED fluo", xlim=xlim.t, ylim=c(2,6),log='y')
# p <- subset(all.data , pop == "hetero")
# plot(p$time, p$bulk_red, col='green',pch=16, xlim=xlim.t, ylim=c(150,200))
#par(new=T)
# plot(cytdiv$time, cytdiv$totRED, xlim=xlim.t, pch=16, xaxt='n', ylab=NA, yaxt='n',col='red')
#axis(4, col='red')

p <- subset(all.data , time > xlim.t[1] & time < xlim.t[2] & pop == "other" & flag==0)
plot(p$time, 10^-6*p$n*p$chl_small_mean/(p$opp/p$evt), col='seagreen3',pch=16, xlim=xlim.t, ylim=c(0.3,15),log='y', ylab="Cumulated RED fluo \n from each particle", main="SeaFlow")
p <- subset(all.data , time > xlim.t[1] & time < xlim.t[2] & pop == "hetero" & flag==0)
points(p$time, 10^-6*p$n*p$chl_small_mean/(p$opp/p$evt), col='red3',pch=16)
legend("left",c("Phytoplankton", "Non-Phytoplankton"), bty='n', pch=16, col=c('seagreen3', 'red3'))



p <- subset(all.data ,  pop == "other" & flag==0)
points(p$time, 10^-6*p$n*p$chl_small_mean/(p$opp/p$evt), col='blue', pch=16)
p <- subset(all.data , pop == "crypto1" & flag==0)
points(p$time, 10^-6*p$n*p$chl_small_mean/(p$opp/p$evt), col='orange', pch=16)
p <- subset(all.data , pop == "crypto2" & flag==0)
points(p$time, 10^-6*p$n*p$chl_small_mean/(p$opp/p$evt), col='orange3', pch=16)







	
	
par(mfrow=c(3,2), cex=size, oma=c(1,1,1,1),pty='m', las=1)
angle <- 45

	p <- all.data
	scatterplot3d(y=p$temp, z=p$sal, x=p$time, pch=16, color=jet.colors(100)[cut(log10(p$fluo),100)], main="bulk RED fluorescence", zlab=NA, xlab=NA, ylab=NA, type='p',angle=angle , x.ticklabs=format(pretty(range(p$time)),format="%b %d"), scale.y=0.5,mar=c(2,2,1,2))

for(i in pop){ 
if(i  == "beads") next
	p <- subset(all.data, pop == i & flag==0)
	print(i)
	para <- log(smooth.spline(p$time, p$conc, spar=0.99, all.knots=T)$y)
	# plot(p$sal, p$temp, pch=16, col=jet.colors(100)[cut(para,100)], main=paste(names[i,'title']), ylab="Temperature (deg C)", xlab="Salinity (psu)")
	# ylim <- par('usr')[c(3,4)]
	# xlim <- par('usr')[c(1,2)]
	# color.legend(xlim[2] , ylim[1], xlim[2] + diff(xlim)/40, ylim[2], legend=pretty(range(para, na.rm=T)), rect.col=jet.colors(100), gradient='y',align='rb',cex=size)
	# mtext(substitute(paste("LOG cell conc (10"^{6},"cells L"^{-1},")")), 4, cex=size, line=2, las=0)
	scatterplot3d(y=p$temp, z=p$sal, x=p$time, pch=16, color=jet.colors(100)[cut(log10(p$conc),100)], main=paste(names[i,'title']), zlab=NA, xlab=NA, ylab=NA, type='p',angle=angle , x.ticklabs=format(pretty(range(p$time)),format="%b %d"), scale.y=0.5, mar=c(2,2,1,2))
}



