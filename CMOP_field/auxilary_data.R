library(flowPhyto)

cruise <- 'CMOP_6'
df <- read.delim(paste("/Volumes/seaflow/",cruise,"/", "stats.tab",sep=""))
df$time <- as.POSIXct(df[,"time"], tz="GMT")

stat <- subset(df, flag==0)


lim.t <- c(min(as.POSIXct(stat$time),na.rm=T), max(as.POSIXct(stat$time),na.rm=T))

pop <- unique(df$pop)
phyto <- pop[!(pop=="beads")]

pop.def <- readPopDef(paste("/Volumes/seaflow/",cruise,"/","/pop.def.tab", sep=""))
pop <- pop.def$abrev
cex <- 1
par(mfrow=c(ceiling(length(phyto)),1), cex=cex, mar=c(2,4,2,3), oma=c(2,1,1,1))

n <- 1
for(i in pop){ 
if(i  == "beads") next
	p <- subset(df, pop == i)
	print(i)
	print(nrow(p))
	if(nrow(p) > 0){
		plot(p$time, p$conc, xlim=lim.t,xlab=NA,ylab=NA, main=paste(pop.def[i,"title"]),las=1, log='y')
		mtext(substitute(paste("Cell density (10"^{6},"cells L"^{-1},")")), 2, line=3, cex=cex)
		if(nrow(p) < 10) mtext(paste("only ", nrow(p), "data points"), side=1, line=-4)
		if(n == length(phyto)) mtext("Time", side=1, line=3,cex=cex)
		n <- n + 1
	}
}

par(mfrow=c(10,1))

path <- "~/Documents/DATA/SeaFlow/CMOP/CMOP_6/SATURN_03_prelimDATA/"

breaks <- seq(min(df$time,na.rm=T), max(df$time,na.rm=T), by=3*60)

fluo1 <- read.csv(paste(path, "saturn03.240.A.AlgaeWatch_2013_09_PD1.csv",sep=""))
fluo2 <- read.csv(paste(path, "saturn03.240.A.AlgaeWatch_2013_10_PD1.csv",sep=""))
fluo <- rbind(fluo1,fluo2)
fluo <- subset(fluo, fluorescence < 20 & fluorescence > 0.5)
fluo[,1] <- as.POSIXct(fluo[,1], format="%Y/%m/%d %H:%M:%S", tz=""); plot(fluo, xlim=range(p$time), type='p')
time <- cut(fluo[,1], breaks=breaks)
fluo <- tapply(fluo[,2], time,function(x) mean(x,na.rm=T))
fluo <- data.frame(fluo)


fv1 <- read.csv(paste(path, "saturn03.240.A.Phytoflash_2013_09_PD1.csv",sep=""))
fv2 <- read.csv(paste(path, "saturn03.240.A.Phytoflash_2013_10_PD1.csv",sep=""))
fv <- rbind(fv1,fv2)
fv[,1] <- as.POSIXct(fv[,1], format="%Y/%m/%d %H:%M:%S", tz=""); plot(fv[,c(1,2)], xlim=range(p$time), type='p')
time <- cut(fv[,1], breaks=breaks)
fv <- tapply(fv[,2], time,function(x) mean(x,na.rm=T))
fv <- data.frame(fv)

pe1 <- read.csv(paste(path, "saturn03.240.A.CyanoWatch_2013_09_PD1.csv",sep=""))
pe2 <- read.csv(paste(path, "saturn03.240.A.CyanoWatch_2013_10_PD1.csv",sep=""))
pe <- rbind(pe1,pe2)
pe[,1] <- as.POSIXct(pe[,1], format="%Y/%m/%d %H:%M:%S", tz="")
pe <- subset(pe, phycoeryth < 200 & phycoeryth > 0); plot(pe, xlim=range(p$time), type='p')
time <- cut(pe[,1], breaks=breaks)
pe <- tapply(pe[,2], time,function(x) mean(x,na.rm=T))
pe <- data.frame(pe)

oxy1 <- read.csv(paste(path, "saturn03.240.A.Oxygen_2013_09_PD1.csv",sep=""))
oxy2 <- read.csv(paste(path, "saturn03.240.A.Oxygen_2013_10_PD1.csv",sep=""))
oxy <- rbind(oxy1,oxy2)
oxy[,1] <- as.POSIXct(oxy[,1], format="%Y/%m/%d %H:%M:%S", tz=""); plot(oxy[,c(1,3)], xlim=range(p$time), type='p')
time <- cut(oxy[,1], breaks=breaks)
oxy <- tapply(oxy[,2], time,function(x) mean(x,na.rm=T))
oxy <- data.frame(oxy)

nit1 <- read.csv(paste(path, "saturn03.240.A.ISUS_2013_09_PD1.csv",sep=""))
nit2 <- read.csv(paste(path, "saturn03.240.A.ISUS_2013_10_PD1.csv",sep=""))
nit <- rbind(nit1,nit2)
nit[,1] <- as.POSIXct(nit[,1], format="%Y/%m/%d %H:%M:%S", tz="")
nit <- subset(nit, nitrate < 30); plot(nit, xlim=range(p$time), type='p')
time <- cut(nit[,1], breaks=breaks)
nit <- tapply(nit[,2], time,function(x) mean(x,na.rm=T))
nit <- data.frame(nit)

ph1 <- read.csv(paste(path, "saturn03.240.A.pH_2013_09_PD1.csv",sep=""))
ph2 <- read.csv(paste(path, "saturn03.240.A.pH_2013_10_PD1.csv",sep=""))
ph <- rbind(ph1,ph2)
ph[,1] <- as.POSIXct(ph[,1], format="%Y/%m/%d %H:%M:%S", tz="")
ph <- subset(ph, ph > 7.5 & ph < 8.5); plot(ph[,c(1,2)], xlim=range(p$time), type='p')
time <- cut(ph[,1], breaks=breaks)
ph <- tapply(ph[,2], time,function(x) mean(x,na.rm=T))
ph <- data.frame(ph)

cdom1 <- read.csv(paste(path, "saturn03.240.A.CDOM_Fluorometer_2013_09_PD1.csv",sep=""))
cdom2 <- read.csv(paste(path, "saturn03.240.A.CDOM_Fluorometer_2013_10_PD1.csv",sep=""))
cdom <- rbind(cdom1,cdom2)
cdom[,1] <- as.POSIXct(cdom[,1], format="%Y/%m/%d %H:%M:%S", tz="")
cdom <- subset(cdom, cdom < 10^3);plot(cdom, xlim=range(p$time), type='p')
time <- cut(cdom[,1], breaks=breaks)
cdom <- tapply(cdom[,2], time,function(x) mean(x,na.rm=T))
cdom <- data.frame(cdom)

tur1 <- read.csv(paste(path, "saturn03.240.A.Turbidity_2013_09_PD1.csv",sep=""))
tur2 <- read.csv(paste(path, "saturn03.240.A.Turbidity_2013_10_PD1.csv",sep=""))
tur <- rbind(tur1,tur2)
tur[,1] <- as.POSIXct(tur[,1], format="%Y/%m/%d %H:%M:%S", tz="")
tur <- subset(tur, turbidity < 5 & turbidity > 1.5);plot(tur, xlim=range(p$time), type='p')
time <- cut(tur[,1], breaks=breaks)
tur <- tapply(tur[,2], time,function(x) mean(x,na.rm=T))
tur <- data.frame(tur)

ct1 <- read.csv(paste(path, "saturn03.240.A.CT_2013_09_PD1.csv",sep=""))
ct2 <- read.csv(paste(path, "saturn03.240.A.CT_2013_10_PD1.csv",sep=""))
ct <- rbind(ct1,ct2)
ct[,1] <- as.POSIXct(ct[,1], format="%Y/%m/%d %H:%M:%S", tz="");plot(ct[,c(1,4)], xlim=range(p$time), type='p');plot(ct[,c(1,2)], xlim=range(p$time), type='p')
time <- cut(ct[,1], breaks=breaks)
sal <- tapply(ct[,2], time,function(x) mean(x,na.rm=T))
sal <- data.frame(sal)
cond <- tapply(ct[,3], time,function(x) mean(x,na.rm=T))
cond <- data.frame(cond)
temp <- tapply(ct[,4], time,function(x) mean(x,na.rm=T))
temp <- data.frame(temp)



data <- cbind(time=row.names(fluo), fluo, fv, pe, oxy, nit, ph, cdom, tur, sal, temp, cond)

data[,1] <- as.POSIXct(data[,1],format="%Y-%m-%d %H:%M:%S", tz='GMT')
data[,1] <- as.POSIXct(format(data[,1],format="%Y-%m-%d %H:%M"), tz='GMT')
df$time <- as.POSIXct(format(df$time ,format="%Y-%m-%d %H:%M"), tz='GMT')

id <- findInterval(df$time, data$time) ## SEAFLOW DATA RESOLUTION
all.data1 <- cbind( df, data[id,])
all.data2 <- merge(data, df, by="time", all=T) ## ALL SENSOR DATA RESOLUTION

id <- which(is.na(match(all.data2$time,all.data1$time))) ## MERGE THE TWO DATASET RESOLUTON
all.data <- rbind(all.data1[,-c(55)],all.data2[id,])

write.csv(all.data, "~/Documents/DATA/SeaFlow/CMOP/CMOP_6/CMOP_6_all.csv", quote=FALSE, row.names=FALSE)


length(which(!is.na(match(data[,1],df$time))))

par(mfrow=c(12,1))
all.data[,1] <- as.POSIXct(all.data[,1],format="%Y-%m-%d %H:%M", tz='GMT')
p <- subset(all.data, pop == "other" & flag==0)
plot(p$time, p$conc, log='y', type='p')
for(i in 2:12) plot(all.data[,1], all.data[,i], type='p', ylab=paste(colnames(all.data[,])[i]))

