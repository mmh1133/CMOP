library(rgl)
library(zoo)
library(plotrix)
library(popcycle)
jet.colors <- colorRampPalette(c("blue4","royalblue4","deepskyblue3", "seagreen3", "yellow", "orangered2","darkred")) # Banas


#### PARAMETERS
cat <- 64#2^6
cruise <- 'Thompson_4'
phyto <- 'prochloro'



## MODEL
all.filelist <- list.files(paste("/Volumes/ribalet/Cell_Division/",cruise,sep=""),pattern=paste(phyto,"_modelHD_growth_",cruise,"_Ncat",cat,sep=""))
filelist <- all.filelist[grep(pattern=paste(phyto), all.filelist)]

filelist <- filelist[-c(1,2,4,6,7,8,13,14,15,18,19,20,21,22)]

n <- c <- 1
Conc.all <- N.proj.all <- V.hist.all <- div.rate <- para.all <- Col <- NULL
for(file in filelist){
	#file <- filelist[11]
	load(paste("/Volumes/ribalet/Cell_Division/",cruise,"/",file, sep=""))
	print(file)
	print(n)
		dim <- conc.proj.all <- n.proj.all <- v.hist.all <- dr.all <- p.all <- NULL
			for(i in 2:dim(model)[2]){
				n.proj <- model[4,i][[1]]
				n.proj.all <- cbind(n.proj.all, n.proj)			
							
				conc.proj <- cbind(as.numeric(colnames(n.proj)), as.numeric(colSums(n.proj)))
				conc.proj.all <- rbind(conc.proj.all, conc.proj)
				
				dr <- model[2,i][[1]]
				h.dr <- cbind(as.numeric(colnames(dr)), as.numeric(dr))
				dr.all <- rbind(dr.all, h.dr)
				
				v.proj <- model[3,i][[1]]	
				v.hist.all <- cbind(v.hist.all, v.proj)			

				para <- model[1,i][[1]]
				param <- cbind(time=as.numeric(colnames(n.proj)), para)
				p.all <- rbind(p.all, param)
			}
		
	
		div.rate <- rbind(div.rate, dr.all)
		N.proj.all <- cbind(N.proj.all, n.proj.all)
		Conc.all <- rbind(Conc.all, conc.proj.all)
		V.hist.all <- cbind(V.hist.all, v.hist.all)
		para.all <- rbind(para.all, p.all)

		col <- rep(c, nrow(dr.all))
		Col <- c(Col,col)

		leg <- unlist(list(strsplit(filelist,"_t")))[seq(2,length(filelist[1:n])*2,2)]

		layout(matrix(c(1,1,2:7),4,2, byrow=T)) 
		par(pty='m')	
		plot(div.rate, ylab="Div Rate", xlab="time",col=Col)
			#abline(v=night$UNIXtime,col='lightgrey');points(div.rate,col=Col)
			legend("topleft",legend=leg, col=1:c, ncol=length(leg), pch=1)
		plot(para.all[,"time"], para.all[,"gmax"], ylab="gmax", xlab="time",col = Col)
		plot(para.all[,"time"], para.all[,"dmax"],ylab="dmax", xlab="time",col = Col)
		plot(para.all[,"time"], para.all[,"a"],ylab="a", xlab="time",col = Col)
		plot(para.all[,"time"], para.all[,"b"],ylab="b", xlab="time",col = Col)
		plot(para.all[,"time"], para.all[,"E_star"],ylab="E_star", xlab="time",col = Col)
		plot(para.all[,"time"], para.all[,"resnorm"],ylab="resnorm", xlab="time",col = Col)

		# # 
		# names(para) <- c("gmax","a","b","E_star","dmax","resnorm")
		# par(mfrow=c(4,2))
		# barplot(d.GR, col='grey', main="GR")
		# for(i in 1:6) barplot(para[,i], main=colnames(para)[i])
n <- n + 1	
c <- c + 1
}



	Div.rate <- div.rate[order(div.rate[,1]),]
	Nproj <- N.proj.all[,order(as.numeric(colnames(N.proj.all)))]
	Conc.proj <- Conc.all[order(Conc.all[,1]),]
	Vproj <- V.hist.all[,order(as.numeric(colnames(V.hist.all)))]
	Para.all <- para.all[order(para.all[,"time"]),]
		
	para <- Vproj; percentile <- cut(unlist(para), 100); plot3d(rep(1:dim(para)[1], dim(para)[2]), rep(1:dim(para)[2], each=dim(para)[1]), z=matrix(para), col=jet.colors(100)[percentile], type='l', lwd=3, xlab="size class", ylab="time", zlab="Frequency")



	

###############
### BINNING ###
###############

		breaks <- seq(as.numeric(colnames(Vproj)[1]),as.numeric(colnames(Vproj)[dim(Vproj)[2]]),by=60*60)

	
	# DIVISION 
		h <- cut(Div.rate[,1], breaks=breaks, labels=F)
		h.time.numc <- as.vector(tapply(Div.rate[,1], h, function(x) mean(x, na.rm=T)))#; h.time.numc <- na.approx(h.time.numc, na.rm=F)
		h.dr.mean <- as.vector(tapply(Div.rate[,2], h, function(x) mean(x, na.rm=T)))#; h.dr.mean <- na.approx(h.dr.mean, na.rm=F)
		h.dr.sd <- as.vector(tapply(Div.rate[,2], h, function(x) sd(x, na.rm=T)))#; h.dr.sd <- na.approx(h.dr.sd, na.rm=F)
		#h.time <- as.POSIXct(h.time.numc,origin="1970-01-01",tz='GMT')
		h.time <- as.POSIXct(breaks[findInterval(h.time.numc, breaks)],origin="1970-01-01",tz='GMT')

		id <- findInterval(h.time, pop$time, rightmost.closed=F)
		h.lat <- pop[id,"lat"]
		h.lon<- pop[id,"long"]
		
		D <- data.frame(cbind(h.time, h.lat, h.lon, h.dr.mean, h.dr.sd))
		   
			par(mfrow=c(2,1))
		    plot(h.time, h.dr.mean, ylim=c(0,max(h.dr.mean, na.rm=T)*1.3))		
			abline(v=night$UNIXtime,col='lightgrey')
		    plotCI(h.time, h.dr.mean, h.dr.sd, add=T)		


					
	# SEAFLOW
		# stat <- read.delim(paste("/Volumes/seaflow/",cruise,"/stats.tab",sep="")) ## OLD VERSION
		stat <- get.stat.table("/Volumes/seaflow/CMOP_6/CMOP_2013_f2/sqlite/popcycle.db")
		pop <- subset(stat, pop == phyto)
		pop$time <- as.POSIXct(pop$time, format="%FT%T",tz='GMT')
		pop$UNIXtime <- as.numeric(pop$time)	

		h2 <- cut(pop[,"UNIXtime"], breaks=breaks, labels=F)
		h2.time.numc <- as.vector(tapply(pop[,"UNIXtime"], h2, function(x) mean(x, na.rm=T)))
		h2.time <- as.POSIXct(breaks[findInterval(h2.time.numc, breaks)],origin="1970-01-01",tz='GMT')
		h2.conc.mean <- as.vector(tapply(pop[,"abundance"], h2, function(x) mean(x, na.rm=T)))
		h2.conc.sd <- as.vector(tapply(pop[,"abundance"], h2, sd))
		h2.fsc.mean <- as.vector(tapply(pop[,"fsc_small"], h2, function(x) mean(x, na.rm=T)))
		h2.fsc.sd <- as.vector(tapply(pop[,"fsc_small"], h2, function(x) sd(x, na.rm=T)))
		h2.chl.mean <- as.vector(tapply(pop[,"chl_small"], h2, function(x) mean(x, na.rm=T)))
		h2.chl.sd <- as.vector(tapply(pop[,"chl_small"], h2, function(x) sd(x, na.rm=T)))
		
		S <- data.frame(cbind(h2.time, h2.conc.mean, h2.conc.sd, h2.fsc.mean, h2.fsc.sd, h2.chl.mean, h2.chl.sd))



	par(mfrow=c(4,2))
		    plotCI(h2.time, h2.conc.mean, h2.conc.sd); lines(h2.time, h2.conc.mean,lwd=2,col=2)
		    plotCI(h2.time, h2.fsc.mean, h2.fsc.sd)	
		    plotCI(h2.time, h2.chl.mean,h2.chl.sd)
	

	## PAR
	Par.path <- paste("/Volumes/ribalet/Cell_Division/",cruise,"/Par_",cruise,sep="")
	Par <- read.csv(Par.path, sep=",")
	Par$time <- as.POSIXct(Par$time, tz='GMT')
		h4 <- cut(as.numeric(Par$time), breaks=breaks, labels=F)
		h4.time.numc <- as.vector(tapply(as.numeric(Par$time), h4, mean))
		h4.par.mean <- as.vector(tapply(Par[,"par"], h4, mean))
		h4.par.sd <- as.vector(tapply(Par[,"par"], h4, sd))
		h4.time <- as.POSIXct(breaks[findInterval(h4.time.numc, breaks)],origin="1970-01-01",tz='GMT')
		

		L <- data.frame(cbind(h4.time, h4.par.mean, h4.par.sd))

		    plotCI(h4.time, h4.par.mean, h4.par.sd)		
	
	
	DS <- merge(D, S, by.x=c("h.time"), by.y= c("h2.time"),all=T)
	DSL <- merge(DS, L, by.x=c("h.time"), by.y= c("h4.time"),all=T)

	





########################
### CHECK DIEL CYCLE ###
########################


par(mfrow=c(4,1))
	for(p in c("h.dr","h2.conc","h2.fsc","h2.chl")){
		plotCI(DSL$h.time, DSL[,paste(p,'.mean',sep="")], DSL[,paste(p,'.sd',sep="")], col=NA, ylab=NA, main=paste(p))
		abline(v=night$UNIXtime,col='lightgrey')
		plotCI(DSL$h.time, DSL[,paste(p,'.mean',sep="")], DSL[,paste(p,'.sd',sep="")],add=T)
	}




####################################
### Net Growth Rate - Loss rates ###   
####################################
spar <- 0.5

par(mfrow=c(4,1),pty='m')

na <- which(is.na(DSL[,"h2.conc.sd"]))
DSL[na,"h2.conc.sd"] <- 0

na <- which(is.na(DSL[,"h2.conc.mean"]))
plot(DSL[-na,"h.time"], DSL[-na,"h2.conc.mean"],pch=NA, main="h2.conc.mean")
abline(v=night$UNIXtime,col='lightgrey')
s.conc <- smooth.spline(DSL[-na,"h.time"], DSL[-na,"h2.conc.mean"], all.knots=T, spar=spar)
plotCI(DSL[-na,"h.time"], DSL[-na,"h2.conc.mean"],DSL[-na,"h2.conc.sd"],add=T)
points(DSL[-na,"h.time"], s.conc$y,col='green',type='o')
gr <- (60*60*diff(s.conc$y)/diff(s.conc$x))
DSL[-na,"h2.concLPF.mean"] <- s.conc$y
DSL[-na,'h2.gr.mean'] <- c(NA,gr)
DSL[-na,'h2.gr.sd'] <- abs(DSL[-na,'h2.gr.mean']*DSL[-na,'h2.conc.sd']/s.conc$y)
DSL[-na,"h2.dr.mean"] <- DSL[-na,'h.dr.mean']*s.conc$y
DSL[-na,"h2.dr.sd"] <- abs(DSL[-na,"h2.dr.mean"] * sqrt((DSL[-na,'h.dr.sd']/DSL[-na,'h.dr.mean'])^2 + (DSL[-na,'h2.conc.sd']/s.conc$y)^2))
DSL[-na,"h2.lr.mean"] <- DSL[-na,"h2.dr.mean"] - DSL[-na,"h2.gr.mean"]
DSL[-na,"h2.lr.sd"] <- abs(DSL[-na,"h2.lr.mean"] * sqrt((DSL[-na,'h.dr.sd']/DSL[-na,'h.dr.mean'])^2 + (DSL[-na,'h2.gr.sd']/DSL[-na,'h2.gr.mean'])^2))
DSL[-na,"h.gr.mean"] <- DSL[-na,'h2.gr.mean']/s.conc$y
DSL[-na,"h.gr.sd"] <- abs(DSL[-na,'h2.gr.sd']/s.conc$y)
DSL[-na,"h.lr.mean"] <- DSL[-na,'h2.lr.mean']/s.conc$y
DSL[-na,"h.lr.sd"] <- abs(DSL[-na,'h2.lr.sd']/s.conc$y)



### RATES EXPRESSED AS 'PER HOUR'
	for(p in c("h.dr","h.gr","h.lr")){
		plotCI(DSL$h.time, DSL[,paste(p,'.mean',sep="")], DSL[,paste(p,'.sd',sep="")], col=NA, ylab=NA, main=paste(p))
		abline(v=night$UNIXtime,col='lightgrey')
		plotCI(DSL$h.time, DSL[,paste(p,'.mean',sep="")], DSL[,paste(p,'.sd',sep="")],add=T)
	}



### RATES EXPRESSED AS 'CELLS PER HOUR'
par(mfrow=c(4,1))
	for(p in c("h2.conc","h2.dr","h2.gr","h2.lr")){
		plotCI(DSL$h.time, DSL[,paste(p,'.mean',sep="")], DSL[,paste(p,'.sd',sep="")], col=NA, ylab=NA, main=paste(p))
		abline(v=night$UNIXtime,col='lightgrey')
		plotCI(DSL$h.time, DSL[,paste(p,'.mean',sep="")], DSL[,paste(p,'.sd',sep="")],add=T)
	}


par(mfrow=c(1,1),pty='s')
plot(DSL$h.dr.mean, DSL$h.gr.mean, xlim=c(0,0.1), ylim=c(0,0.1))



write.csv(DSL, paste("/Volumes/ribalet/Cell_Division/",cruise,"/",phyto,"_HD_",cruise, ".binned.csv",sep=""),quote=F, row.names=F)



##################################################################
### CORRECT FOR PHYSICAL PROCESSES ASSOCIATED WITH SHIP MOVING ###
##################################################################

# lag <- 1
# DSL[1:lag,"h.distance"] <-  c(rep(NA, lag))
# for(i in (lag+1):nrow(DSL))
# DSL[i,"h.distance"] <-  acos(sin(DSL[i,"h.lat"]*pi/180)*sin(DSL[i-lag,"h.lat"]*pi/180)+cos(DSL[i,"h.lat"]*pi/180)*cos(DSL[i-lag,"h.lat"]*pi/180)*cos(diff(DSL[c(i, i-lag),"h.lon"]*pi/180))) * 6371
# DSL[,"h.distance2"] <- c(0,smooth.spline(DSL[-1,"h.distance"], all.knots=T)$y)
# lag <- 23
# DSL[1:lag,"d.distance"] <-  c(rep(NA, lag))
# for(i in (lag+1):nrow(DSL))
# DSL[i,"d.distance"] <-  acos(sin(DSL[i,"h.lat"]*pi/180)*sin(DSL[i-lag,"h.lat"]*pi/180)+cos(DSL[i,"h.lat"]*pi/180)*cos(DSL[i-lag,"h.lat"]*pi/180)*cos(diff(DSL[c(i, i-lag),"h.lon"]*pi/180))) * 6371

# lag <- 23
# DSL[,"h2.lr2.mean"] <- DSL[,"h.distance2"]*c(rep(NA, lag), diff(DSL[,"h2.conc2.mean"],lag))/DSL[,"d.distance"]
# plot(DSL[,"h.time"], DSL[,"h3.lr.mean"],type='o')
# points(DSL[,"h.time"], DSL[,"h3.lr2.mean"],col='blue')
# points(DSL[,"h.time"], DSL[,"h3.lr2.mean"]+DSL[,"h3.lr.mean"], col='red',type='o')
# points(DSL[,"h.time"], DSL[,"h2.conc.mean"]*DSL[,"h.dr.mean"], col='green')

DSL[,"h2.lr2.mean"] <- NA

# DAILY
na <- which(is.na(DSL[,"h.dr.mean"]))
DSL[,"d.dr.mean"] <- c(rep(NA,24), rollsum(DSL[-na,"h.dr.mean"],24), rep(NA,4))
na <- which(is.na(DSL[,"h.dr.sd"]))
DSL[,"d.dr.sd"] <- c(rep(NA,27), rollsum(DSL[-na,"h.dr.sd"],24),rep(NA,7))
na <- which(is.na(DSL[,"h3.lr.mean"]+DSL[,"h3.lr2.mean"]))
DSL[,"d.lr.mean"] <- c(rep(NA,23+23), rollsum(DSL[-na,"h3.lr.mean"]+DSL[-na,"h3.lr2.mean"],24),NA,NA)
na <- which(is.na(DSL[,"h3.lr.sd"]))
DSL[,"d.lr.sd"] <- c(rep(NA,25), rollsum(DSL[-na,"h3.lr.sd"],24),NA,NA,NA)
DSL[,"d.conc.mean"] <- c(rep(NA,23), rollmean(DSL[,"h2.conc.mean"],24))
DSL[,"d.conc.sd"] <- c(rep(NA,23), rollmean(DSL[,"h2.conc.sd"],24))
DSL[,"d.temp.mean"] <- c(rep(NA,23), rollmean(DSL[,"h2.temp.mean"],24))
DSL[,"d.temp.sd"] <- c(rep(NA,23), rollmean(DSL[,"h2.temp.sd"],24))
DSL[,"d.sal.sd"] <- c(rep(NA,23), rollmean(DSL[,"h2.sal.sd"],24))
DSL[,"d.sal.mean"] <- c(rep(NA,23), rollmean(DSL[,"h2.sal.mean"],24))
DSL[,"d.par.sd"] <- c(rep(NA,23), rollmean(DSL[,"h4.par.sd"]*exp(-Kd*5),24))
DSL[,"d.par.mean"] <- c(rep(NA,23), rollmean(DSL[,"h4.par.mean"]*exp(-Kd*5),24))
DSL[,"d.fluo.mean"] <- c(rep(NA,23), rollmean(DSL[,"h2.fluo.mean"],24))
DSL[,"d.fluo.sd"] <- c(rep(NA,23), rollmean(DSL[,"h2.fluo.sd"],24))
DSL[,"d.lon.mean"] <- c(rep(NA,23), rollmean(DSL[,"h.lon"],24))
DSL[,"d.lat.mean"] <- c(rep(NA,23), rollmean(DSL[,"h.lat"],24))


plot(DSL[,"d.dr.mean"])
plot(DSL[,"d.gr.mean"])














########################
### MODEL PARAMETERS ###
########################

	
	
	


	# PARAMETERS	
		h5 <- cut(Para.all[,"time"], breaks=breaks)
		h5.time <- as.vector(tapply(Para.all[,"time"], h5, mean))#; h5.time.numc <- na.approx(h5.time.numc, na.rm=F)
		h5.gmax.mean <- as.vector(tapply(Para.all[,"gmax"], h5, mean))#; h5.gmax.mean <- na.approx(h5.gmax.mean, na.rm=F)
		h5.dmax.mean <- as.vector(tapply(Para.all[,"dmax"], h5, mean))#; h5.dmax.mean <- na.approx(h5.dmax.mean, na.rm=F)
		h5.a.mean <- as.vector(tapply(Para.all[,"a"], h5, mean))#; h5.a.mean <- na.approx(h5.a.mean, na.rm=F)
		h5.b.mean <- as.vector(tapply(Para.all[,"b"], h5, mean))#; h5.b.mean <- na.approx(h5.b.mean, na.rm=F)
		h5.E_star.mean <- as.vector(tapply(Para.all[,"E_star"], h5, mean))#; h5.E_star.mean <- na.approx(h5.E_star.mean, na.rm=F)
		h5.resnorm.mean <- as.vector(tapply(Para.all[,"resnorm"], h5, mean))#; h5.resnorm.mean <- na.approx(h5.resnorm.mean, na.rm=F)
		h5.gmax.sd <- as.vector(tapply(Para.all[,"gmax"], h5,sd))#; h5.gmax.sd <- na.approx(h5.gmax.sd, na.rm=F)
		h5.dmax.sd <- as.vector(tapply(Para.all[,"dmax"], h5, sd))#; h5.dmax.sd<- na.approx(h5.dmax.sd, na.rm=F)
		h5.a.sd <- as.vector(tapply(Para.all[,"a"], h5, sd))#; h5.a.sd <- na.approx(h5.a.sd, na.rm=F)
		h5.b.sd <- as.vector(tapply(Para.all[,"b"], h5, sd))#; h5.b.sd <- na.approx(h5.b.sd, na.rm=F)
		h5.E_star.sd <- as.vector(tapply(Para.all[,"E_star"], h5, sd))#; h5.E_star.sd <- na.approx(h5.E_star.sd, na.rm=F)
		h5.resnorm.sd <- as.vector(tapply(Para.all[,"resnorm"], h5, sd))#; h5.resnorm.sd <- na.approx(h5.resnorm.sd, na.rm=F)
		h5.time <- as.POSIXct(h5.time,origin="1970-01-01",tz='GMT')
	
		id <- findInterval(h5.time, pop$time, rightmost.closed=F)
		h5.lat <- pop[id,"lat"]
		h5.lon <- pop[id,"long"]
	
		h5.gamma.mean <- h5.gmax.mean*(1-exp(-h4.par.mean/h5.E_star.mean))
		h5.gamma.sd <- h5.gmax.sd*(1-exp(-h4.par.sd/h5.E_star.sd))
	
		P <- data.frame(h5.time,h5.resnorm.mean,h5.resnorm.sd,h5.E_star.mean,h5.E_star.sd, h5.gmax.mean,h5.gmax.sd, h5.dmax.mean,h5.dmax.sd, h5.a.mean,h5.a.sd, h5.b.mean,h5.b.sd )

			### PARAMETERS 
		par(mfrow=c(3,2))
			for(p in c("h5.resnorm","h5.E_star","h5.gmax","h5.dmax","h5.a","h5.b")){
				plotCI(P$h5.time, P[,paste(p,'.mean',sep="")], P[,paste(p,'.sd',sep="")], col=NA, ylab=NA, main=paste(p))
				abline(v=night$UNIXtime,col='lightgrey')
				plotCI(P$h5.time, P[,paste(p,'.mean',sep="")], P[,paste(p,'.sd',sep="")],add=T)
			}
	


		del <- matrix(nrow=length(h5.time), ncol=cat)
		for(i in 1:cat){
			del[,i] <- h5.dmax.mean * (h5.a.mean*volbins[i])^h5.b.mean/ (1 + (h5.a.mean*volbins[i])^h5.b.mean)

			}
		
	
	


		par(mfrow=c(2,1),mar=c(4,4,4,4), las=1)
		plot(volbins, del[1,], ylim=c(0,0.6), type='l', col="#00007F", lwd=2, xlab="Cell volume", ylab=paste("Delta (per",10,"min)"))
				for(i in 2:nrow(del))	points(volbins, del[i,], type='l', col=jet.colors(nrow(del))[cut(as.numeric(h5.time),nrow(del))][i], lwd=2)
			ylim <- par('usr')[c(3,4)]
   			xlim <- par('usr')[c(1,2)]
  			color.legend(xlim[2]- diff(xlim)/40 , ylim[1], xlim[2], ylim[2], legend=format(as.POSIXct(pretty(h5.time),origin="1970-01-01"),"%d %b"), rect.col=jet.colors(100), gradient='y',align='rb')


		plot(seq(0,1000,by=10),h5.gmax.mean[1]*(1-exp(-seq(0,1000,by=10)/h5.E_star.mean[1])), ylim=c(0,0.3),type='l', col="#00007F", lwd=2, xlab="Light Intensity", ylab=paste("Gamma (per",10,"min)"))
				for(i in 1:length(h5.time)) points(seq(0,1000,by=10),h5.gmax.mean[i]*(1-exp(-seq(0,1000,by=10)/h5.E_star.mean[i])),type='l',col=jet.colors(nrow(del))[cut(as.numeric(h5.time),length(h5.time))][i],lwd=2)
					ylim <- par('usr')[c(3,4)]
   					xlim <- par('usr')[c(1,2)]
  			color.legend(xlim[2]- diff(xlim)/40 , ylim[1], xlim[2], ylim[2], legend=format(as.POSIXct(pretty(h5.time),origin="1970-01-01"),"%d %b"), rect.col=jet.colors(100), gradient='y',align='rb')

	
	


		write.csv(P, paste("/Volumes/ribalet/Cell_Division/",cruise,"/",phyto,"_HD_",cruise, ".PARA.csv",sep=""),quote=F, row.names=F)


