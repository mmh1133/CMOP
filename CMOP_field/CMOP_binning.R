library(rgl)
library(zoo)
library(plotrix)
library(popcycle)
	library(popcycle)
	set.evt.location("/Volumes/seaflow/CMOP_6")
	set.project.location("~/CMOP_2013_f2")
	set.cruise.id("CMOP_6")

jet.colors <- colorRampPalette(c("blue4","royalblue4","deepskyblue3", "seagreen3", "yellow", "orangered2","darkred")) # Banas


#### PARAMETERS
cat <- 57#2^6
cruise <- 'CMOP_6'
phyto <- 'crypto'



## MODEL
all.filelist <- list.files(paste("/Users/francois/CMOP/CMOP_field/",sep=""),pattern=paste("NEW",phyto,"_modelHD_growth_",cruise,"_Ncat",cat,sep=""))
filelist <- all.filelist[grep(pattern=paste(phyto), all.filelist)]


n <- c <- 1
Conc.all <- N.proj.all <- V.hist.all <- div.rate <- para.all <- Col <- NULL
for(file in filelist){
	#file <- filelist[11]
	load(paste("/Users/francois/CMOP/CMOP_field","/",file, sep=""))
	print(file)
	print(n)
		dim <- conc.proj.all <- n.proj.all <- v.hist.all <- dr.all <- p.all <- NULL
			for(i in 2:dim(model)[2]){
				n.proj <- model[4,i][[1]]
				n.proj.all <- cbind(n.proj.all, n.proj)			
							
				conc.proj <- cbind(as.numeric(colnames(n.proj)), as.numeric(colSums(n.proj)))
				conc.proj.all <- rbind(conc.proj.all, conc.proj)
				
				try(dr <- model[2,i][[1]])
				h.dr <- cbind(as.numeric(colnames(dr)), as.numeric(dr))
				dr.all <- rbind(dr.all, h.dr)
				
				try(v.proj <- model[3,i][[1]])	
				v.hist.all <- cbind(v.hist.all, v.proj)			

				try(para <- model[1,i][[1]])
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
		#h.lat <- pop[id,"lat"]
		#h.lon<- pop[id,"long"]
		
		#D <- data.frame(cbind(h.time, h.lat, h.lon, h.dr.mean, h.dr.sd))
		D <- data.frame(cbind(h.time, h.dr.mean, h.dr.sd))   
		   
			par(mfrow=c(2,1))
		    plot(h.time, h.dr.mean, ylim=c(0,max(h.dr.mean, na.rm=T)*1.3))		
			abline(v=night$UNIXtime,col='lightgrey')
		    plotCI(h.time, h.dr.mean, h.dr.sd, add=T)		


					
	# SEAFLOW
		# stat <- read.delim(paste("/Volumes/seaflow/",cruise,"/stats.tab",sep="")) ## OLD VERSION
		stat <- get.stat.table()
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
	Par.path <- paste("/Users/francois/CMOP/CMOP_field","/Par_",cruise,sep="")
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



write.csv(DSL, paste("/Users/francois/CMOP/CMOP_field","/","NEW",phyto,"_HD_",cruise, ".binned.csv",sep=""),quote=F, row.names=F)


