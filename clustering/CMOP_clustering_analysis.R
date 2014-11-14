##############################################
## reclustering and reanalysis of CMOP data ##
## August/Sept 2014 ##########################
##############################################

library(flowPhyto)


cruise.path <- paste("/Volumes/seaflow/CMOP_6");pop.def  <- readPopDef(paste(cruise.path, "/pop.def.tab",sep="")) 
cruise.path<-paste("."); save.path<-paste(".") 

all.files <- getCruiseFiles(cruise.dir=cruise.path, ext=".evt.opp") #warning: "no files found in directory 2013_252"



#for(file in all.files){	
print(file)
day <- basename(dirname(file))
file <- all.files[690]; pop.def  <- readPopDef(paste(cruise.path, "/pop.def.tab",sep="")) #change number to change file
opp <-readSeaflow(file,transform=F)
opp[,"pop"] <- 'x'

#okay crypto files:480,500,300
#meh crypto files:520,540,100,200,400,600,700
#bad crypto files:710
#max x too small after 800, at 690

##Cluster Beads##
	x <- subset(opp, pop=='x')
	beads <- subset(x, pe == max(opp$pe, na.rm=T)) ## PE-saturated particles assigned as 'beads
	opp[row.names(beads),'pop'] <- 'beads'
	
	x <- subset(opp, pop=='x')
	xvar <- pop.def["beads", "xvar"]
	yvar <- pop.def["beads", "yvar"]
	beads <- subset(x, x[,yvar] > 0.75*x[,xvar] + pop.def["beads", "lim"] & x[,xvar] > pop.def["beads", "xmin"] & x[,yvar] > pop.def["beads", "ymin"] & x[,xvar] < pop.def["beads", "xmax"] & x[,yvar] < pop.def["beads", "ymax"])
	opp[row.names(beads),'pop'] <- 'beads'
	
	
##Cluster Large NON_CHL Particles##
	x <- subset(opp, pop=='x')
	hetero <- subset(x, x[,"fsc_small"] > 0.75*x[,"chl_small"] + 25000)
	opp[row.names(hetero), 'pop'] <- 'hetero'


##Cluster Synechococcus##
	x <- subset(opp, pop=='x')
	yvar <- pop.def["synecho", "yvar"]
	xvar <- pop.def["synecho", "xvar"]
	synecho <- subset(x, x[,yvar] > x[,xvar] - pop.def["synecho", "lim"] & x[,xvar] > pop.def["synecho", "xmin"] & x[,yvar] > pop.def["synecho", "ymin"] & x[,xvar] < 	pop.def["synecho", "xmax"] & x[,yvar] < pop.def["synecho", "ymax"])
	opp[row.names(synecho), 'pop'] <- 'synecho'
	
	
##Cluster Cryptophytes SMALL##
	x <- subset(opp, pop=='x')
	yvar <- pop.def["crypto1", "yvar"]
	xvar <- pop.def["crypto1", "xvar"]
	crypto1 <- subset(x, x[,yvar] > x[,xvar] + pop.def["crypto1", "lim"] & x[,xvar] > pop.def["crypto1", "xmin"] & x[,yvar] > pop.def["crypto1", "ymin"])
	opp[row.names(crypto1), 'pop'] <- 'crypto1'


##Cluster Cryptophytes LARGE##
	x <- subset(opp, pop=='x')
	yvar <- pop.def["crypto2", "yvar"]
	xvar <- pop.def["crypto2", "xvar"]
	crypto2 <- subset(x, x[,yvar] > x[,xvar] + pop.def["crypto2", "lim"] & x[,xvar] > pop.def["crypto2", "xmin"] & x[,yvar] > pop.def["crypto2", "ymin"])
	opp[row.names(crypto2), 'pop'] <- 'crypto2'


##Cluster Other phytoplankton##
	x <- subset(opp, pop=='x')
	yvar <- pop.def["other", "yvar"] 
	xvar <- pop.def["other", "xvar"]
	other <- subset(x,  x[,xvar] > pop.def["other", "xmin"] & x[,yvar] > pop.def["other", "ymin"])
	opp[row.names(other), 'pop'] <- 'other'
	
	
		par(mfrow=c(2,2), pty='s')
 plotCytogram(opp, "fsc_small","chl_small", pop.def=pop.def, transform=F)
 plotCytogram(opp, "fsc_small","pe", pop.def=pop.def, transform=F)
 plotCytogram(opp, "chl_small","pe", pop.def=pop.def, transform=F)


write.table(opp$pop,paste(save.path,"/",day,"/",basename(file),".",getFileNumber(file),'-class.vct',sep=""),row.names=F,quote=F)
		
}




##Check Your Work##

	#chl_small and pe
	plot(opp[,c("chl_small","pe")], pch=20, cex=.75, col="grey")	
	points(crypto1[,c("chl_small","pe")], pch=20, cex=.75, col="blue")	
	points(crypto2[,c("chl_small","pe")], pch=20, cex=.75, col="red")	

	#fsc_small and pe	
	plot(opp[,c("fsc_small","pe")], pch=20, cex=.75, col="grey")	
	points(crypto1[,c("fsc_small","pe")], pch=20, cex=.75, col="blue")	
	points(crypto2[,c("fsc_small","pe")], pch=20, cex=.75, col="red")	
	


##Changing pop.def##

	#need to change line between crypto pop and x max?
pop.def_new <- pop.def
pop.def_new["crypto2","ymin"]<-17000



