##############################################
## reclustering and reanalysis of CMOP data ##
## August/Sept 2014 ##########################
##############################################

library(flowPhyto)


cruise.path <- paste("/Volumes/seaflow/CMOP_6");pop.def  <- readPopDef(paste(cruise.path, "/pop.def.tab",sep="")) 
cruise.path<-paste("."); save.path<-paste(".") #what is this for?

all.files <- getCruiseFiles(cruise.dir=cruise.path, ext=".evt.opp") #warning: "no files found in directory"


##since I only want to recluster (not refilter), don't need this?

#for(file in all.files){	
#print(file)
#day <- basename(dirname(file))
#file <- all.files[480]; pop.def  <- readPopDef(paste(cruise.path, "/pop.def.tab",sep=""))
#opp <-readSeaflow(file,transform=F)
#opp[,"pop"] <- 'x'


#Cluster Beads
	x <- subset(opp, pop=='x')
	beads <- subset(x, pe == max(opp$pe, na.rm=T)) ## PE-saturated particles assigned as 'beads
	opp[row.names(beads),'pop'] <- 'beads'
	
	x <- subset(opp, pop=='x')
	xvar <- pop.def["beads", "xvar"]
	yvar <- pop.def["beads", "yvar"]
	beads <- subset(x, x[,yvar] > 0.75*x[,xvar] + pop.def["beads", "lim"] & x[,xvar] > pop.def["beads", "xmin"] & x[,yvar] > pop.def["beads", "ymin"] & x[,xvar] < pop.def["beads", "xmax"] & x[,yvar] < pop.def["beads", "ymax"])
	opp[row.names(beads),'pop'] <- 'beads'

	#Cluster Large NON_CHL Particles
	x <- subset(opp, pop=='x')
	hetero <- subset(x, x[,"fsc_small"] > 0.75*x[,"chl_small"] + 25000)
	opp[row.names(hetero), 'pop'] <- 'hetero'


	#Cluster Synechococcus
	x <- subset(opp, pop=='x')
	yvar <- pop.def["synecho", "yvar"]
	xvar <- pop.def["synecho", "xvar"]
	synecho <- subset(x, x[,yvar] > x[,xvar] - pop.def["synecho", "lim"] & x[,xvar] > pop.def["synecho", "xmin"] & x[,yvar] > pop.def["synecho", "ymin"] & x[,xvar] < 	pop.def["synecho", "xmax"] & x[,yvar] < pop.def["synecho", "ymax"])
	opp[row.names(synecho), 'pop'] <- 'synecho'
	
 
	#Cluster Cryptophytes SMALL
	x <- subset(opp, pop=='x')
	yvar <- pop.def["crypto1", "yvar"]
	xvar <- pop.def["crypto1", "xvar"]
	crypto1 <- subset(x, x[,yvar] > x[,xvar] + pop.def["crypto1", "lim"] & x[,xvar] > pop.def["crypto1", "xmin"] & x[,yvar] > pop.def["crypto1", "ymin"])
	opp[row.names(crypto1), 'pop'] <- 'crypto1'

	#Cluster Cryptophytes LARGE
	x <- subset(opp, pop=='x')
	yvar <- pop.def["crypto2", "yvar"]
	xvar <- pop.def["crypto2", "xvar"]
	crypto2 <- subset(x, x[,yvar] > x[,xvar] + pop.def["crypto2", "lim"] & x[,xvar] > pop.def["crypto2", "xmin"] & x[,yvar] > pop.def["crypto2", "ymin"])
	opp[row.names(crypto2), 'pop'] <- 'crypto2'

	

	#Cluster Other phytoplankton
	x <- subset(opp, pop=='x')
	yvar <- pop.def["other", "yvar"] 
	xvar <- pop.def["other", "xvar"]
	other <- subset(x,  x[,xvar] > pop.def["other", "xmin"] & x[,yvar] > pop.def["other", "ymin"])
	opp[row.names(other), 'pop'] <- 'other'





