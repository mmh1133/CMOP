# This script does model optimization on the phyto
# for i in $(seq 0 1 24); do echo "Rscript ~/Cell_Division/CMOP_6/model_HD_div_rate_CMOP.R $i crypto CMOP_6" | qsub -lwalltime=30:00:00,nodes=1:ppn=1 -N proGR$i -d.; done


library(rgl)
library(DEoptim)
library(zoo)

args <- commandArgs(TRUE)
t <- as.numeric(args[1])
phyto <- as.character(args[2])
cruise <- as.character(args[3])
script.home <- as.character(args[4])


# t = 1
# phyto= "prochloro"
# cruise = "DeepDOM"
# script.home <- "/Volumes/gwennm/DeepDOM/ssPopModel"
# in.dir <-"/Volumes/gwennm/DeepDOM/Cell_Division"
# out.dir <- "/Volumes/gwennm/DeepDOM/Cell_Division"

#  t = 1
# phyto= "crypto"
# cruise = "CMOP_6"
# script.home <- "/Users/francois/CMOP/CMOP_field"
in.dir <- out.dir <- "/Users/francois/CMOP/CMOP_field"

# in.dir <- out.dir <- script.home <- "/Users/francois/Documents/DATA/SeaFlow/CMOP/CMOP_git/CMOP_field"

source(paste(script.home,'functions_modelHD.R',sep="/"), chdir = TRUE)

jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow",	"#FF7F00", "red", "#7F0000"))




###############################
m <- 57#2^6 # number of size class
###############################




###############
## REFERENCE ##
###############
# library(R.matlab); mat <- readMat("/Users/francois/Documents/DATA/SeaFlow/Cell_Division/Matlab/day733320data.mat"); res <- readMat("/Users/francois/Documents/DATA/SeaFlow/Cell_Division/Matlab/results.mat")
# volbins <- mat$volbins[1,]
# Edata <- mat$Edata
# V.hists <- mat$Vhists
# N.dist <- mat$N.dist
# Vproj <- res$Vproj
# para <- V.hists; percentile <- cut(para, 100); plot3d(log(rep(volbins , breaks)), rep(1:ncol(para), each=nrow(para)), z=matrix(para), col=jet.colors(100)[percentile], type='l', lwd=6)


	##############
	## PAR DATA ##
	##############
	Par.path <- paste0(in.dir,"/Par_",cruise)
	Par <- read.csv(Par.path, sep=",")
	Par$time <- as.POSIXct(Par$time, format="%Y/%m/%d %H:%M:%S",  tz= "GMT")
	Par$num.time <- as.numeric(Par$time)

## ONLY FOR CMOP_6 PAR
	Par[which(Par$par < 1.5), "par"] <- 0

	#######################	
	## SIZE DISTRIBUTION ##
	#######################

	# t <- 0	
	# phyto <- "prochloro"
   
    print(paste("time delay:", t))
	print(paste("phytoplankton population:",phyto))
	
	load(paste(in.dir,"/", phyto,"_dist_Ncat",m,"_",cruise,sep=""))
	Vhists <- distribution[[1]]
	header <- na.approx(as.numeric(colnames(Vhists)), na.rm=F)
	Vhists <- try(t(apply(Vhists, 1, function(x) na.approx(x, na.rm=F))))
	colnames(Vhists) <- header
	
	N_dist <- distribution[[2]]
	N_dist <- try(t(apply(N_dist, 1, function(x) na.approx(x, na.rm=F))))
	colnames(N_dist) <- header    
	

	volbins <- as.numeric(row.names(Vhists))
			sizebins <- 2*(volbins*3/(pi*4))^(1/3)# to check the actual diameter

	time.numc <- as.numeric(colnames(Vhists))	
	time <- as.POSIXct(time.numc, origin="1970-01-01" ,tz="GMT")	
	n.day <- round(diff(range(na.omit(time)))); print(paste("Number of days in the dataset:",n.day))

	para <- Vhists; percentile <- cut(unlist(para), 100); plot3d(log2(rep(as.numeric(row.names(para)), dim(para)[2])), rep(as.numeric(colnames(para)), each=dim(para)[1]) , Vhists , col=jet.colors(100)[percentile], type='l', lwd=6, xlab="size class", ylab="time", zlab="Frequency")
	
	

	##############################
	## RUN size.model.functions ##
	##############################

	resol <-  60 # number of minutes per interval
	breaks <- 25*60/resol

	model <- array(NA, dim=c(4,1))
for(t in 0:24){

	for(i in seq(1,length(time)-24, 24)){
		print(paste("starting hour:",i+t))
		#i <- 25
		start <- time[i+t]
		end <- time[(i+t)+24]
		
		# if(is.na(end)){
			# print("cycle is less than 24h")
			# next
			# }
		# if(is.na(start)){
			# print("NA in start time, skip ahead")
			# next
		# }
		# if(sum(is.na(time[i:(i+24)+t]))>4){
			# print("more than 4 hours missing from time period, skip ahead")
			# next
		# }
		print(paste("calculating growth projection from ",start , "to",end))

	
	plot(Par$time, Par$par, type='o'); points(c(start, end),c(0,0), col='red',pch=16, cex=2)

		### SELECT SIZE DISTRIBUTION for DAY i
		V.hists <- Vhists[,c(i:(i+24)+t)]
		N.dist <- N_dist[,c(i:(i+24)+t)]
		
		# #NAs break this part and need to be made into zeros
			# mk.zero <- which(is.na(V.hists))
			# V.hists[mk.zero] <- 0
			# mk.zero <- which(is.na(N.dist))
			# N.dist[mk.zero] <- 0

	    # para <- V.hists; percentile <- cut(unlist(para), 100); plot3d(log(rep(as.numeric(row.names(para))), dim(para)[2]), rep(as.numeric(colnames(para)), each=dim(para)[1]), para , col=jet.colors(100)[percentile], type='l', lwd=6, xlab="size class", ylab="time", zlab="Frequency")


	 
		para <- proj$Vproj; percentile <- cut(unlist(para), 100); plot3d(log(rep(volbins, 24)), rep(1:ncol(para), each=nrow(para)), z=matrix(para), col=jet.colors(100)[percentile], type='l', lwd=6, xlab="size class", ylab="time", zlab="Frequency")
		
		if(class(proj) !='try-error'){
		model <- matrix(cbind(as.array(model), as.array(proj)), nrow=4,ncol=ncol(model)+1)
	    save(model, file=paste(out.dir,"/",phyto,"_modelHD_growth_",cruise,"_Ncat",m,"_t",t, sep=""))

	  }else{print("error during optimization")}
}
