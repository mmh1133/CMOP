library(flowCore)
library(splancs) 
library(popcycle)
library(cluster)
library(plotrix)

.rainbow.cols <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow","#FF7F00", "red", "#7F0000"))


##############
### STEP 1 ###
##############



## BATCH FILES ##



path <- paste("/Users/francois/CMOP/CMOP_INFLUX_Sept2013")
file.list <- dir(path, pattern = ".fcs", recursive=T)
savepath<- "/Users/francois/CMOP/results"

## fun stuff for later 

table <- NULL
for(file in file.list){
	print(file)
	fcs <- read.FCS(paste(path,"/",file,sep=""), transformation= FALSE)
	info <- try(fcs@description$`$SMNO`)
	#strsplit(info, " ")
	t <- c(file, info)
	table <- rbind(table, t)
}

table <- table[order(table$info),]

write.csv(table,file=paste(savepath,"file_names.csv", sep=""), row.names=FALSE)

file.names<-read.csv("/Users/francois/CMOP/CMOP_INFLUX_Sept2013/file_names.csv", header=TRUE)



file <- file.list[2]

#files that are strange:40, 5, 12, 35
#files that have only 1 crypto:10, 7
#okay files:20, 30, 25
#files that could potentially work well:15


summary.table <- NULL
for (file in file.list) {
	print(paste("processing file:",file))



## read FCS ###


fcs <- read.FCS(paste(path,"/",file,sep=""), transformation= FALSE)
opp <- caroline:::tab2df(exprs(fcs)) 

if(ncol(opp) == 7) names(opp) <- c("pulse_width", "fsc_small",  "chl_small", "pe", "green","ssc","time")
if(ncol(opp) == 11) names(opp) <- c("pulse_width", "fsc_small", "fsc_big", "chl_small", "monochrome","pe", "green","blue","wavelength","ssc","time")


#### 
opp$pop <- 0


### GATING ###

### NOISE & BEADS & CRYPTO 
x <- subset(opp, pop==0)
plot(x[,"chl_small"], x[,"pe"], pch=16, cex=0.4, col = densCols(x[,"chl_small"], x[,"pe"], colramp = .rainbow.cols), 
		xlim=c(0,2^16), ylim=c(0,2^16), main="Gating NOISE & BEADS")

#print("Gating Noise")
#poly.noise <- getpoly(quiet=TRUE)
#print("Gating Beads")
#poly.beads <- getpoly(quiet=TRUE)
#print("Gating Pre-Crypto1")
#poly.pre.crypto1 <- getpoly(quiet=TRUE)
#print("Gating Pre-Crypto2")
#poly.pre.crypto2 <- getpoly(quiet=TRUE)

polygon(poly.noise, lwd=2,border=2, col=NA)
polygon(poly.beads, lwd=2,border=2, col=NA)
polygon(poly.pre.crypto1, lwd=2,border=2, col=NA)
polygon(poly.pre.crypto2, lwd=2,border=2, col=NA)

noise <- subset(x,inout(x[,c("chl_small","pe")],poly=poly.noise, bound=TRUE, quiet=TRUE))
opp[row.names(noise),'pop'] <- "noise"
beads <- subset(x,inout(x[,c("chl_small","pe")],poly=poly.beads, bound=TRUE, quiet=TRUE))
opp[row.names(beads),'pop'] <- "beads"
pre.crypto1 <- subset(x,inout(x[,c("chl_small","pe")],poly=poly.pre.crypto1, bound=TRUE, quiet=TRUE))
pre.crypto2 <- subset(x,inout(x[,c("chl_small","pe")],poly=poly.pre.crypto2, bound=TRUE, quiet=TRUE))


## SYN & HETERO
x <- subset(opp, pop==0)
plot(x[,"fsc_small"], x[,"pe"], pch=NA, xlim=c(0,2^16), ylim=c(0,2^16), main="Gating PRO, SYNECHO & BACT")
points(beads$fsc_small, beads$pe, col="grey")
points(pre.crypto1$fsc_small, pre.crypto1$pe, col=2)
points(pre.crypto2$fsc_small, pre.crypto2$pe, col=3)
points(x[,"fsc_small"], x[,"pe"], pch=16, cex=0.4, col = densCols(x[,"fsc_small"], x[,"pe"], colramp = .rainbow.cols), xlim=c(0,2^16), ylim=c(0,2^16))

#print("Gating Noise2")
#poly.noise2 <- getpoly(quiet=TRUE)
#print("gating SYN")
#poly.syn <- getpoly(quiet=TRUE)
#print("gating CRYPTO1")
#poly.crypto1 <- getpoly(quiet=TRUE)
#print("gating CRYPTO2")
#poly.crypto2 <- getpoly(quiet=TRUE)

polygon(poly.noise2, lwd=2,border=2, col=NA)
polygon(poly.syn, lwd=2,border=2, col=NA)
polygon(poly.crypto1, lwd=2,border=2, col=NA)
polygon(poly.crypto2, lwd=2,border=2, col=NA)


noise <- subset(x,inout(x[,c("fsc_small","pe")],poly=poly.noise2, bound=TRUE, quiet=TRUE))
opp[row.names(noise),'pop'] <- "noise"
syn <- subset(x,inout(x[,c("fsc_small","pe")],poly=poly.syn, bound=TRUE, quiet=TRUE))
opp[row.names(syn),'pop'] <- "synecho"
crypto1 <- subset(x,inout(x[,c("fsc_small","pe")],poly=poly.crypto1, bound=TRUE, quiet=TRUE))
opp[row.names(crypto1),'pop'] <- "crypto1"
crypto2 <- subset(x,inout(x[,c("fsc_small","pe")],poly=poly.crypto2, bound=TRUE, quiet=TRUE))
opp[row.names(crypto2),'pop'] <- "crypto2"



		
## PLOT PHYTO ##
	
			
	png(paste(savepath, "/", basename(file),".png", sep=""),width=9, height=12, unit='in', res=100)

par(mfrow=c(2,2))
plot.vct.cytogram(opp, "fsc_small", "chl_small",main=basename(file))
plot.vct.cytogram(opp, "fsc_small", "pe")
plot.vct.cytogram(opp, "chl_small", "pe")

	dev.off()



### SAVE .VCT ###


write.table(opp$pop, paste(savepath,"/",basename(file),'-class.vct',sep=""), row.names=FALSE, col.names='pop', quote=FALSE)


### SUMMARY ###


stat.table <- NULL
for(i in c("beads", "synecho","prochloro","heteroBact")){
#print(i)
p <- subset(opp, pop == i)
n <- nrow(p)
if(n ==0) {
fsc <- 0
chl <- 0
	}else{
fsc <- round(median(p$fsc_small))
chl <- round(median(p$chl_small))
ssc <- round(median(p$ssc))
green <- round(median(p$green))
}
var <- cbind(i,n,fsc,chl,ssc,green)
stat.table <- rbind(stat.table, var)
}


table <- data.frame(cbind(stat.table, file=basename(file)))
summary.table <- rbind(summary.table, table)

}


write.csv(summary.table,file=paste(savepath,"summary_2.csv", sep=""), row.names=FALSE)

























##############
### STEP 2 ###
##############

file.list
file <- file.list[58]

fcs <- read.FCS(paste(path,"/",file,sep=""), transformation= FALSE)
opp <- caroline:::tab2df(exprs(fcs)) 

if(ncol(opp) == 7) names(opp) <- c("pulse_width", "fsc_small",  "chl_small", "pe", "green","ssc","time")
if(ncol(opp) == 11) names(opp) <- c("pulse_width", "fsc_small", "fsc_big", "chl_small", "monochrome","pe", "green","blue","wavelength","ssc","time")



opp$pop <- 0



### GATING ###


### NOISE & BEADS & CRYPTO 
x <- subset(opp, pop==0)
plot(x[,"chl_small"], x[,"pe"], pch=16, cex=0.4, col = densCols(x[,"chl_small"], x[,"pe"], colramp = .rainbow.cols), 
		xlim=c(0,2^16), ylim=c(0,2^16), main="Gating NOISE & BEADS")

print("Gating Noise")
poly.noise <- getpoly(quiet=TRUE)
print("Gating Beads")
poly.beads <- getpoly(quiet=TRUE)
print("Gating Pre-Crypto1")
poly.pre.crypto1 <- getpoly(quiet=TRUE)
print("Gating Pre-Crypto2")
poly.pre.crypto2 <- getpoly(quiet=TRUE)

polygon(poly.noise, lwd=2,border=2, col=NA)
polygon(poly.beads, lwd=2,border=2, col=NA)
polygon(poly.pre.crypto1, lwd=2,border=2, col=NA)
polygon(poly.pre.crypto2, lwd=2,border=2, col=NA)

noise <- subset(x,inout(x[,c("chl_small","pe")],poly=poly.noise, bound=TRUE, quiet=TRUE))
opp[row.names(noise),'pop'] <- "noise"
beads <- subset(x,inout(x[,c("chl_small","pe")],poly=poly.beads, bound=TRUE, quiet=TRUE))
opp[row.names(beads),'pop'] <- "beads"
pre.crypto1 <- subset(x,inout(x[,c("chl_small","pe")],poly=poly.pre.crypto1, bound=TRUE, quiet=TRUE))
pre.crypto2 <- subset(x,inout(x[,c("chl_small","pe")],poly=poly.pre.crypto2, bound=TRUE, quiet=TRUE))


# SYN & HETERO
x <- subset(opp, pop==0)
plot(x[,"fsc_small"], x[,"pe"], pch=NA, xlim=c(0,2^16), ylim=c(0,2^16), main="Gating PRO, SYNECHO & BACT")
points(beads$fsc_small, beads$pe, col="grey")
points(pre.crypto1$fsc_small, pre.crypto1$pe, col=2)
points(pre.crypto2$fsc_small, pre.crypto2$pe, col=3)
points(x[,"fsc_small"], x[,"pe"], pch=16, cex=0.4, col = densCols(x[,"fsc_small"], x[,"pe"], colramp = .rainbow.cols), xlim=c(0,2^16), ylim=c(0,2^16))

print("Gating Noise2")
poly.noise2 <- getpoly(quiet=TRUE)
print("gating SYN")
poly.syn <- getpoly(quiet=TRUE)
print("gating CRYPTO1")
poly.crypto1 <- getpoly(quiet=TRUE)
print("gating CRYPTO2")
poly.crypto2 <- getpoly(quiet=TRUE)

polygon(poly.noise2, lwd=2,border=2, col=NA)
polygon(poly.syn, lwd=2,border=2, col=NA)
polygon(poly.crypto1, lwd=2,border=2, col=NA)
polygon(poly.crypto2, lwd=2,border=2, col=NA)


noise <- subset(x,inout(x[,c("fsc_small","pe")],poly=poly.noise2, bound=TRUE, quiet=TRUE))
opp[row.names(noise),'pop'] <- "noise"
syn <- subset(x,inout(x[,c("fsc_small","pe")],poly=poly.syn, bound=TRUE, quiet=TRUE))
opp[row.names(syn),'pop'] <- "synecho"
crypto1 <- subset(x,inout(x[,c("fsc_small","pe")],poly=poly.crypto1, bound=TRUE, quiet=TRUE))
opp[row.names(crypto1),'pop'] <- "crypto1"
crypto2 <- subset(x,inout(x[,c("fsc_small","pe")],poly=poly.crypto2, bound=TRUE, quiet=TRUE))
opp[row.names(crypto2),'pop'] <- "crypto2"


### SAVE .VCT ###

write.table(opp$pop, paste(savepath,"/",basename(file),'-class.vct',sep=""), row.names=FALSE, col.names='pop', quote=FALSE)

			
png(paste(savepath, "/", basename(file),".png", sep=""),width=9, height=12, unit='in', res=100)

par(mfrow=c(2,2))
plot.vct.cytogram(opp, "fsc_small", "chl_small",main=basename(file))
plot.vct.cytogram(opp, "fsc_small", "pe")
plot.vct.cytogram(opp, "chl_small", "pe")

dev.off()



















##############
### STEP 3 ###
##############

summary.table <- NULL

for (file in file.list) {
	print(paste("processing file:",file))



## read FCS ###


fcs <- read.FCS(paste(path,"/",file,sep=""), transformation= FALSE)
opp <- caroline:::tab2df(exprs(fcs)) 

if(ncol(opp) == 7) names(opp) <- c("pulse_width", "fsc_small",  "chl_small", "pe", "green","ssc","time")
if(ncol(opp) == 11) names(opp) <- c("pulse_width", "fsc_small", "fsc_big", "chl_small", "monochrome","pe", "green","blue","wavelength","ssc","time")


#### LOAD THE VCT
pop.id <- read.delim(paste(savepath,"/",basename(file),"-class.vct",sep=""))
opp$pop <- pop.id



### SUMMARY ###

info <- try(fcs@description$`$SMNO`)

stat.table <- NULL
	for(i in c("beads", "synecho","crypto1","crypto2")){
	#print(i)
		p <- subset(opp, pop == i)
		n <- nrow(p)
			if(n ==0) {
				fsc <- 0
				chl <- 0
			}else{
				fsc <- round(mean(p$fsc_small))
				chl <- round(mean(p$chl_small))
				pe <- round(mean(p$pe))
			}
		var <- cbind(basename(file),info, n,fsc,chl,pe)
		stat.table <- rbind(stat.table, var)
	}

summary.table <- rbind(summary.table, stat.table)

}


write.csv(summary.table,file=paste(savepath,"summary_2.csv", sep=""), row.names=FALSE)



















