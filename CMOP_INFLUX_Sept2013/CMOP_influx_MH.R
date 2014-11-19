library(flowCore)
library(splancs) 
library(popcycle)
library(cluster)
library(plotrix)

.rainbow.cols <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow","#FF7F00", "red", "#7F0000"))


#################
## BATCH FILES ##
#################


path <- paste("/Users/francois/CMOP/CMOP_INFLUX_Sept2013")
file.list <- dir(path, pattern = ".fcs", recursive=T)

file <- file.list[25]
summary.table <- NULL

#files that are strange:40, 5
#files that have only 1 crypto:10
#okay files:20, 30, 25
#files that could potentially work well:15

for (file in file.list) {
	print(paste("processing file:",file))


###############
## read FCS ###
###############

fcs <- read.FCS(paste(path,"/",file,sep=""), transformation= FALSE)
opp <- caroline:::tab2df(exprs(fcs)) 

if(ncol(opp) == 7) names(opp) <- c("pulse_width", "fsc_small",  "chl_small", "pe", "green","ssc","time")
if(ncol(opp) == 11) names(opp) <- c("pulse_width", "fsc_small", "fsc_big", "chl_small", "monochrome","pe", "green","blue","wavelength","ssc","time")


opp$pop <- 0

##############
### GATING ###
##############

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


## SYN & HETERO
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





### Phyto
x <- subset(opp, pop==0)
plot(x[,"fsc_small"], x[,"chl_small"], pch=NA, cex=0.4, xlim=c(0,2^16), ylim=c(0,2^16),main="Gating pre.PRO")
points(beads$fsc_small, beads$chl_small, col="grey")
points(syn$fsc_small, syn$chl_small, col=1)
points(crypto1$fsc_small, crypto1$chl_small, col=2)
points(crypto2$fsc_small, crypto2$chl_small, col=3)
points(x[,"fsc_small"], x[,"chl_small"], pch=16, cex=0.4, col = densCols(x[,"fsc_small"], x[,"chl_small"], colramp = .rainbow.cols),
	 xlim=c(0,2^16), ylim=c(0,2^16),main="Gating Phyto")

# print("gating other Phyto")
# poly.other <- getpoly(quiet=TRUE)
# print("gating hetero")
# poly.hetero <- getpoly(quiet=TRUE)

polygon(poly.other, lwd=2,border=2, col=NA)
polygon(poly.hetero, lwd=2,border=2, col=NA)


other <- subset(x,inout(x[,c("fsc_small","chl_small")],poly=poly.other, bound=TRUE, quiet=TRUE))
opp[row.names(other),'pop'] <- "other"
hetero <- subset(x,inout(x[,c("fsc_small","chl_small")],poly=poly.hetero, bound=TRUE, quiet=TRUE))
opp[row.names(hetero),'pop'] <- "hetero"


par(mfrow=c(2,2))
plot.vct.cytogram(opp, "fsc_small", "chl_small",main=basename(file))
plot.vct.cytogram(opp, "fsc_small", "pe")
plot.vct.cytogram(opp, "chl_small", "pe")


###################
### SAVE .VCT ###
###################

write.table(opp$pop, paste(savepath,"/",basename(file),'-class.vct',sep=""), row.names=FALSE, col.names='pop', quote=FALSE)




###############
### SUMMARY ###
###############

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

		################
		## PLOT PHYTO ##
		################
			
	png(paste(savepath, "/", basename(file),".png", sep=""),width=9, height=12, unit='in', res=100)

	breaks= 25
	opp <- subset(opp, !(pop == 'noise'))
	hist1 <- hist(opp$fsc_small, breaks=seq(0,2^16, by=2^16/breaks), plot=FALSE)
	hist2 <- hist(opp$chl_small, breaks=seq(0,2^16, by=2^16/breaks), plot=FALSE)
	hist3 <- hist(opp$pe, breaks=seq(0,2^16, by=2^16/breaks), plot=FALSE)
	hist4 <- hist(opp$green, breaks=seq(0,2^16, by=2^16/breaks), plot=FALSE)
	hist5 <- hist(opp$green_lin, breaks=seq(0,2^16, by=2^16/breaks), plot=FALSE)
	hist6 <- hist(opp$ssc, breaks=seq(0,2^16, by=2^16/breaks), plot=FALSE)


	def.par <- par(no.readonly = TRUE) # save default, for resetting...
	nf <- layout(matrix(c(2,0,5,0,1,3,4,6,8,0,11,0,7,9,10,12,14,0,16,16,13,15,16,16),6,4,byrow=TRUE), c(3,1,3,1,3), c(1,3,1,3,1,3), TRUE)
	
	par(mar=c(6,6,1,1))
	plotCytogram(opp, 'fsc_small', 'chl_small', pop.def=pop.def)
	par(mar=c(0,6,1,1))
	barplot(hist1$counts, axes=FALSE, space=0, col=NA)
	par(mar=c(6,0,1,1))
	barplot(hist2$counts, axes=FALSE, space=0, horiz=TRUE, col=NA)

	par(mar=c(6,6,1,1))
	plotCytogram(opp, 'fsc_small', 'pe', pop.def=pop.def)
	par(mar=c(0,6,1,1))
	barplot(hist1$counts, axes=FALSE, space=0, col=NA)
	par(mar=c(6,0,1,1))
	barplot(hist3$counts, axes=FALSE, space=0, horiz=TRUE, col=NA)
		
	par(mar=c(6,6,1,1))
	plotCytogram(opp, 'chl_small', 'pe', pop.def=pop.def)
	par(mar=c(0,6,1,1))
	barplot(hist2$counts, axes=FALSE, space=0, col=NA)
	par(mar=c(6,0,1,1))
	barplot(hist3$counts, axes=FALSE, space=0, horiz=TRUE, col=NA)
	
	par(mar=c(6,6,1,1))
	plotCytogram(opp, 'fsc_small', 'green', pop.def=pop.def)
	par(mar=c(0,6,1,1))
	barplot(hist1$counts, axes=FALSE, space=0, col=NA)
	par(mar=c(6,0,1,1))
	barplot(hist4$counts, axes=FALSE, space=0, horiz=TRUE, col=NA)
	
	par(mar=c(6,6,1,1))
	plotCytogram(opp, 'ssc', 'green_lin', pop.def=pop.def)
	par(mar=c(0,6,1,1))
	barplot(hist6$counts, axes=FALSE, space=0, col=NA)
	par(mar=c(6,0,1,1))
	barplot(hist5$counts, axes=FALSE, space=0, horiz=TRUE, col=NA)
	
	par(mar=c(6,6,1,1))
	plot(1,1,bty='n', type='n', xaxt='n', yaxt='n',xlab=NA, ylab=NA, main=paste("filename:",basename(file)))
	addtable2plot(x=0.5,y=1, table=stat.table)
	
	
	par(def.par)

	dev.off()



table <- data.frame(cbind(stat.table, file=basename(file)))
summary.table <- rbind(summary.table, table)

}


write.csv(summary.table,file=paste(savepath,"summary_2.csv", sep=""), row.names=FALSE)