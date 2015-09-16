library(flowCore)
library(splancs) 
library(popcycle)
library(cluster)
library(plotrix)
library(caroline)
.rainbow.cols <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow","#FF7F00", "red", "#7F0000"))


#################
## BATCH FILES ##
#################


path <- paste("/Users/francois/Documents/DATA/SeaFlow/CMOP/CMOP_git/CMOP_INFLUX_Sept2013")
savepath <- paste("/Users/francois/Documents/DATA/SeaFlow/CMOP/CMOP_git/CMOP_INFLUX_Sept2013/results")
file.list <- list.files(path, pattern = ".fcs", recursive=T, full.names=TRUE)

#file.list <- file <- file.list[42]

summary.table <- NULL



for (file in file.list) {
	print(paste("processing file:",file))


###############
## read FCS ###
###############

fcs <- read.FCS(file, transformation= FALSE)
opp <- tab2df(exprs(fcs)) 

if(ncol(opp) == 7) names(opp) <- c("pulse_width", "fsc_small",  "chl_small", "pe", "green","ssc","time")
if(ncol(opp) == 11) names(opp) <- c("pulse_width", "fsc_small", "fsc_big", "chl_small", "monochrome","pe", "green","blue","wavelength","ssc","time")


opp$pop <- 0

##############
### GATING ###
##############

### NOISE & BEADS & CRYPTO 
x <- subset(opp, pop==0)

# plot(x[,"chl_small"], x[,"pe"], pch=16, cex=0.4, col = densCols(x[,"chl_small"], x[,"pe"], colramp = .rainbow.cols), 
# 		xlim=c(0,2^16), ylim=c(0,2^16), main="Gating NOISE & BEADS")
# # print("Gating Beads")
# # poly.beads <- getpoly(quiet=TRUE)
# # print("Gating crypto")
# # poly.crypto <- getpoly(quiet=TRUE)


# polygon(poly.beads, lwd=2,border=2, col=NA)
# polygon(poly.crypto, lwd=2,border=2, col=NA)

beads <- subset(x,inout(x[,c("chl_small","pe")],poly=poly.beads, bound=TRUE, quiet=TRUE))
opp[row.names(beads),'pop'] <- "beads"
crypto <- subset(x,inout(x[,c("chl_small","pe")],poly=poly.crypto, bound=TRUE, quiet=TRUE))
opp[row.names(crypto),'pop'] <- "crypto"

png(paste(savepath, "/", basename(file),".png", sep=""),width=9, height=12, unit='in', res=100)
par(mfrow=c(2,2))
plot.vct.cytogram(opp, "fsc_small", "chl_small",main=basename(file))
plot.vct.cytogram(opp, "fsc_small", "pe")
plot.vct.cytogram(opp, "chl_small", "pe")
dev.off()

###################
### SAVE .VCT ###
###################

write.table(opp$pop, paste(savepath,"/",basename(file),'-class.vct',sep=""), row.names=FALSE, col.names='pop', quote=FALSE)




###############
### SUMMARY ###
###############

stat.table <- NULL
for(i in c("beads", "crypto")){
#print(i)
p <- subset(opp, pop == i)
n <- nrow(p)
if(n ==0) {
fsc <- 0
chl <- 0
	}else{
fsc <- round(median(p$fsc_small))
chl <- round(median(p$chl_small))
pe <- round(median(p$pe))
}
var <- cbind(i,n,fsc,chl,pe)
stat.table <- rbind(stat.table, var)
}


table <- data.frame(cbind(stat.table, file=basename(file)))
summary.table <- rbind(summary.table, table)
write.csv(summary.table,file=paste(savepath,"summary_V2-FR.csv", sep=""), row.names=FALSE)
}

}


write.csv(summary.table,file=paste(savepath,"summary_2.csv", sep=""), row.names=FALSE)