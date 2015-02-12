library(flowCore)
#library(flowDensity)
library(flowViz)
library(flowStats)

home <- "/Users/francois/Documents/DATA/SeaFlow/CMOP/CMOP_git/"
filelist <- list.files(path=paste0(home,"Rhodo_labExperiment/cell_cycle/INFLUX_cell_cycle"), 
                                pattern='.fcs', recursive=TRUE, full.names=TRUE)

frames <- lapply(filelist, function(x) read.FCS(x,transformation=F))
frames <- flowSet(frames)
colnames(frames) <- c("pulse","fsc_small","fsc_big","red","mono","orange","green","wavelength","SSC","time")

g0 <- rectangleGate(filterId='nondebris', 'red'=c(20000,Inf))
all <- Subset(frames, g0)
dat <- gaussNorm(all, "green")$flowset

png("RHODO-Beads_cell_cycle.png",width=89, height=89*1.5, units='mm',res=600, pointsize=5) 
    densityplot(~green,dat) # RHODO + BEADS
dev.off()

png("RHODO_cell_cycle.png",width=89, height=89*1.5, units='mm',res=600, pointsize=5) 
    densityplot(~green,dat, xlim=c(0,35000)) # ONLY RHODO
dev.off()


lim.G1 <- 15000 # or 15000
lim.G2 <- 20000

g1 <- rectangleGate(filterId='g1', 'green'=c(5000,lim.G1))
G1 <- Subset(dat, g1)
densityplot(~green,G1) # G1
p.G1 <- fsApply(G1, nrow)

s <- rectangleGate(filterId='g2s', 'green'=c(lim.G1,lim.G2))
S <- Subset(dat, s)
densityplot(~green,S) # S
p.S <- fsApply(S, nrow)

g2 <- rectangleGate(filterId='s', 'green'=c(lim.G2,30000))
G2 <- Subset(dat, g2)
densityplot(~green,G2) # G2
p.G2 <- fsApply(G2, nrow)

tot <- p.G1 + p.G2 + p.S

stat <- data.frame(basename(filelist), cbind(p.G1/tot, p.S/tot, p.G2/tot))
colnames(stat) <- c("filename","G1","S","G2")

write.csv(stat, "RHODOcell_cycle_revised.csv", quote=F, row.names=F)


## TO check if normalization make sense...
# g1 <- rectangleGate(filterId='rhodo', 'red'=c(40000,55000))
# rhodo <- Subset(frames, g1)
# dat.r <- gaussNorm(rhodo, "green")$flowset
# densityplot(~green,dat.r, xlim=c(0,40000))

# g2 <- rectangleGate(filterId='beads', list("red"=c(0,30000), "orange"=c(35000,Inf)))
# beads <- Subset(frames, g2)
# dat.b<- gaussNorm(beads, "green")$flowset
# densityplot(~green,dat.b)




###########
### OLD ###
###########

# results <- NULL  
# for(file in filelist) {
        
#        # file <- filelist[7]
#         print(paste("processing file #",basename(file)))
#             fcs <- read.FCS(file,transformation= F)
#             plot(fcs[,c(2,4)])
            
#             beads <- Subset(fcs, as.logical(exprs(fcs[,4]) < 30000) & as.logical(exprs(fcs[,6]) > 35000))
#             med.beads <- summary(beads[,2])[3]

#             data <- Subset(fcs, as.logical(exprs(fcs[,4]) > 40000) & as.logical(exprs(fcs[,4]) < 55000))

#                 normTrans <- norm(transformId="Normalization", med.beads=med.beads)
#                 norm.data <- transform(data, SSC=SSC/med.beads, SSC)

#                 res <- flowDensity(norm.data , channels=c(2,7), position=c(FALSE,FALSE), upper=c(T,NA),sd.threshold=c(T,T),  n.sd=c(1,1))
                    
#                     png(paste(basename(file),".png",sep=""),width=89, height=89, units='mm',res=600, pointsize=5) 
#                         plot(data , res)
#                     dev.off()
            
#             RES <- cbind(filename=basename(file), percent_G1=round(res@proportion,3), sample_id=fcs@description[12])
#             results <- rbind(results, RES)
# }







home <- "/Users/francois/Documents/DATA/SeaFlow/CMOP/CMOP_git/"

log <- read.csv(paste0(home,"/Rhodo_labExperiment/cell_cycle/RHODOcell_cycle_time.csv"))
stat <- read.csv(paste0(home,"/Rhodo_labExperiment/cell_cycle/RHODOcell_cycle_revised.csv"))

results <- merge(log, stat, by='filename')

raw.time <- paste(results$date, results$time)
results$date.time <- as.POSIXct(strptime(raw.time, "%m/%d/%y %H:%M:%S", tz='GMT'))
results <- results[order(results$date.time),]

plot(results$date.time, results$G1, ylim=c(0,1))

time2 <- unique(results$date.time)

results <- results[-12,] ## remove file #25, obvious issue with instrument

mean.f.G1 <- tapply(results$G1, results$date.time, function(x) mean(x, na.rm=T))
sd.f.G1 <- tapply(results$G1, results$date.time, function(x) sd(x, na.rm=T))
mean.f.G2 <- tapply(results$G2, results$date.time, function(x) mean(x, na.rm=T))
sd.f.G2 <- tapply(results$G2, results$date.time, function(x) sd(x, na.rm=T))
mean.f.S <- tapply(results$S, results$date.time, function(x) mean(x, na.rm=T))
sd.f.S <- tapply(results$S, results$date.time, function(x) sd(x, na.rm=T))

plot(time2, mean.f.G1, type='o', ylim=c(0,1))
points(time2, mean.f.G2, col=2, type='o')
points(time2, mean.f.S, col=3, type='o')

## t.div defined as twice the time between peak of S and G2
t.div <- as.numeric(2*(time2[which(mean.f.G2 == max(mean.f.G2))] - time2[which(mean.f.S == max(mean.f.S))])) 

GR <- function(freqG2S, t.div=4,n=12){
    mu <- 1/(n*t.div)*log(1+freqG2S)
    return(mu)
}

div <- GR(100*(mean.f.G2+mean.f.S), t.div=t.div, n=length(mean.f.G1))
div.sd <- div*sqrt((sd.f.G2/(mean.f.G2*log(10)))^2+(sd.f.S/(mean.f.S*log(10)))^2+(sd.f.G1/(mean.f.G1*log(10)))^2)
div.se <- div.sd/sqrt(2)
table <- data.frame(cbind(time=time2, div=div, div.sd=div.sd, div.se=div.se, mean.f.G1, mean.f.S, mean.f.G2, sd.f.G1, sd.f.S, sd.f.G2))
write.csv(table, "RHODO_div-rate.csv", quote=F, row.names=T)

daily.div <- sum(div) / as.numeric(diff(range(time2)))
print(paste("Daily Growth Rate =",round(daily.div,2), "d-1"))



