library(flowCore)
#library(flowDensity)
library(flowViz)
library(flowStats)


norm <- function(transformId, med.beads){
    t <- new("transform", .Data=function(x) x/med.beads)
    t@transformationId = transformId
    t
}



home <- "/Users/francois/Documents/DATA/SeaFlow/CMOP/CMOP_git/"
filelist <- list.files(path=paste0(home,"Rhodo_labExperiment/cell_cycle/INFLUX_cell_cycle"), 
                                pattern='.fcs', recursive=TRUE, full.names=TRUE)

frames <- lapply(filelist, function(x) read.FCS(x,transformation=F))
frames <- flowSet(frames)
colnames(frames) <- c("pulse","fsc_small","fsc_big","red","mono","orange","green","wavelength","SSC","time")

g0 <- rectangleGate(filterId='nondebris', 'red'=c(20000,Inf))
all <- Subset(frames, g0)
dat <- gaussNorm(all, "green")$flowset
densityplot(~green,dat) # RHODO + BEADS
densityplot(~green,dat, xlim=c(0,30000)) # ONLY RHODO


g1 <- rectangleGate(filterId='g1', 'green'=c(0,12500))
G1 <- Subset(dat, g1)
densityplot(~green,G1) # G1
g2s <- rectangleGate(filterId='g2s', 'green'=c(12500,35000))
G2S <- Subset(dat, g2s)
densityplot(~green,G2S) # G2
s <- rectangleGate(filterId='s', 'green'=c(0,17500))
S <- Subset(G2S, s)
densityplot(~green,S) # G2


### GET THE STATS...
p.G1 <- summary(filter(dat, g1))
p.G2
p.S




## TO check if normalization make sense...
# g1 <- rectangleGate(filterId='rhodo', 'red'=c(40000,55000))
# rhodo <- Subset(frames, g1)
# dat.r <- gaussNorm(rhodo, "green")$flowset
# densityplot(~green,dat.r, xlim=c(0,40000))

# g2 <- rectangleGate(filterId='beads', list("red"=c(0,30000), "orange"=c(35000,Inf)))
# beads <- Subset(frames, g2)
# dat.b<- gaussNorm(beads, "green")$flowset
# densityplot(~green,dat.b)










results <- NULL  
for(file in filelist) {
        
       # file <- filelist[7]
        print(paste("processing file #",basename(file)))
            fcs <- read.FCS(file,transformation= F)
            plot(fcs[,c(2,4)])
            
            beads <- Subset(fcs, as.logical(exprs(fcs[,4]) < 30000) & as.logical(exprs(fcs[,6]) > 35000))
            med.beads <- summary(beads[,2])[3]

            data <- Subset(fcs, as.logical(exprs(fcs[,4]) > 40000) & as.logical(exprs(fcs[,4]) < 55000))

                normTrans <- norm(transformId="Normalization", med.beads=med.beads)
                norm.data <- transform(data, SSC=SSC/med.beads, SSC)

                res <- flowDensity(norm.data , channels=c(2,7), position=c(FALSE,FALSE), upper=c(T,NA),sd.threshold=c(T,T),  n.sd=c(1,1))
                    
                    png(paste(basename(file),".png",sep=""),width=89, height=89, units='mm',res=600, pointsize=5) 
                        plot(data , res)
                    dev.off()
            
            RES <- cbind(filename=basename(file), percent_G1=round(res@proportion,3), sample_id=fcs@description[12])
            results <- rbind(results, RES)
}

write.csv(results, "Rhodo_labExperiment/cell_cycle/RHODOcell_cycle.csv", quote=F, row.names=F)









home <- "/Users/francois/Documents/DATA/SeaFlow/CMOP/CMOP_git/"

log <- read.csv(paste0(home,"/Rhodo_labExperiment/cell_cycle/RHODOcell_cycle_time.csv"))
G1 <- read.csv(paste0(home,"/Rhodo_labExperiment/cell_cycle/RHODOcell_cycle.csv"))

results <- merge(log, G1, by='filename')

raw.time <- paste(results$date, results$time)
results$date.time <- as.POSIXct(strptime(raw.time, "%m/%d/%y %H:%M:%S", tz='GMT'))
results <- results[order(results$date.time),]
# plot(tiff[,2], ylim=c(0,100))
time2 <- unique(results$date.time)


mean.f.G1 <- tapply(results$percent_G1, results$date.time, function(x) mean(x, na.rm=T))/100
sd.f.G1 <- tapply(results$percent_G1, results$date.time, function(x) sd(x, na.rm=T))/100

GR <- function(freq, t.div=4,n=15){
    mu <- 1/(n*t.div)*log(1+freq)
    return(mu)
}

div <- GR(1-mean.f.G1, t.div=8, n=1)

daily.div <- sum(div) / as.numeric(diff(range(time2)))
print(paste("Daily Growth Rate =",round(daily.div,2), "d-1"))


plot(time2, div, type='o', ylim=c(0,0.1))
points(time, Div.rate[,2], col=2)

 png(filename="/Users/francois/CMOP/Rhodo_labExperiment/modelandcc")
dev.off()
