library(flowCore)
library(flowDensity)

filelist <- list.files(path="Rhodo_labExperiment/cell_cycle/INFLUX_cell_cycle", 
                                pattern='.fcs', recursive=TRUE, full.names=TRUE)
results <- NULL  
for(file in filelist) {
        
        # file <- filelist[27]
        print(paste("processing file #",basename(file)))
            fcs <- read.FCS(file,transformation= F)
            data <- Subset(fcs, as.tiffical(exprs(fcs[,4]) > 40000) & as.logical(exprs(fcs[,4]) < 55000))

                res <- flowDensity(data, channels=c(2,7), position=c(FALSE,FALSE), upper=c(T,NA),sd.threshold=c(T,T),  n.sd=c(1,1))
                    
                    png(paste(basename(file),".png",sep=""),width=89, height=89, units='mm',res=600, pointsize=5) 
                        plot(data , res)
                    dev.off()
            
            RES <- cbind(filename=basename(file), percent_G1=round(res@proportion,3), sample_id=fcs@description[12])
            results <- rbind(results, RES)
}

write.csv(results, "Rhodo_labExperiment/cell_cycle/RHODOcell_cycle.csv", quote=F, row.names=F)










log <- read.csv("/Users/francois/CMOP/Rhodo_labExperiment/cell_cycle/RHODOcell_cycle_time.csv")
G1 <- read.csv("/Users/francois/CMOP/Rhodo_labExperiment/cell_cycle/RHODOcell_cycle.csv")

results <- merge(log, G1, by='filename')

raw.time <- paste(results$date, results$time)
results$date.time <- as.POSIXct(strptime(raw.time, "%m/%d/%y %H:%M:%S", tz='GMT'))
results <- results[order(results$date.time),]
# plot(tiff[,2], ylim=c(0,100))
time2 <- unique(results$date.time)


mean.f.G1 <- tapply(results$percent_G1, results$date.time, function(x) mean(x, na.rm=T))/100
sd.f.G1 <- tapply(results$percent_G1, results$date.time, function(x) sd(x, na.rm=T))/100

GR <- function(freq, t.div=4,n=12){
    mu <- 1/(n*t.div)*log(1+freq)
    return(mu)
}

div <- GR(1-mean.f.G1, t.div=4, n=1)

daily.div <- sum(div) / as.numeric(diff(range(time2)))
print(paste("Daily Growth Rate =",round(daily.div,2), "d-1"))


plot(time2, div, type='o', ylim=c(0,0.1))
points(time, Div.rate[,2], col=2)

 png(filename="/Users/francois/CMOP/Rhodo_labExperiment/modelandcc")
dev.off()
