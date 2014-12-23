library(flowCore)
library(flowDensity)

filelist <- list.files(path="Rhodo_labExperiment/cell_cycle/INFLUX_cell_cycle", 
                                pattern='.fcs', recursive=TRUE, full.names=TRUE)
results <- NULL  
for(file in filelist) {
        
        # file <- filelist[27]
        print(paste("processing file #",basename(file)))
            fcs <- read.FCS(file,transformation= F)
            data <- Subset(fcs, as.logical(exprs(fcs[,4]) > 40000) & as.logical(exprs(fcs[,4]) < 55000))

                res <- flowDensity(data, channels=c(2,7), position=c(FALSE,FALSE), upper=c(T,NA),sd.threshold=c(T,T),  n.sd=c(1,1))
                    
                    png(paste(basename(file),".png",sep=""),width=89, height=89, units='mm',res=600, pointsize=5) 
                        plot(data , res)
                    dev.off()
            
            RES <- cbind(filename=basename(file), percent_G1=round(res@proportion,3), sample_id=fcs@description[12])
            results <- rbind(results, RES)
}

write.csv(results, "Rhodo_labExperiment/cell_cycle/RHODOcell_cycle.csv", quote=F, row.names=F)



# results <- read.csv("RHODOcell_cycle.csv")
# results[order(results$sample_id),]
# plot(results[,2], ylim=c(0,100))