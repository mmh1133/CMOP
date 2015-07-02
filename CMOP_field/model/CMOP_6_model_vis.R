## MODEL
home <- "/Users/francois/CMOP/CMOP_field/model"
#home <- "/Users/francois/Documents/DATA/SeaFlow/CMOP/CMOP_git/"

cruise <- "CMOP_6"
location.model <- paste0(home, "")
phyto <- "crypto"
out.dir <- paste0(home, "")
cat <- 57 # 2^6 # number of size bin
n.day <- 18 #number of days in dataset, need for below because it did not find in loading model, may be a bug somewhere?

library(rgl)
library(ggplot2)


all.filelist <- list.files(paste0(location.model,"/"),pattern=paste0("NEW",phyto,"_modelHD_growth_",cruise,"_Ncat",cat))
filelist <- all.filelist[grep(pattern=paste(phyto), all.filelist)]

n <- c <- 1
Conc.all <- N.proj.all <- V.hist.all <- div.rate <- para.all <- Col <- NULL


for(file in filelist){
    #file <- filelist[1]
    load(paste(location.model,file, sep="/"))
    print(file)
        dim <- conc.proj.all <- n.proj.all <- v.hist.all <- dr.all <- p.all <- NULL
            for(i in 2:dim(model)[2]){
                try(n.proj <- model[4,i][[1]])
                n.proj.all <- cbind(n.proj.all, n.proj)         
                            
                conc.proj <- cbind(as.numeric(colnames(n.proj)), as.numeric(colSums(n.proj)))
                conc.proj.all <- rbind(conc.proj.all, conc.proj)
                
                try(dr <- model[2,i][[1]])
                h.dr <- cbind(as.numeric(colnames(dr)), as.numeric(dr))
                dr.all <- rbind(dr.all, h.dr)
                
               try(v.proj <- model[3,i][[1]])   
                v.hist.all <- cbind(v.hist.all, v.proj)         

               try(para <- model[1,i][[1]])
                param <- cbind(time=as.numeric(colnames(n.proj)), para)
                p.all <- rbind(p.all, param)
            }
        
    
        div.rate <- rbind(div.rate, dr.all)
        N.proj.all <- cbind(N.proj.all, n.proj.all)
        Conc.all <- rbind(Conc.all, conc.proj.all)
        V.hist.all <- cbind(V.hist.all, v.hist.all)
        para.all <- rbind(para.all, p.all)

        col <- rep(c, nrow(dr.all))
        Col <- c(Col,col)

        layout(matrix(c(1,1,2:7),4,2, byrow=T)) 
        par(pty='m')    
        plot(div.rate[,1], div.rate[,2], ylab="Div Rate", xlab="time",col=Col)
            #abline(v=night$UNIXtime,col='lightgrey');points(div.rate,col=Col)
            legend("topleft",legend=1:c, col=1:c, ncol=c, pch=1)
        plot(para.all[,"time"], para.all[,"gmax"], ylab="gmax", xlab="time",col = Col)
        plot(para.all[,"time"], para.all[,"dmax"],ylab="dmax", xlab="time",col = Col)
        plot(para.all[,"time"], para.all[,"a"],ylab="a", xlab="time",col = Col)
        plot(para.all[,"time"], para.all[,"b"],ylab="b", xlab="time",col = Col)
        plot(para.all[,"time"], para.all[,"E_star"],ylab="E_star", xlab="time",col = Col)
        plot(para.all[,"time"], para.all[,"resnorm"],ylab="resnorm", xlab="time",col = Col)

        # 
        names(para.all) <- c("time","gmax","a","b","E_star","dmax","resnorm")
   
c <- c + 1
}

### ORDERING DATA by TIME

    Div.rate <- div.rate[order(div.rate[,1]),]
    Nproj <- N.proj.all[,order(as.numeric(colnames(N.proj.all)))]
    Conc.proj <- Conc.all[order(Conc.all[,1]),]
    Vproj <- V.hist.all[,order(as.numeric(colnames(V.hist.all)))]
    Para.all <- para.all[order(para.all[,"time"]),]
        
  ### VISUALIZATION    
	jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow",	"#FF7F00", "red", "#7F0000"))  
    para <- Vproj
    percentile <- cut(unlist(para), 100); plot3d(rep(1:dim(para)[1], dim(para)[2]), rep(1:dim(para)[2], each=dim(para)[1]), z=matrix(para), col=jet.colors(100)[percentile], type='l', lwd=3, xlab="size class", ylab="time", zlab="Frequency")


#Average estimates for division rate from same time point and make new table with Ave, SE and number of estimates
Div.rate.ave <- as.data.frame(matrix(data= NA, nrow=length(na.omit(unique(Div.rate[,1]))) , ncol=5))
colnames(Div.rate.ave)=c("time", "div.ave", "div.sd", "div.se", "div.n")
Div.rate.ave$time <- na.omit(unique(Div.rate[,1]))
 for(n in 1:length(Div.rate.ave$time)){
 	sum = sum(na.omit(Div.rate[,1] == Div.rate.ave$time[n]))
 	index <- which(Div.rate[,1] == Div.rate.ave$time[n])
 	if(sum <2){
 		Div.rate.ave$div.ave[n] <-Div.rate[index,2]
 		Div.rate.ave$div.sd[n] <- NA
 		Div.rate.ave$div.n[n] <- 1
		Div.rate.ave$div.se[n] <- NA
		next
 	}
  	sub <- Div.rate[index,]
 	Div.rate.ave$div.ave[n] <- mean(na.omit(sub[,2]))
 	Div.rate.ave$div.sd[n] <- sd(na.omit(sub[,2]))
 	Div.rate.ave$div.n[n] <- sum(!is.na(sub[,2]))
	Div.rate.ave$div.se[n] <- Div.rate.ave$div.sd[n]/sqrt(Div.rate.ave$div.n[n])
 }

plotCI(as.POSIXct(Div.rate.ave$time, origin="1970-01-01" ,tz='GMT'), Div.rate.ave$div.ave, uiw=Div.rate.ave$div.se)

write.csv(Div.rate.ave, file=paste0(out.dir,"newmodel_output-V2.csv"), row.names=F)





yay <- read.csv("/Users/francois/CMOP/CMOP_field/model/crypto_HD_CMOP_6.binned.csv")

yay$daily.GRmean <- rollapply(data=yay$h.dr.mean, width=24, FUN=mean, na.rm=T, fill=NA)*24
yay$daily.GRsd <- rollapply(data=yay$h.dr.sd, width=24, FUN=mean, na.rm=T, fill=NA)*24


plotCI(as.POSIXct(yay$h.time, origin="1970-01-01", tz='GMT'), yay$daily.GRmean, uiw= yay$daily.GRsd, sfrac=0)



