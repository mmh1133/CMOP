cruise <- "Rhodo_lab"

ON <- "21:00:00"
OFF <- "15:00:00"
light <- 40

sds <- read.delim(paste("/Users/francois/CMOP/Rhodo_labExperiment/sds.tab",sep=""))
sds$time <- as.POSIXct(strptime(sds$time, "%Y-%m-%d %H:%M:%S"), tz="GMT")

#sfl<-read.delim(paste("/Users/francois/CMOP/Rhodo_labExperiment/cruise.sfl", sep=""))
#sfl$time <- as.POSIXct(strptime(sfl$time, "%Y-%m-%d %H:%M:%S"), tz="GMT")

date <- format(strptime(sds$time, format="%Y-%m-%d %H:%M:%S"), "%Y-%m-%d",tz="GMT")
time <- format(strptime(sds$time, format="%Y-%m-%d %H:%M:%S"), "%X",tz="GMT")
#time.local <- as.POSIXct(strptime(sds$time, "%Y-%m-%d %H:%M:%S"), tz=" ") - 60*8*60 
Par <- data.frame(cbind(date, time))
#Par$time.local<- as.POSIXct(time.local)

on <- paste(unique(date), rep(ON, length(unique(date))))
off <- paste(unique(date), rep(OFF, length(unique(date))))

sunrise <- as.POSIXct(strptime(on, "%Y-%m-%d %H:%M:%S"), tz="GMT")
sunset <- as.POSIXct(strptime(off, "%Y-%m-%d %H:%M:%S"), tz="GMT")

id.sunrise <- findInterval(sunrise, sds$time)
id.sunset <- findInterval(sunset, sds$time)

Par$par <- light
Par$time <- sds$time
Par <- Par[,c("time", "par")]
for(i in 1:length(id.sunrise)) Par[id.sunrise[i]:id.sunset[i],'par']<- 0

plot(Par$time, Par$par, type='o')

write.csv(Par, paste("/Users/francois/CMOP/Rhodo_labExperiment/Par_",cruise, sep=""), quote=F, row.names=F)