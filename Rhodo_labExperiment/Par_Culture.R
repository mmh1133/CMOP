cruise <- "Crypto_TimeCourse_June2013"

ON <- "06:00:00"
OFF <- "22:00:00"
light <- 40

sds <- read.delim(paste("/Volumes/seaflow/",cruise, "/sds.tab",sep=""))
sds$time <- as.POSIXct(strptime(sds$time, "%Y-%m-%d %H:%M:%S"), tz="GMT")

date <- format(strptime(sds$time, format="%Y-%m-%d %H:%M:%S"), "%Y-%m-%d",tz="GMT")
time <- format(strptime(sds$time, format="%Y-%m-%d %H:%M:%S"), "%X",tz="GMT")
time.local <- as.POSIXct(strptime(sds$time, "%Y-%m-%d %H:%M:%S"), tz=" ") - 60*8*60 
Par <- data.frame(cbind(date, time))
Par$time.local<- as.POSIXct(time.local)

on <- paste(unique(date), rep(ON, length(unique(date))))
off <- paste(unique(date), rep(OFF, length(unique(date))))

sunrise <- as.POSIXct(strptime(on, "%Y-%m-%d %H:%M:%S"), tz=" ")
sunset <- as.POSIXct(strptime(off, "%Y-%m-%d %H:%M:%S"), tz=" ")

id.sunrise <- findInterval(sunrise, time.local)
id.sunset <- findInterval(sunset, time.local)

Par$par <- 0
Par$time <- sds$time
Par <- Par[,c("time", "par")]
for(i in 1:length(id.sunrise)) Par[id.sunrise[i]:id.sunset[i],'par']<- light

plot(Par$time, Par$par, type='o')

write.csv(Par, paste("/Volumes/ribalet/Cell_Division/",cruise,"/Par_",cruise, sep=""), quote=F, row.names=F)