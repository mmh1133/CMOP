
library(popcycle)
set.evt.location("/Volumes/seaflow/Rhodomonas_Sept2014")
set.project.location("~/Rhodo_lab")


evt.location<-"/Volumes/seaflow/Rhodomonas_Sept2014"

file.list <-list.files(evt.location, recursive=T,pattern='.evt')
file.list <- file.list[!grepl('.opp', file.list)]
file.list <- file.list[!grepl('.png', file.list)]
# getting only .evt files

#evt <- readSeaflow(paste(evt.location, file.list[500], sep='/'))

evt <- readSeaflow(paste(file.list[107]))

notch <- find.filter.notch(evt, notch=seq(0.1, 1.4, by=0.1),width=0.2, do.plot=TRUE) #finds notch  
plot.filter.cytogram(evt, notch=1, width=0.5) #plots filteration

opp<-filter.notch(evt, notch=1, width=0.5)

plot.cytogram(opp, para.x='fsc_small', para.y='chl_small')

png(filename="/Users/francois/CMOP/Rhodo_labExperiment/filter_400.png")
dev.off()

setFilterParams(notch=1, width=0.5)



# SELECT an OPP file
opp.list <- get.opp.list()
opp.name <- opp.list[280] # to select the opp file (e.g., the 10th opp file in the list)
#100

opp<-get.opp.by.file(opp.name)

plot.cytogram(opp, para.x='chl_small', para.y='pe')

#beads
setGateParams(opp, popname='beads', para.x='chl_small', para.y='pe')
#synecho
setGateParams(opp, popname='synecho', para.x='fsc_small', para.y='pe')
#pro
setGateParams(opp, popname='prochloro', para.x='fsc_small', para.y='chl_small')
#picoeukaryotes
setGateParams(opp, popname='picoeuk', para.x='fsc_small', para.y='chl_small')
#crypto
setGateParams(opp, popname='crypto', para.x='chl_small', para.y='pe')
#other
setGateParams(opp, popname='other', para.x='fsc_small', para.y='chl_small')


vct <- classify.opp(opp, ManualGating)
opp$pop <- vct
par(mfrow=c(1,2))
plot.vct.cytogram(opp, para.x='fsc_small', para.y='chl_small')
plot.vct.cytogram(opp, para.x='fsc_small', para.y='pe')
plot.vct.cytogram(opp, para.x='chl_small', para.y='pe')





set.evt.location("/Volumes/seaflow/Rhodomonas_Sept2014")
set.project.location("~/Rhodo_lab")
set.cruise.id("Rhodo_lab")

evt.list<-get.evt.list()
run.filter(evt.list)

evt.files <- get.evt.list()
evt.files.without.opp <- filter.evt.files.parallel(evt.files, notch=1, width=0.5, cores=1)

run.gating(opp.list)



stat <- get.stat.table()

time <- as.POSIXct(stat$time,format="%FT%T",tz='GMT')

# subseting files #
pre.crypto <- subset(stat, pop == 'crypto') 
pre.crypto <- pre.crypto[order(pre.crypto$time),] #orders by time
pre.crypto2 <- pre.crypto[60:308,] #just keep good days
id <- which(diff(pre.crypto2$time) > 4) #subset files that are too short
pre.crypto3 <- pre.crypto2[-id,] 
id2 <- which(pre.crypto3$flow_rate < 2400) #subset files that have low flow rate
crypto <- pre.crypto3[-id2,]


plot(crypto$time2, crypto$abundance,ylim=c(10,40))
plot(crypto$time2, crypto$fsc_small, ylim=c(300,500))


id.good.file <- na.rm(match(stat$file, crypto$file))
stat[id.good.file, "flag"] <- 0

unique(crypto$file)


stat$flag <- 1


savepath<-"Users/francois/CMOP/Rhodo_labexperiment"
write.delim(stat,file=paste(savepath,"stat.tab", sep="/"))



