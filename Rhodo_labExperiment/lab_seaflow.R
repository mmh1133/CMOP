#################################################################
###### Rhodomonas SeaFlow culture experiment data analysis ######
#################################################################

library(popcycle)
set.evt.location("/Volumes/seaflow/Rhodomonas_Sept2014")
set.project.location("~/Rhodo_lab")
set.cruise.id("Rhodo_lab")

evt.location<-"/Volumes/seaflow/Rhodomonas_Sept2014"

#####################
##### filtering #####
#####################


file.list <-list.files(evt.location, recursive=T,pattern='.evt')
file.list <- file.list[!grepl('.opp', file.list)]
file.list <- file.list[!grepl('.png', file.list)]
# getting only .evt files

#evt <- readSeaflow(paste(evt.location, file.list[500], sep='/'))

evt <- readSeaflow(paste(file.list[300]))

notch <- find.filter.notch(evt, notch=seq(0.1, 1.4, by=0.1),width=0.2, do.plot=TRUE) #finds notch  
plot.filter.cytogram(evt, notch=1, width=0.5) #plots filteration

opp<-filter.notch(evt, notch=1, width=0.5)

plot.cytogram(opp, para.x='fsc_small', para.y='chl_small')

png(filename="/Users/francois/CMOP/ASLO/figure_making/rough_plots.png")
dev.off()

setFilterParams(notch=1, width=0.5)


##################
##### gating #####
##################


# SELECT an OPP file
opp.list <- get.opp.list()
opp.name <- opp.list[280] # to select the opp file (e.g., the 10th opp file in the list)
#100

opp<-get.opp.by.file(opp.name)

plot.cytogram(opp, para.x='fsc_small', para.y='chl_small', xlab="forward scatter", ylab="chlorophyll", cex.lab=2)

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



#################################################
##### actually running filtering and gating #####
#################################################


set.evt.location("/Volumes/seaflow/Rhodomonas_Sept2014")
set.project.location("~/Rhodo_lab")
set.cruise.id("Rhodo_lab")

evt.list<-get.evt.list()
run.filter(evt.list)

evt.files <- get.evt.list()
evt.files.without.opp <- filter.evt.files.parallel(evt.files, notch=1, width=0.5, cores=1)

run.gating(opp.list)



#############################################################
##### looking at stats and making flagged stat.tab file #####
#############################################################


stat <- get.stat.table() #already odered by time

stat$time <- as.POSIXct(stat$time,format="%FT%T",tz='GMT')

# subseting files #
pre.crypto <- subset(stat, pop == 'crypto') 
pre.crypto2 <- subset(pre.crypto, time > as.POSIXct("2014-09-22 19:30:00") & time < as.POSIXct("2014-09-24 13:10:00")) #just keep good days
id <- which(diff(pre.crypto2$time) > 4) #subset files that are too short
pre.crypto3 <- pre.crypto2[-id,] 
id2 <- which(pre.crypto3$flow_rate < 2400) #subset files that have low flow rate
crypto <- pre.crypto3[-id2,]


plot(crypto$time, crypto$abundance,ylim=c(0,40))
plot(crypto$time, crypto$fsc_small, ylim=c(300,500))


# flagging files #
id.good.file <- match(stat$file, crypto$file)
id.good.file <- which(!is.na(id.good.file))
stat$flag <- 1
stat[id.good.file, "flag"] <- 0

#unique(crypto$file)

savepath<-"/Users/francois/CMOP/Rhodo_labExperiment"
write.delim(stat,file=paste(savepath,"flag_file.txt", sep="/"), row.names=F)






#weird.id <- which(crypto$abundance > 35)
#weird.id
#plot(crypto$time2, crypto$n_count)
#points(crypto$time2[weird.id], crypto$abundance[weird.id], col=2)
#diff(crypto[,"time2"])


###############################
##### making nice figures #####
###############################

stat <- get.stat.table() #already odered by time

stat$time <- as.POSIXct(stat$time,format="%FT%T",tz='GMT')

# subseting files #
pre.crypto <- subset(stat, pop == 'crypto') 
pre.crypto2 <- subset(pre.crypto, time > as.POSIXct("2014-09-22 19:30:00") & time < as.POSIXct("2014-09-24 13:10:00")) #just keep good days
id <- which(diff(pre.crypto2$time) > 4) #subset files that are too short
pre.crypto3 <- pre.crypto2[-id,] 
id2 <- which(pre.crypto3$flow_rate < 2400) #subset files that have low flow rate
crypto <- pre.crypto3[-id2,]


#light cycle: GMT 21:00-15:00
#dark cycle: GMT 15:00-21:00 

light1<- subset(crypto, time > as.POSIXct("2014-09-22 21:00:00") & time < as.POSIXct("2014-09-23 15:00:00"))
light2<- subset(crypto, time > as.POSIXct("2014-09-23 21:01:00") & time < as.POSIXct("2014-09-24 13:10:00"))

#plotting abundance over time with light/dark cycle 
plot(crypto$time, crypto$abundance,ylim=c(5,30), pch=16, xlab="time", ylab="abundance", main="Rhodomonas", cex.main=2, cex.lab=1.5)
points(light1$time, light1$abundance, col="gold", pch=16)
points(light2$time, light2$abundance, col="gold", pch=16)








