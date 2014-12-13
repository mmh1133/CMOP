 ##### CMOP 2013 #####


library(popcycle)
set.evt.location("/Volumes/seaflow/CMOP_6")
set.project.location("~/CMOP_2013_f2")

#evt.location<-"/Volumes/seaflow/CMOP_6"
#file.name <-get.latest.evt.with.day() #name of the latest evt file collected
file.list <-list.files(evt.location, recursive=T,pattern='.evt')
file.list <- file.list[!grepl('.opp', file.list)]
file.list <- file.list[!grepl('.png', file.list)]
#getting only .evt files

i<-3960
evt1 <- readSeaflow(paste(file.list[i])) #load evt file
evt2 <- readSeaflow(paste(evt.location, file.list[i+1], sep='/'))
evt3 <- readSeaflow(paste(evt.location, file.list[i+2], sep='/'))
evt <- rbind(evt1,evt2,evt3)
evt <- readSeaflow(paste(evt.location, file.list[100], sep='/')) 
#files tested(oct)- gating:3980, 2700, 2900,3000(for synecho), 3100(beads), 3500(beads), 3900, 500
#no beads- 3600, 1680, 1690
#opp1 stronger sig- 3900, 1500, 1660

#files tested(nov 4/7)- 2550(?), 2000(meh but no real beads- needed 0.9), 3500(beadsish- 0.9), 3900(maybe?), 3980(yay for crypt, no beads), 1080(bleh), 1680(bleh)
#"good" files- (0.8 and 0.5 for most)3960, 3950, 3965, 100("best.filter.notch" tells me 1), 500

## file numbers ##
#week 1: 1-1045
#week 2: 1046-2128
#week 3: 2127-3751
#week 4: 3752-4468

notch <- best.filter.notch(evt, notch=seq(0.1, 1.4, by=0.1),width=0.2, do.plot=TRUE) #finds notch  
plot.filter.cytogram(evt, notch=0.8, width=0.5) #plots filteration

##won't best.filter.notch- 3960
##bad graphics- 3550, 263, 3560, 260, 3540, 2575, 720, 710 (mostly high positive D2-D1)

png(filename="/Volumes/ceg-1/Maria/CMOP/filtering_round_2/256_168_f800_cytb.png")
dev.off()

opp <- filter.notch(evt, notch=0.8, width=0.5)
opp1 <- flowPhyto:::filter(evt, notch=0.8, width=0.5)
setFilterParams(notch=0.8, width=0.5) #saves filter parameters


evt$D1b <- evt$D1 + 0
plot.cytogram(evt[1:10000,],"D1b","D2")

# SELECT an OPP file
opp.list <- get.opp.list()
opp.name <- opp.list[100] # to select the opp file (e.g., the 10th opp file in the list)

opp<-get.opp.by.file(opp.name)

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

plot.cytogram(opp, para.x='fsc_small', para.y='pe')

vct <- classify.opp(opp, ManualGating)
opp$pop <- vct
par(mfrow=c(1,2))
plot.vct.cytogram(opp, para.x='fsc_small', para.y='chl_small')
plot.vct.cytogram(opp, para.x='fsc_small', para.y='pe')
plot.vct.cytogram(opp, para.x='chl_small', para.y='pe')


set.evt.location("/Volumes/seaflow/CMOP_6")
set.project.location("~/CMOP_2013_f2")
set.cruise.id("CMOP_6")

evt.files <- get.evt.list()
evt.files.without.opp <- filter.evt.files.parallel(evt.files, notch=0.8, width=0.5, cores=2)

run.gating("2013_253", "2013-09-10T16:49:35+0000", "2013_275", "2013-10-02T23:58:48+0000")
run.gating(opp.list)

run.filter.v1('/Volumes/seaflow/CMOP_6')


stat <- get.stat.table() # to load the aggregate statistics
plot.map(stat, pop='crypto', param='abundance') 
plot.time(stat, pop='crypto', param='abundance')



for(opp.file in opp.list) {
	tryCatch({
		print("updating stat")
		insert.stats.for.file(opp.file, db = db.name)
		}, error = function(e) {
            print(paste("Encountered error with file", opp.file))
        }, finally = {
            print(paste("Finished with file", opp.file))
        })
}













