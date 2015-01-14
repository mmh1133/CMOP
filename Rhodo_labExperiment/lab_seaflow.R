
library(popcycle)
set.evt.location("/Volumes/seaflow/Rhodomonas_Sept2014")



evt.location<-"/Volumes/seaflow/Rhodomonas_Sept2014"

file.list <-list.files(evt.location, recursive=T,pattern='.evt')
file.list <- file.list[!grepl('.opp', file.list)]
file.list <- file.list[!grepl('.png', file.list)]
# getting only .evt files

#evt <- readSeaflow(paste(evt.location, file.list[500], sep='/'))

evt <- readSeaflow(paste(file.list[300]))

notch <- find.filter.notch(evt, notch=seq(0.1, 1.4, by=0.1),width=0.2, do.plot=TRUE) #finds notch  
plot.filter.cytogram(evt, notch=1, width=0.5) #plots filteration

opp<-filter.notch(evt, notch=notch, width=0.5)

plot.cytogram(opp, para.x='chl_small', para.y='pe')