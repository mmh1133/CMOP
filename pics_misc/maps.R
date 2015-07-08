#####################################
## various methods for making maps ##
#####################################

library(maps)
library(mapdata)
library(maptools)
library(scales)
library(mapproj)

setwd("/Users/mariaham/Dropbox/code_share/seaflow")

#### using maps package ####

#making a map of canada+usa
map("worldHires", "Canada", xlim=c(-140,-110), ylim=c(48,64), col="lightcyan", fill=T)
map("worldHires","usa", xlim=c(-140,-110), ylim=c(48,64), col="thistle2", fill=T, add=T)  

#changing the projection
map(database="world", ylim=c(45,90), xlim=c(-160,-50), col="lightcyan", fill=T, projection="gilbert", orientation=c(90,0,225))
lon<-c(-72,-66,-107, -154)
lat<-c(81.7,64.6,68.3,60)
coord<-mapproject(lon, lat, proj="gilbert", orientation=c(90,0,225))
points(coord, pch=20, cex=1.2, col="red") #adding some points



#### using google maps ####
library(RgoogleMaps)
lat<-c(43,45)
lon<-c(-123,-122)
center=c(mean(lat), mean(lon))
zoom<-4
terrmap<-GetMap(center=center, zoom=zoom, maptype="satellite") 




##################
## transect map ##
##################

mega_ID_table<-read.csv("mega_ID.csv")
mega_ID_data_frame<-as.data.frame(mega_ID_table)

mega_ID_new<-mega_ID_data_frame[-c(4),]

#using maps package
map(database="world", ylim=c(-50,20), xlim=c(-90,-20), col="paleturquoise", fill=T, )
lon<-c(-56.1667,-44.99850,-38.52550,-33.01333,-28.50200,-41.26383,-55.30267,-59.5300)
lat<-c(-34.8667,-37.99767,-28.23550,-22.49367,-2.70000,5.92100,9.70450,13.1594)
points(lon, lat, pch=20, cex=1.2, col="red")
lines(lon,lat, col="red")
text(x=lon, y=lat, labels=c(NA,2,5,7,15,19,23,NA), pos=2) 
map.scale(ratio=F, metric=T, x=-35.5, relwidth=.10, cex=.7) #scale
title(main="Map of Transect Megastations")
abline(a=0, b=0, lty=2) #equator






