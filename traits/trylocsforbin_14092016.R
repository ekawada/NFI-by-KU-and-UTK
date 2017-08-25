#Create binned dataset for SLA and SSD traits
#C. Chisholm, 28/08/2016

library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)

# set wd as nfi

#read in trait data
trytraits <- read.csv('./data/trait data/trytraits.csv')
extras <- trytraits %>% filter(DataName=="Wood density; stem specific density") %>% filter(AccSpeciesName %in% c("Alnus incana","Picea abies","Sorbus aucuparia","Betula pubescens"))
trytraits <- trytraits[complete.cases(trytraits[7:8]),] #removes NAs

#get bioclim data
require(raster)
BClim_temp <- getData("worldclim", var="tmean", res=2.5)
BClim_precip <- getData("worldclim", var="prec", res=2.5)

#extract data
point <- SpatialPoints(trytraits[,c("longitude","latitude")], proj4string=CRS('+proj=longlat +datum=WGS84'))
traittemp <- extract(BClim_temp, point) # for the subsampled presence points
traitprec <- extract(BClim_precip, point) # for the subsampled presence points
traitenv<- data.frame(temp=rowMeans(traittemp/10), precip=rowSums(traitprec*5)) #worldclim are multipled by 10, and precip data is sum of five years in mods

#combine
alldat <- cbind(trytraits, traitenv)
alldat %>% group_by(DataName, AccSpeciesName) %>% summarise(n=n())
#only a few graor, dunbjork and osp. mostly furu and eik/gran
#only SSD for eik, furu, gran, osp and rogn

#add in an average for alnus and picea
finaldat <- alldat %>% bind_rows(., extras)
#write data
write.csv(finaldat, file='./data/trait data/traitclim.csv')

