# Modifications to model and to input data
# Originally created March 23, 2016
# Last modified August 26, 2016 (attempt to create 9 bins, 3x3 for temp x precip)

library(plyr)
library(dplyr)
tru <- read.csv('./Data/tree data/treused.csv')
areaxdist <- read.csv('./Data/tree data/areaxdist2_corrected.csv')
# Read sites where climate data are missing.
dataMissing <- read.csv('./Data/tree data/dataMissing.csv')

# 1. reload climate data and growth rate data and convert them both to five-year values. That way we will have the total climate and total basal area increment over the period between measurements.

tru$ba <- with(tru, pi * ((dbh/10)/2)^2)

ba.to5 <- function(ba1, ba2, years){
  rate <-  (ba2 / ba1 )^(1/years) - 1
  ba5 <- ba1 * (1 + rate)^5
  return(ba5)
}

ba.to.x <- function(ba1, ba2, years, x){
  rate <-  (ba2 / ba1 )^(1/years) - 1
  bax <- ba1 * (1 + rate)^x
  return(bax)
}

# Re run growth rate calculation to make into a long vector.
growthrate <- dlply(tru, .(newtreeID), function(x) {
  x <- x[order(x$year), ]
  diff_ba <- diff(x$ba)
  ba_begin <- x$ba[-nrow(x)]
  ba_end <- x$ba[-1]
  diff_year <- diff(x$year)
  ba_1 <- ba.to.x(ba_begin, ba_end, diff_year, 1)
  list(treeID=x$newtreeID[1], grate=ba_1 - ba_begin, year=x$year[-1])
},
.progress='text')

growthrate_longform <- do.call('rbind', lapply(growthrate, function(x) ifelse(length(x$grate) > 0, as.data.frame(x), data.frame(treeID=integer(0),grate=numeric(0),year=integer(0)))))
growthrate2 <- lapply(growthrate, do.call, what='cbind')
growthrate2 <- Filter(function(x) ncol(x) == 3, growthrate2)
growthrate_longform <- do.call('rbind', growthrate2)

# Get rid of anomalously large growth rate values.
#growthrate_longform <- growthrate_longform[growthrate_longform[,2] < 20 & growthrate_longform[,2] > -20, ]
#dimnames(growthrate_longform)[[2]] <- c('newtreeID','growthrate','year')

growthrate_longform <- as.data.frame(growthrate_longform)
names(growthrate_longform) <- c('newtreeID', 'bainc', 'year')

# Load new climate data
load('./Data/nfi climate data/klima2016-01-05.RData')

# Convert new climate data to the proper yearly GDD and precipitation units.
library(plyr)
days <- c(31,28.25,31,30,31,30,31,31,30,31,30,31)
gddprecip_new <- ddply(klima, .(flateid, aar), function(x) {
  precip <- sum(x$nedb.abs)
  degrees <- x$temp.abs - 10
  degrees[degrees < 0] <- 0
  gdd <- sum(degrees * days)
  data.frame(gdd=gdd,precip=precip)
}, .progress = 'text')
names(gddprecip_new) <- c('plotID','year','gddnew','precipnew')

tempave <- ddply(klima, .(flateid, aar), function(x) {
	data.frame(temp_ave = mean(x$temp.abs))
}, .progress ='text')

### use tempave for the bins.

#### here read in trait PCA data
traits <- read.csv('./Data/trait data/scores22Nov.csv', row.names=1)
# add an "average" value for the 8th category, the miscellaneous species.
traits[8,] <- 0
# add the new species code.
traits$editedspnum <- c(3,5,2,1,6,4,7,8)
traits <- traits[order(traits$editedspnum), ]
# standardize traits
traits_stan <- sapply(traits, function(x) (x - mean(x))/sd(x))


##### here add the bins
# For each year, now get total GDD and total Precip from that year and the previous 4. Add them together.
gdd_sum_byyear <- dlply(gddprecip_new,.(plotID),function(x) {
  gddsum <- numeric(nrow(x)-4)
  for (i in 5:nrow(x)) gddsum[i-4] <- sum(x$gddnew[(i-4):i])
  data.frame(plotID=x$plotID[1], year=x$year[5:nrow(x)], gddsum=gddsum)
}, .progress='text')
precip_sum_byyear <- dlply(gddprecip_new,.(plotID),function(x) {
  precipsum <- numeric(nrow(x)-4)
  for (i in 5:nrow(x)) precipsum[i-4] <- sum(x$precipnew[(i-4):i]) #changed this to average across years, otherwise odd for binning
  data.frame(plotID=x$plotID[1], year=x$year[5:nrow(x)], precipsum=precipsum)
}, .progress='text')
temp_ave_byyear <- dlply(tempave,.(flateid),function(x) {
  avetemp <- numeric(nrow(x)-4)
  for (i in 5:nrow(x)) avetemp[i-4] <- mean(x$temp_ave[(i-4):i])
  data.frame(plotID=x$flateid[1], year=x$aar[5:nrow(x)], avetemp=avetemp)
}, .progress='text')

gdd_sum_byyear <- do.call('rbind', gdd_sum_byyear)
precip_sum_byyear <- do.call('rbind', precip_sum_byyear)
temp_ave_byyear <- do.call('rbind', temp_ave_byyear)


# 2. Stratify the input data into 3 or 4 bins based on mean annual temp and precip (to match with traits).
# 26 Aug 2016: Changed to 3x3 per.

temp_means <- ddply(temp_ave_byyear, .(plotID), summarize, temp=mean(avetemp, na.rm=T)) #had to remove NAS here. Maybe remove NAs in climatemissing instead?
temp_means$bin <- Hmisc::cut2(temp_means$temp, g=3)
temp_means$bint <- factor(temp_means$bin, labels=1:3)
temp_grps <- temp_means[,c('plotID','temp','bint')]

precip_means <- ddply(precip_sum_byyear, .(plotID), summarize, precip=mean(precipsum, na.rm=T)) #had to remove NAS here. Maybe remove NAs in climatemissing instead?
precip_means$bin <- Hmisc::cut2(precip_means$precip, g=3)
precip_means$binp <- factor(precip_means$bin, labels=4:6)
precip_grps <- precip_means[,c('plotID','precip','binp')]

# create bins of both labels
clim_grps <- left_join(temp_grps, precip_grps) %>% mutate(bingrp = paste(bint, binp))
grp_levels <- sort(unique(clim_grps$bingrp))
clim_grps$bingrp <- factor(clim_grps$bingrp , levels = grp_levels, labels=1:9)
# level order is: low temp low precip, low temp mid precip, etc etc
clim_grps <- clim_grps[,c('plotID','bingrp')]

allstandata <- cbind(tru, areaxdist)
allstandata <- merge(allstandata, as.data.frame(growthrate_longform), all.x=TRUE) # Adds growth rate.
allstandata <- merge(allstandata, gddprecip_new, all.x=TRUE)
#allstandata <- merge(allstandata, gdd_sum_byyear, all.x=TRUE) # Adds growing degreedays (5yr sums).
#allstandata <- merge(allstandata, precip_sum_byyear, all.x=TRUE) # Adds yearly precip (5yr sums).
allstandata <- merge(allstandata, clim_grps, all.x=TRUE) # Adds clim binning (1=T1P1, 2=T1P2, 3=T1P3, 4=T2P1, 5=T2P2, 6=T2P3, 7=T3P1, 8=T3P2, 9=T3P3)
#allstandata <- merge(allstandata, traits[,c(1,2,6)], all.x=TRUE) # Adds PC1 and PC2 for traits.

allstandata <- allstandata[, c("plotID", "plotnum", "year", "newtreeID", "dbh", "editedspnum", "V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "ba", "bainc", "gddnew", "precipnew", "bingrp")]

# get rid of NAs in data.
allstandata <- allstandata[complete.cases(allstandata), ]


############################## updated up to this point.

#### here read in climate data as well.

# Load new climate data
load('./Data/nfi climate data/klima2016-01-05.RData')

# Convert new climate data to the proper yearly GDD and precipitation units.
library(plyr)
days <- c(31,28.25,31,30,31,30,31,31,30,31,30,31)
gddprecip_new <- ddply(klima, .(flateid, aar), function(x) {
  precip <- sum(x$nedb.abs)
  degrees <- x$temp.abs - 10
  degrees[degrees < 0] <- 0
  gdd <- sum(degrees * days)
  data.frame(gdd=gdd,precip=precip)
}, .progress = 'text')
names(gddprecip_new) <- c('plotID','year','gddnew','precipnew')

source('./Code/processing climate data/gddprecip_nfi.r') # takes a while

# For each year, now get total GDD and total Precip from that year and the previous 4. Add them together.
gdd_sum_byyear <- dlply(gdd_byyear,.(plotID,plotnum),function(x) {
  gddsum <- numeric(nrow(x)-4)
  for (i in 5:nrow(x)) gddsum[i-4] <- sum(x$gdd[(i-4):i])
  data.frame(plotID=x$plotID[1], plotnum=x$plotnum[1], year=x$year[5:nrow(x)], gddsum=gddsum)
}, .progress='text')
precip_sum_byyear <- dlply(totalprecip_byyear,.(plotID,plotnum),function(x) {
  precipsum <- numeric(nrow(x)-4)
  for (i in 5:nrow(x)) precipsum[i-4] <- sum(x$precip[(i-4):i]) #changed this to average across years, otherwise odd for binning
  data.frame(plotID=x$plotID[1], plotnum=x$plotnum[1], year=x$year[5:nrow(x)], precipsum=precipsum)
}, .progress='text')
temp_ave_byyear <- dlply(avetemp_byyear,.(plotID,plotnum),function(x) {
  avetemp <- numeric(nrow(x)-4)
  for (i in 5:nrow(x)) avetemp[i-4] <- mean(x$temp[(i-4):i])
  data.frame(plotID=x$plotID[1], plotnum=x$plotnum[1], year=x$year[5:nrow(x)], avetemp=avetemp)
}, .progress='text')

gdd_sum_byyear <- do.call('rbind', gdd_sum_byyear)
precip_sum_byyear <- do.call('rbind', precip_sum_byyear)
temp_ave_byyear <- do.call('rbind', temp_ave_byyear)

# 2. Stratify the input data into 3 or 4 bins based on mean annual temp and precip (to match with traits).
# 26 Aug 2016: Changed to 3x3 per.

temp_means <- ddply(temp_ave_byyear, .(plotID,plotnum), summarize, temp=mean(avetemp, na.rm=T)) #had to remove NAS here. Maybe remove NAs in climatemissing instead?
temp_means$bin <- Hmisc::cut2(temp_means$temp, g=3)
temp_means$bint <- factor(temp_means$bin, labels=1:3)
temp_grps <- temp_means[,c(1,2,5)]

precip_means <- ddply(precip_sum_byyear, .(plotID,plotnum), summarize, precip=mean(precipsum, na.rm=T)) #had to remove NAS here. Maybe remove NAs in climatemissing instead?
precip_means$bin <- Hmisc::cut2(precip_means$precip, g=3)
precip_means$binp <- factor(precip_means$bin, labels=4:6)
precip_grps <- precip_means[,c(1,2,5)]

# create bins of both labels
clim_grps <- left_join(temp_grps, precip_grps) %>% mutate(bingrp = paste(bint, binp))
grp_levels <- sort(unique(clim_grps$bingrp))
clim_grps$bingrp <- factor(clim_grps$bingrp , levels = grp_levels, labels=1:9)
# level order is: low temp low precip, low temp mid precip, etc etc
clim_grps <- clim_grps[,c(1,2,5)]
#clim_grps$plotID <- as.character(clim_grps$plotID)
save(clim_grps, file = 'Data/climgrps.r')

# 3. Match and get bins for trait data used

## Organize sampled sla data from Norway, 2015
sla <- read.csv('./Data/trait data/indtraits_nor.csv')

#add rows to complete bins
sla_bin <- inner_join(sla, clim_grps) %>% group_by(species, bingrp) %>% summarise(SLA=mean(SLA_m))
sla_bin_ex <- sla_bin[c(1,2,3,6,6,6,15,15,15,16),]
sla_bin_ex$bingrp <- c(3,4,3,1,2,3,1,2,3,3)
sla_binned <- rbind(transform(as.data.frame(sla_bin), bingrp=as.numeric(as.character(bingrp))), as.data.frame(sla_bin_ex))
sla_binned <- sla_binned[order(sla_binned$species, sla_binned$bingrp),]
# only gran and graor have values for all 4 bins

## Read in try data for sla (supp) and ssd
trytraits <- read.csv('./Data/trait data/traitclim.csv') #read in all sla and ssd try traits from traitlocsforbin_28082016.R
# spit into sla and ssd
try_sla <- trytraits %>% filter(grepl('Specific leaf area', DataName))
try_ssd <- trytraits %>% filter(grepl('Wood density', DataName)) 

#Create corresponding climate bins in TRY data
tcuts <- Hmisc::cut2(temp_means$temp, g=3, onlycuts=T)
pcuts <- Hmisc::cut2(precip_means$precip, g=3, onlycuts=T)

try_ssd$tcut <- Hmisc::cut2(try_ssd$temp, cuts=tcuts, minmax=F) 
try_ssd$bint <- factor(try_ssd$tcut, labels=1:3)
try_ssd$pcut <- Hmisc::cut2(try_ssd$precip, cuts=pcuts, minmax=F) 
try_ssd$binp <- factor(try_ssd$pcut, labels=4:6)

try_sla$tcut <- Hmisc::cut2(try_sla$temp, cuts=tcuts, minmax=F) 
try_sla$bint <- factor(try_sla$tcut, labels=2:3) # we only have bins 2:3 in try data
try_sla$pcut <- Hmisc::cut2(try_sla$precip, cuts=pcuts, minmax=F)
try_sla$binp <- factor(try_sla$pcut, labels=4:5) # we only have bins 1:2 (=4:5) in try data

# ssd <- try_ssd %>% mutate(bingrp = paste(bint, binp))
# ssd$bingrp <- factor(ssd$bingrp , levels = c("1 3","1 4","2 3","2 4"), labels=1:4)
# 

# sla_bin <- inner_join(sla, clim_grps) %>% group_by(species, bingrp) %>% summarise(SLA=mean(SLA_m))
# sla_bin_ex <- sla_bin[c(1,2,3,6,6,6,15,15,15,16),]
# sla_bin_ex$bingrp <- c(3,4,3,1,2,3,1,2,3,3)
# sla_binned <- rbind(transform(as.data.frame(sla_bin), bingrp=as.numeric(as.character(bingrp))), as.data.frame(sla_bin_ex))
# sla_binned <- sla_binned[order(sla_binned$species, sla_binned$bingrp),]
# 
# ssd_bin <- ssd %>% group_by(species, bingrp) %>% summarise(SSD_m=mean(ssd, na.rm=T), SSD_sd=sd(ssd), SSD_n=n())
# ssd_bin$species <- car::recode(ssd_bin$species, '"Betula pubescens" = "dunbjork"; "Pinus sylvestris" = "furu"; "Picea abies" = "gran"; "Alnus incana" = "graor"; "Populus tremula" = "osp"; "Sorbus aucuparia" = "rogn"; "Quercus robur" = "eik"')
# ssd_bin_ex <- ssd_bin[c(1,1,1,2,2,2,4,3,9,10,11,12,13,14),]
# ssd_bin_ex$bingrp <- c(1,2,4,1,2,4,1,4,1,2,1,2,1,2)
# ssd_binned <- rbind(transform(as.data.frame(ssd_bin), bingrp=as.numeric(as.character(bingrp))), as.data.frame(ssd_bin_ex))
# ssd_binned <- ssd_binned[order(ssd_binned$species, ssd_binned$bingrp),]
# 
# save(sla_binned, ssd_binned, file = './Data/traitsbinned.r')

###############################
# After making all the above changes, make new input data. . . 
# Make full input data and input data in bins.

# Combine everything into one large data frame.
allstandata <- cbind(tru, areaxdist)
allstandata <- merge(allstandata, as.data.frame(growthrate_longform), all.x=TRUE) # Adds growth rate.
allstandata <- merge(allstandata, gdd_sum_byyear, all.x=TRUE) # Adds growing degreedays (5yr sums).
allstandata <- merge(allstandata, precip_sum_byyear, all.x=TRUE) # Adds yearly precip (5yr sums).
allstandata <- merge(allstandata, clim_grps, all.x=TRUE) # Adds clim binning (1=T1P1, 2=T1P2, 3=T1P3, 4=T2P1, 5=T2P2, 6=T2P3, 7=T3P1, 8=T3P2, 9=T3P3)
#allstandata <- merge(allstandata, traits[,c(1,2,6)], all.x=TRUE) # Adds PC1 and PC2 for traits.

# take only rows to be used. 
# Aug 26: made names explicit
#allstandata <- allstandata[,c(1,2,3,4,11,19:33)]

allstandata <- allstandata[, c("plotID", "plotnum", "year", "newtreeID", "dbh", "editedspnum", "V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "growthrate", "gddsum", "precipsum", "bingrp")]

# get rid of NAs in data.
allstandata <- allstandata[complete.cases(allstandata), ]

#traits_stan <- traits

allstandata$ba <- with(allstandata, pi * ((dbh/10)/2)^2)
allstandata$bainc <- with(allstandata, pi * (((dbh+growthrate)/10)/2)^2 - ba)

save(allstandata, traits_raw_stan, file = './Data/allstandata26aug.r')
# 
# # Create rdumps for input data to STAN
# load('./Data/allstandata15jan.r')
# 
# allstandata_species <- split(allstandata, allstandata$editedspnum)
# allstandata_input <- lapply(allstandata_species, function(x) {
#   x$plotfactor <- as.integer(factor(x$plotnum, labels=1:length(unique(x$plotnum))))
#   x$idfactor <- as.integer(factor(x$newtreeID, labels=1:length(unique(x$newtreeID))))
#   with(x, list(N=nrow(x), Nspp=8, Nyear=20, Nplot=max(plotfactor), Ntree=max(idfactor), targetsp=editedspnum[1], ba=ba/100, bainc=bainc, year=year, plot=plotfactor, tree=idfactor, gdd=gddsum/1000, precip=precipsum/1000, areaxdist=as.matrix(x[,7:14]/1000), trait=as.matrix(traits_raw_stan[,1:2]))) 
# })
# #rm(allstandata_species, allstandata)
# 
# library(rstan)
# for (i in 1:7) {
#   
#   NAMES <- names(allstandata_input[[i]])
#   filename <- paste0('C:/Users/Q/Dropbox/nfi/Cluster/stan/rdumps/scaleddata',i,'.R')
#   with(allstandata_input[[i]], stan_rdump(NAMES, file = filename))
#   
# }
# 
# # Binned data.
# # Note that all species are present in all bins EXCEPT species 5 (oak) which is only present in bin 3 and bin 4.
# 
# data_binned <- split(allstandata, list(allstandata$editedspnum, allstandata$bingrp))
# 
# 
# inputdata_binned <- lapply(data_binned, function(x) { if(nrow(x) > 1) {
#   x$plotfactor <- as.integer(factor(x$plotnum, labels=1:length(unique(x$plotnum))))
#   x$idfactor <- as.integer(factor(x$newtreeID, labels=1:length(unique(x$newtreeID))))
#   with(x, list(N=nrow(x), Nspp=8, Nyear=20, Nplot=max(plotfactor), Ntree=max(idfactor), targetsp=editedspnum[1], bin=bingrp[1], ba=ba/100, bainc=bainc, year=year, plot=plotfactor, tree=idfactor, gdd=gddsum/1000, precip=precipsum/1000, areaxdist=as.matrix(x[,7:14]/1000), trait=as.matrix(traits_raw_stan[,1:2]))) 
# } else { NA }})
# 
# tag <- 7
# 
# library(rstan)
# for (i in 1:length(inputdata_binned)) {
#   if (!is.na(inputdata_binned[[i]])) {
#     if (inputdata_binned[[i]]$targetsp != 8) {
#       tag <- tag + 1
#       NAMES <- names(inputdata_binned[[i]])
#       filename <- paste0('C:/Users/Q/Dropbox/nfi/Cluster/stan/rdumps/scaleddata',tag,'.R')
#       with(inputdata_binned[[i]], stan_rdump(NAMES, file = filename))
#       
#     }
#   }
# }