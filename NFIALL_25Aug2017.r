# NFI Workflow from start to finish.

### Convert all factor columns in a data frame to character

f2c <- function(dataframe) {
  i <- sapply(dataframe, is.factor)
  dataframe[i] <- lapply(dataframe[i], as.character)
  return(dataframe)
}

# Load data prepared by Clara

load('./Data/tree data/Pre2015-02-13.RData') # Creates data frames tr, fl
spcode <- read.csv('./Data/tree data/nor_splist.csv')

# Translate column names from Bokmal to English!
tEnglishNames <- c('plotID','partialPlotClass','year','species','distance','azimuth','measHeight','estHeight','dbh','crownHeight','crownColor','crownDensity','harvestClass')
fEnglishNames <- c('plotID','partialPlotClass','partialPlotProp','year','commune','soilDepth','areaType','landUse','soilType','crownCover')

tre <- tr
fle <- fl
names(tre) <- tEnglishNames
names(fle) <- fEnglishNames

# Attempt to track individual trees over time.
# First attempt is just to find trees with matching (exactly identical) coordinates.

library(plyr)
library(reshape2)

# Find unique trees
locations <- tre[, c('plotID', 'distance', 'azimuth')]
lu <- unique(locations)
lu$plotID <- as.character(lu$plotID)

# Assign each unique tree a number.
treeid <- rep(NA, nrow(tre))
for (i in 1:nrow(tre)) {
  A <- as.character(tre[i, 'plotID'])
  B <- tre[i, 'distance']
  Z <- tre[i, 'azimuth']
  idx <- which(A == lu[,1] & B == lu[,2] & Z == lu[,3])
  if (length(idx) > 0) treeid[i] <- idx
  if (i%%1000 == 0) print(i)
}

tre$treeID <- treeid
write.table(tre, file='./Data/tree data/trid.csv', sep=',', row.names=FALSE)

# Check how many plot/year combinations have missing locations for trees.
anymissing <- ddply(tre, .(plotID, year), function(x) any(is.na(x$distance)), .progress = 'text')

# Only 3.7% of plot/year combinations have any missing locations.


##########

# Get 5-year growth increments for the trees using the new IDs.
incr <- dlply(tru, .(newtreeID), function(x) {
  x <- x[order(x$year), ]
  list(treeID=x$newtreeID[1], incr=x$dbh[-1] - x$dbh[-nrow(x)], year=x$year[-1])
},
.progress='text')

# Scale this by the number of years since the last sampling date to get a growth rate per year.
growthrate <- dlply(tru, .(newtreeID), function(x) {
  x <- x[order(x$year), ]
  list(treeID=x$newtreeID[1], grate=(x$dbh[-1] - x$dbh[-nrow(x)])/(x$year[-1] - x$year[-nrow(x)]), year=x$year[-1])
},
.progress='text')

incrdat <- matrix(NA, nrow=length(incr), ncol=length(1987:2013)+1)
for (i in 1:length(incr)) {
  incrdat[i, 1] <- incr[[i]]$treeID
  y <- incr[[i]]$year - 1985
  incrdat[i, y] <- incr[[i]]$incr
}

gratedat <- matrix(NA, nrow=length(growthrate), ncol=length(1987:2013)+1)
for (i in 1:length(growthrate)) {
  gratedat[i, 1] <- growthrate[[i]]$treeID
  y <- growthrate[[i]]$year - 1985
  growthrate[[i]]$grate[growthrate[[i]]$grate == Inf | growthrate[[i]]$grate == -Inf] <- NA
  gratedat[i, y] <- growthrate[[i]]$grate
}

# get rid of "NA" row
#incrdat <- incrdat[-nrow(incrdat),]
incrnames <- c('treeID', paste0('incr', 1987:2013))
incrdat <- as.data.frame(incrdat)
names(incrdat) <- incrnames

write.table(incrdat, file='./Data/tree data/incr.csv', sep=',', row.names=FALSE)

#gratedat <- gratedat[-nrow(gratedat),]
gratenames <- c('treeID', paste0('growthrate', 1987:2013))
gratedat <- as.data.frame(gratedat)
names(gratedat) <- gratenames

write.table(gratedat, file='./Data/tree data/grthrate.csv', sep=',', row.names=FALSE)

############
# Get whether or not the tree died between sampling points.

# Find the last year sampled for each plot.
lastYear <- ddply(tru, .(plotID), function(x) max(x$year))
nYears <- ddply(tru, .(plotID), function(x) length(unique(x$year)))
lastYear <- f2c(lastYear)

# This will give a lower (earlier) bound on when the tree died since it is the last year a tree that later disappeared was recorded as alive.
mort <- ddply(tru, .(newtreeID), function(x) {
  x <- f2c(x)
  lastYearx <- max(x$year)
  lastYearPlot <- lastYear$V1[lastYear$plotID == x$plotID[1]]
  if (lastYearx < lastYearPlot) lastYearx else NA
  },
.progress='text')


# get rid of "NA" row
#mort <- mort[-nrow(mort),]
names(mort) <- c('treeID','lastAlive')

write.table(mort, file='./Data/tree data/mort.csv', sep=',', row.names=FALSE)

# Remove records from tre dataframe that are not good.
# Use to create tru dataframe.

# Edit down the tre dataframe to include only plot/year combinations with all tree locations, only species we have growth parameters for, and remove all partial plots that we do not know the area of.

# Get rid of bad plot/year combos:

keep <- with(anymissing[!anymissing$V1,], paste(plotID, year))
treUsed <- tre[paste(tre$plotID, tre$year) %in% keep, ]

# Get rid of plots that have species we don't have growth parameters for.

badspp <- unique(tre$species)[!unique(tre$species) %in% growthpars$species]
badplots <- tre$plotID[tre$species %in% badspp]
treUsed <- treUsed[!treUsed$plotID %in% badplots, ]

# Get rid of partial plots
treUsed <- subset(treUsed, partialPlotClass == 0)


# We are now down to 530K records instead of the original 584K.
write.table(treUsed, file='./Data/tree data/treused.csv', sep=',', row.names=FALSE)

# Also get rid of all the trees that have more than one species recorded for them. They're bad.
# Removes about 2000 trees.
spcheck <- ddply(tru, .(treeID), function(x) length(unique(x$species)))

keep <- !tru$treeID %in% spcheck$treeID[spcheck$V1 > 1]
tru <- tru[keep, ]

tru$newtreeID <- factor(tru$treeID)
levels(tru$newtreeID) <- 1:length(levels(tru$newtreeID))
tru$newtreeID <- as.integer(tru$newtreeID)

### Another problem has been discovered. There are a few trees with the same ID and the same year appearing in the data set. We should find and deal with them.

duplicatecheck <- ddply(tru, .(newtreeID,year), nrow, .progress='text') # If >1 there is a duplicate
# 150 trees are duplicated.
keep <- !tru$newtreeID %in% duplicatecheck$newtreeID[duplicatecheck$V1 > 1]
tru <- tru[keep,]

tru$newtreeID <- factor(tru$newtreeID)
levels(tru$newtreeID) <- 1:length(levels(tru$newtreeID))
tru$newtreeID <- as.integer(tru$newtreeID)


write.table(tru, file='./Data/tree data/treused.csv', sep=',', row.names=FALSE)

# To simplify models, use only the most common species and sort everything else into "other" deciduous and coniferous.
### THIS IS NEW AS OF NOVEMBER 22. We remove "hassel" and "selje" to leave us with only 7 species and everything else thrown into other.

abtable <- table(tru$species)

x <- as.numeric(names(abtable))
names(abtable) <- spcode$sp_lat[match(x, spcode$code)]
cumsum(sort(abtable, decreasing=TRUE))/nrow(tru)

# Anything outside of the top 7 species will be classified as "other" and given species code 999. In total over 98% of the individuals are in the top 9.
commonspp <- names(sort(table(tru$species), decreasing=TRUE))[1:7]
tru$editedspecies <- tru$species
tru$editedspecies[!tru$editedspecies %in% as.numeric(commonspp)] <- 999

tru$editedspnum <- factor(tru$editedspecies)
levels(tru$editedspnum) <- 1:8
tru <- subset(tru, !is.na(treeID))

write.table(tru, file='./Data/tree data/treused.csv', sep=',', row.names=FALSE)


### Calculation of area/distance^2 for all species for crowding index.

# Creation of array for spatial crowding index.
# Also set the distance decay function to a very simple function of 1/distance^2. Later we can improve this if needed.
# Edited this on march 13 to use the square of distance.
ddf <- function(d) 1/(d^2)

# Convenience function to calculate the straight line distance between two points in polar coordinates (also convert from degrees to radians)
dpol <- function(r1, theta1, r2, theta2) 
  sqrt((r1*cos(theta1*pi/180) - r2*cos(theta2*pi/180))^2 + (r1*sin(theta1*pi/180) - r2*sin(theta2*pi/180))^2)

# Area of two overlapping circles with the same radius and distance D between them.

aoverlap <- function(D, R = sqrt(250/pi)) {
	Q <- 2 * acos(D/(2*R))
	(R^2) * (Q - sin(Q))
}


# Correct the dumb 400-degree scale used by NFI to the 360-degree scale.
tru <- transform(tru, azimuth = azimuth * 360/400)

areaxdist.list <- dlply(tru, .(plotID, year), function(x) {
  areaxdist <- matrix(0, nrow=nrow(x), ncol=8)
  for (i in 1:nrow(x)) {
    treex <- x[i,]
    for (s in 1:8) {
      neighbors <- subset(x[-i,], editedspnum==s)
      if (nrow(neighbors) > 0) {
        distances <- dpol(treex$distance/10, treex$azimuth, neighbors$distance/10, neighbors$azimuth)
        basalareas <- pi * (neighbors$dbh/2)^2
		# Correct for overlap portion.
        areaxdist[i,s] <- sum(ddf(distances) * basalareas) / (aoverlap(D = treex$distance/10)/250)
      }
    }
  }
  return(areaxdist)
}, .progress = 'text')


index.list <- ddply(tru, .(plotID,year), function(x) cbind(treeID=x$newtreeID, IDinplot=1:nrow(x)), .progress='text')
index.list <- cbind(index.list, index=1:nrow(index.list))

areaxdist.df <- do.call('rbind',areaxdist.list)
#nneighb.df <- do.call('rbind',nneighb.list)

names(index.list)[3] <- 'newtreeID'
truidx <- join(tru, index.list, type='left', match='first')

areaxdist.df <- areaxdist.df[truidx$index, ]

write.table(areaxdist.df, file='./Data/tree data/areaxdist2_corrected.csv', sep=',', row.names=FALSE)

# Chelsea's code for trait pca


library(vegan)
library(dplyr)
library(tidyr)

DTtr <- read.csv('./Data/trait data/traits22Nov.csv', stringsAsFactors=F)

#df from sorttry
unique(DTtr$variable)
unique(DTtr$Species)

df_pca <- DTtr %>% select(SpeciesName, variable, V1) %>% 
  spread(variable, V1, fill=NA)
df_pca <- as.data.frame(df_pca)

#check for coverage
colSums(is.na(df_pca)) #slife has some zeros. CHECK!

#select amd transform
df_p<-df_pca[c(2, 8:12,16)]
#Traits: leaf C (cmass), leaf thickness (lth), leaf N (nmass), plant lifespan (plife), 
#leaf P (pmass), plant relative growth rate (prgr), stem specific density (ssd)
df_p<-log(df_p)

#pca
pca_nor <- prcomp(df_p, scale=T, center=T)
pca_nor
biplot(pca_nor)

plot(pca_nor, type='l') #keep first two!
#First axis- cmass/nmass/pmass, second is leaf thickness, plant lifespan, stem den
summary(pca_nor)

#get scores
pca_sc<-as.matrix(predict(pca_nor))
pca_scores<-data.frame(df_pca$SpeciesName, pca_sc[,1:5])

#get trait loadings
pca_lo<-pca_nor$rotation
percent_lo <- abs(pca_nor$rotation)

# #save and write
write.csv(pca_scores, file='./Data/trait data/scores22Nov.csv')
write.csv(percent_lo, file='./Data/trait data/loadings22Nov.csv')
save(pca_nor, file='./Data/trait data/pca_nor22Nov.RData')




####################################################

# Script can be run starting here, by loading the files.

# Process all the data for stan format and save as data frame.


tru <- read.csv('./Data/tree data/treused.csv')
#grthr <- read.csv('./Data/tree data/grthrate.csv')
areaxdist <- read.csv('./Data/tree data/areaxdist2_corrected.csv')

#### here read in climate data as well.
source('./Code/processing climate data/gddprecip.r') # takes a while
#### here read in trait PCA data
traits <- read.csv('./Data/trait data/scores22Nov.csv', row.names=1)
# add an "average" value for the 8th category, the miscellaneous species.
traits[8,] <- 0
# add the new species code.
traits$editedspnum <- c(3,5,2,1,6,4,7,8)
traits <- traits[order(traits$editedspnum), ]
# Read sites where climate data are missing.
dataMissing <- read.csv('./Data/tree data/dataMissing.csv')


# Re run growth rate calculation to make into a long vector.
growthrate <- dlply(tru, .(newtreeID), function(x) {
  x <- x[order(x$year), ]
  list(treeID=x$newtreeID[1], grate=(x$dbh[-1] - x$dbh[-nrow(x)])/(x$year[-1] - x$year[-nrow(x)]), year=x$year[-1])
},
.progress='text')

growthrate_longform <- do.call('rbind', lapply(growthrate, function(x) ifelse(length(x$grate) > 0, as.data.frame(x), data.frame(treeID=integer(0),grate=numeric(0),year=integer(0)))))
growthrate2 <- lapply(growthrate, do.call, what='cbind')
growthrate_longform <- growthrate2[[1]]
for (i in 2:length(growthrate2)) if(ncol(growthrate2[[i]])==3) growthrate_longform <- rbind(growthrate_longform, growthrate2[[i]])
#save(growthrate_longform, file = './Data/grlongform.r')
#load('./Data/grlongform.r')

# Get rid of anomalously large growth rate values.
growthrate_longform <- growthrate_longform[growthrate_longform[,2] < 20 & growthrate_longform[,2] > -20, ]
dimnames(growthrate_longform)[[2]] <- c('newtreeID','growthrate','year')

# Combine everything into one large data frame.
allstandata <- cbind(tru, areaxdist)
allstandata <- merge(allstandata, as.data.frame(growthrate_longform), all.x=TRUE) # Adds growth rate.
allstandata <- merge(allstandata, gdd_byyear, all.x=TRUE) # Adds growing degreedays.
allstandata <- merge(allstandata, totalprecip_byyear, all.x=TRUE) # Adds yearly precip.
#allstandata <- merge(allstandata, traits[,c(1,2,6)], all.x=TRUE) # Adds PC1 and PC2 for traits.

# take only rows to be used.
allstandata <- allstandata[,c(1,2,3,4,11,19:30)]

# get rid of NAs in data.
allstandata <- allstandata[complete.cases(allstandata), ]

traits_stan <- traits

allstandata$ba <- with(allstandata, pi * ((dbh/10)/2)^2)
allstandata$bainc <- with(allstandata, pi * (((dbh+growthrate)/10)/2)^2 - ba)

save(allstandata, traits_stan, file = './Data/allstandata11jan.r')

#save(allstandata, traits_stan, file = './Data/allstandata22nov.r')

#load('./Data/allstandata22nov.r') # Loads allstandata and traits_stan (created with stan_dataprocessing.r script)

# Format as lists for Stan input.

# Scaling of units: dbh in cm (dbh/10), growthrate in mm (no change), gdd in thousands of degreedays/year (gdd/1000), precip in m/year (precip/1000), area/dist2 in m2/m2/1000 (areaxdist/1000)

load('./Data/allstandata26aug.r')

# standardize traits
traits_stan <- sapply(traits_raw_stan, function(x) (x - mean(x))/sd(x))

# check how many plots there are by year for each species
library(dplyr)
allstandata %>% group_by(editedspnum) %>% summarize(nplots=length(unique(plotID))) 

allstandata$ba <- with(allstandata, pi * ((dbh/10)/2)^2)
allstandata$bainc <- with(allstandata, pi * (((dbh+growthrate)/10)/2)^2 - ba)


# Sample down to 500 plots for the ones that have more than 500.

allstandata_species <- split(allstandata, allstandata$editedspnum)
set.seed(21812)
allstandata_input <- lapply(allstandata_species, function(x) {
  
  x_plots <- unique(x$plotnum)
  if (length(x_plots) > 500) {
    use_plots <- sample(x_plots, size=500, replace=FALSE)
    x <- subset(x, plotnum %in% use_plots)
  }
  
	x$plotfactor <- as.integer(factor(x$plotnum, labels=1:length(unique(x$plotnum))))
	x$idfactor <- as.integer(factor(x$newtreeID, labels=1:length(unique(x$newtreeID))))
	
	with(x, list(N=nrow(x), Nspp=8, Nyear=20, Nplot=max(plotfactor), Ntree=max(idfactor), targetsp=editedspnum[1], ba=ba/100, bainc=bainc, year=year, plot=plotfactor, tree=idfactor, gdd=gddsum/1000, precip=precipsum/1000, areaxdist=as.matrix(x[,7:14]/1000), trait=as.matrix(traits_stan[,1:2]))) 
})
rm(allstandata_species, allstandata)

library(rstan)
for (i in 1:7) {

NAMES <- names(allstandata_input[[i]])
filename <- paste0('C:/Users/Q/Dropbox/nfi/Cluster/stan/subsampledata',i,'.R')
with(allstandata_input[[i]], stan_rdump(NAMES, file = filename))

}

######################################
# 30 November: Add basal area and basal area increment to data set.
# Convert to cm^2 before adding.
allstandata$ba <- with(allstandata, pi * ((dbh/10)/2)^2)
allstandata$bainc <- with(allstandata, pi * (((dbh+growthrate)/10)/2)^2 - ba)
  
allstandata_species <- split(allstandata, allstandata$editedspnum)
allstandata_input <- lapply(allstandata_species, function(x) {
  x$plotfactor <- as.integer(factor(x$plotnum, labels=1:length(unique(x$plotnum))))
  x$idfactor <- as.integer(factor(x$newtreeID, labels=1:length(unique(x$newtreeID))))
  with(x, list(N=nrow(x), Nspp=8, Nyear=20, Nplot=max(plotfactor), Ntree=max(idfactor), targetsp=editedspnum[1], ba=ba/100, bainc=bainc, year=year, plot=plotfactor, tree=idfactor, gdd=gdd/1000, precip=precip/1000, areaxdist=as.matrix(x[,6:13]/1000), trait=as.matrix(traits_stan[,1:2]))) 
})
rm(allstandata_species, allstandata)

library(rstan)
for (i in 1:7) {
  
  NAMES <- names(allstandata_input[[i]])
  filename <- paste0('C:/Users/Q/Dropbox/nfi/Cluster/stan/scaleddata30Nov',i,'.R')
  with(allstandata_input[[i]], stan_rdump(NAMES, file = filename))
  
}

######################################
# 11 January 2016: Update with Clara's new climate data.

# Load old stan data
load('./Data/allstandata11jan.r')
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

# Match new climate data with the plot IDs
allstandata <- merge(allstandata, gddprecip_new, all.x=TRUE, sort=FALSE)

# Scale data and create rdumps for use with stan.
allstandata_species <- split(allstandata, allstandata$editedspnum)
allstandata_input <- lapply(allstandata_species, function(x) {
  x$plotfactor <- as.integer(factor(x$plotnum, labels=1:length(unique(x$plotnum))))
  x$idfactor <- as.integer(factor(x$newtreeID, labels=1:length(unique(x$newtreeID))))
  with(x, list(N=nrow(x), Nspp=8, Nyear=20, Nplot=max(plotfactor), Ntree=max(idfactor), targetsp=editedspnum[1], ba=ba/100, bainc=bainc, year=year, plot=plotfactor, tree=idfactor, gdd=gdd/1000, precip=precip/1000, areaxdist=as.matrix(x[,6:13]/1000), trait=as.matrix(traits_stan[,1:2]))) 
})
rm(allstandata_species, allstandata)

library(rstan)
for (i in 1:7) {
  
  NAMES <- names(allstandata_input[[i]])
  filename <- paste0('C:/Users/Q/Dropbox/nfi/Cluster/stan/scaleddata11Jan',i,'.R')
  with(allstandata_input[[i]], stan_rdump(NAMES, file = filename))
  
}

