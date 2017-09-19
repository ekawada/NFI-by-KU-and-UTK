# Entire NFI workflow
# QDR 06 Sep 2017

# Steps required in analysis

### I. Processing tree data

# 1. compare treid with the azimuth and position of the tree. Get rid of plots that have too many discrepancies.
# 2. calculate basal area and annualized basal area increments from the dbh's of the trees
# 3. find mortality events
# 4. get rid of plots with the following issues: trees with missing locations or nonmatching IDs, partial plots, trees with the same DI in different locations
# 5. classify all but the most common 7 species as "other"
# 6. transform azimuth to degrees
# 7. calculate the distance from each tree to its neighbor
# 8. calculate sum of basal area/distance^2 for each neighbor of each tree, one sum for each species
# 9. adjust sums depending on the distance of the target tree from the center of the plot

### II. Processing climate data

# 1. load Clara's climate data
# 2. calculate yearly sums of growing degree days and precipitation for each plot
# 3. Bin by 3x3 temperature (or gdd) and precipitation


### III. Processing trait data

# 1. Combine field-collected with TRY data to get maximum coverage.
# 2. Interpolate missing bins to get both an overall mean, and a by-bin mean, for each species and trait (SLA and SSD)


### IV. Create rdumps

# 1. If necessary, for all tree species occurring in >500 plots, sample down to 500 plots for neighbor trait model (there will be 7 rdumps for this model)
# 2. rdump for each of the 9 temperature bins for the individual trait model.

### V. Fit the neighbor trait model

# Stan scripts to use: GitHub/mc/mc_18sep2017/mc1_nullmodel.stan (and so forth)
# Upload appropriate rdumps and run model on ND cluster

### VI. Fit the (simpler) focal tree trait model

# Stan script to use: GitHub/mc/mc_18sep2017/individualmodel.stan
# Upload appropriate rdumps and run model on ND cluster


# I. Processing tree data -------------------------------------------------

library(dplyr)

# Begin with new raw data

load('Data/tree data/Pre2017-09-01.RData')
spcode <- read.csv('Data/tree data/nor_splist.csv', stringsAsFactors = FALSE)

# Translate column names from Bokmal to English!
tEnglishNames <- c('treeID', 'plotID','partialPlotClass','year','species','distance','azimuth','measHeight','estHeight','dbh','crownHeight','crownColor','crownDensity','harvestClass')
fEnglishNames <- c('plotID','partialPlotClass','partialPlotProp','year','commune','siteQuality','treeQuality','soilDepth','areaType','landUse','soilType','crownCover')

names(tr) <- tEnglishNames
names(fl) <- fEnglishNames

# Convert factors (lame) to strings (sweet)
tr <- tr %>% mutate_if(is.factor, as.character)
fl <- fl %>% mutate_if(is.factor, as.character)

# Convert azimuth to degrees
# Basal area (diameter converted to m to get basal area in m^2)
# Distance from dm to m
# Cartesian coordiantes using formula to convert from polar coordinates
tr <- tr %>%
  mutate(azimuth360 = azimuth * 360/400,
         ba = pi * ((dbh/1000)/2)^2,
         distance = distance / 10,
         coord_x = distance * cos(azimuth360 * pi/180),
         coord_y = distance * sin(azimuth360 * pi/180))


# Try to find the distance in between the multiple location trees. If the distance is small, "merge" the locations, aka just ignore the fact that the tree moved slightly since it is probably a measurement error. Otherwise if it is big, flag it.

# Calculate distance matrix for all the trees.
tr_distmats <- tr %>%
  group_by(treeID) %>%
  do(d = dist(cbind(.$coord_x, .$coord_y)))

table(unlist(sapply(tr_distmats$d, length)))

maxdists <- unlist(sapply(tr_distmats$d, max))
hist(maxdists)
table(maxdists > 2)
# Use the threshold of 2 meters. This leads to 674 trees being identified as too far apart between censuses.
length(unique(tr$plotID[which(maxdists > 2)]))
tr_identify_bad_plots <- tr %>%
  left_join(data.frame(treeID = tr_distmats$treeID, maxdist = maxdists)) %>%
  group_by(plotID) %>%
  summarize(proportion_bad = sum(maxdist > 2)/n())

table(tr_identify_bad_plots$proportion_bad > 0.2)

# Remove any plots that have over 20% trees with multiple locations that are at least 5 meters apart, and the same ID.

bad_plot <- tr_identify_bad_plots$plotID[which(tr_identify_bad_plots$proportion_bad > 0.2)]

# Also check how many plots have trees with missing locations
tr_identify_bad_plots2 <- tr %>%
  group_by(plotID) %>%
  summarize(proportion_missingloc = sum(is.na(distance))/n())

table(tr_identify_bad_plots2$proportion_missingloc > 0.2)

# Remove plots with over 20% trees with missing locations
bad_plot2 <- tr_identify_bad_plots2$plotID[which(tr_identify_bad_plots2$proportion_missingloc > 0.2)]

tru <- tr %>%
  filter(!plotID %in% c(bad_plot, bad_plot2))

# Now get rid of the trees that have missing locations in the remaining plots (not very many).
tru <- tru %>%
  filter(!is.na(distance), !is.na(azimuth))

# Calculate annualized basal area increment

ba.to.x <- function(ba1, ba2, years, x){
  rate <-  (ba2 / ba1 )^(1/years) - 1
  bax <- ba1 * (1 + rate)^x
  return(bax)
}

# Get basal area increment for every tree.

get_bainc <- function(dat) {
  dat <- dat[order(dat$year), ]
  diff_ba <- diff(dat$ba)
  ba_begin <- dat$ba[-nrow(dat)]
  ba_end <- dat$ba[-1]
  diff_year <- diff(dat$year)
  ba_1 <- ba.to.x(ba_begin, ba_end, diff_year, 1)
  data.frame(bainc = ba_1 - ba_begin, year = dat$year[-1])
}

tr_bainc <- tr %>%
  group_by(treeID) %>%
  do(get_bainc(.))


# Merge basal area increment with the data frame of trees to be used.
tru <- left_join(tru, tr_bainc)

# Find mortality events
# *** skip this for now.

# Combine all the species other than the Big Seven into an "other" code.
commonspp <- names(sort(table(tru$species), decreasing=TRUE))[1:7]
tru$editedspecies <- tru$species
tru$editedspecies[!tru$editedspecies %in% as.numeric(commonspp)] <- 999

# Create lookup table to link species # 1-7 to the NFI species codes.

tru$editedspecies <- factor(tru$editedspecies)

sp_lookup_table <- data.frame(code =  as.numeric(names(table(tru$editedspecies)))[-8], id = 1:7)

#   code id
# 1    1  1
# 2   10  2
# 3   30  3
# 4   32  4
# 5   40  5
# 6   50  6
# 7   53  7

levels(tru$editedspecies) <- 1:8

# Calculate distance from each tree to its neighbor, weighted by the size of the neighbor.

# Distance decay function we're using is just 1 over distance squared.
ddf <- function(d) 1/(d^2)

# Convenience function to calculate the straight line distance between two points in polar coordinates (also convert from degrees to radians)
dpol <- function(r1, theta1, r2, theta2) 
  sqrt((r1*cos(theta1*pi/180) - r2*cos(theta2*pi/180))^2 + (r1*sin(theta1*pi/180) - r2*sin(theta2*pi/180))^2)

# Area of two overlapping circles with the same radius and distance D between them.

aoverlap <- function(D, R = sqrt(250/pi)) {
  Q <- 2 * acos(D/(2*R))
  (R^2) * (Q - sin(Q))
}

# Get area by distance matrix for each plot
get_areaxdist <- function(dat) {
  res <- matrix(0, nrow = nrow(dat), ncol = 8)
  for (i in 1:nrow(dat)) {
    treex <- dat[i,]
    for (s in 1:8) {
      neighbors <- subset(dat[-i,], editedspecies==s)
      if (nrow(neighbors) > 0) {
        distances <- dpol(treex$distance, treex$azimuth360, neighbors$distance, neighbors$azimuth360)
        # Correct for overlap portion.
        res[i,s] <- sum(ddf(distances) * neighbors$ba) / (aoverlap(D = treex$distance)/250)
      }
    }
  }
  res <- as.data.frame(res)
  names(res) <- paste0('area_', 1:8)
  return(cbind(treeID = dat$treeID, res))
}



# Calculate area x distance for each tree (takes 2 hours)
areaxdist <- tru %>% 
  group_by(plotID, year) %>%
  do(get_areaxdist(.))

# Merge the calculated area x distance values with the main dataframe.
areaxdist[areaxdist == Inf] <- 0
tru <- left_join(tru, areaxdist)

save.image('~/tempwksp.RData')

# II. Processing climate data ---------------------------------------------

load('./Data/nfi climate data/ClimNewChelsea.RData') # This is the new climate data from Clara
# Edited 18 Sep to use the corrected temperature and precip. Use the average daily precip rather than the sum.

# Calculate annual growing degree day sums and precipitation sums.
days <- c(31,28.25,31,30,31,30,31,31,30,31,30,31)
threshold <- 5

# Growing degree day threshold is 5 C (recommended by Clara). Multiply average monthly degrees above threshold by the number of days in each month to get that month's GDD.
# This summarize operation takes a while to run.
gddprecip <- clim.chelsea %>%
  rename(plotID = flateid) %>%
   filter(year >= 1986) %>%
   mutate(degrees = (temp.corr - threshold) * (temp.corr - threshold > 0)) %>%
   group_by(plotID, year) %>%
   summarize(gdd = sum(degrees * days),
             precip = mean(prep))

# Get average gdd and precip of each plot across all years
gddprecip_allyears <- gddprecip %>%
  ungroup() %>%
  group_by(plotID) %>%
  summarize(gdd = mean(na.omit(gdd)), precip = mean(na.omit(precip)))

# Create climate bins. 3x3 temperature x precipitation.
temp_cuts <- quantile(gddprecip_allyears$gdd, probs = c(1/3, 2/3))
precip_cuts <- quantile(gddprecip_allyears$precip, probs = c(1/3, 2/3))

gddprecip_allyears <- mutate(gddprecip_allyears, gddbin = 1, precipbin = 1)
gddprecip_allyears$gddbin[gddprecip_allyears$gdd > temp_cuts[1]] <- 2
gddprecip_allyears$gddbin[gddprecip_allyears$gdd > temp_cuts[2]] <- 3
gddprecip_allyears$precipbin[gddprecip_allyears$precip > precip_cuts[1]] <- 2
gddprecip_allyears$precipbin[gddprecip_allyears$precip > precip_cuts[2]] <- 3
gddprecip_allyears <- mutate(gddprecip_allyears, climbin = interaction(gddbin, precipbin))

# Find the GDD and precip that correspond to the actual growth interval, not just the single year.
# Average gdd by year and average precip for each of the years.

get_climate_interval <- function(dat) {
  dat_i <- dat[order(dat$year), ]
  gdd_interval <- rep(NA, nrow(dat_i))
  precip_interval <- rep(NA, nrow(dat_i))
  for (y in 2:nrow(dat_i)) {
    gddprec_subset <- subset(gddprecip, plotID == dat_i$plotID[1] & year > dat_i$year[y-1] & year <= dat_i$year[y])
    gdd_interval[y] <- mean(gddprec_subset$gdd)
    precip_interval[y] <- mean(gddprec_subset$precip)
  }
  data.frame(gdd = gdd_interval, precip = precip_interval, year = dat$year)
}

tr_clim_inc <- tru %>%
  group_by(treeID) %>%
  do(get_climate_interval(.))

tru <- tru %>%
  left_join(tr_clim_inc %>% ungroup %>% filter(!is.na(gdd), !is.na(precip))) %>%
  left_join(gddprecip_allyears[,c('plotID','gddbin','precipbin', 'climbin')])

save.image('~/tempwksp.RData')

# III. Processing trait data ----------------------------------------------

# These two data frames have already been processed elsewhere by CC.

## sampled sla data from Norway, 2015
slainds <- read.csv('Data/trait data/indtraits_nor.csv')

#bin based on plot location data
sla_ind <- left_join(slainds, gddprecip_allyears) %>% group_by(species, bingrp) %>% dplyr::summarise(SLA=mean(SLA_m))

# TRY traits, with climate information already included.
trytraits <- read.csv('Data/trait data/traitclim.csv') #read in all sla and ssd try traits from traitlocsforbin_28082016.R
# split into sla and ssd
try_sla <- trytraits %>% filter(grepl('Specific leaf area', DataName))
try_ssd <- trytraits %>% filter(grepl('Wood density', DataName)) 



#add rows to complete bins
sla_bin <- inner_join(sla, gddprecip_allyears[,c('plotID', 'climbin')]) %>% group_by(species, climbin) %>% summarise(SLA=mean(SLA_m))

#Create corresponding climate bins in TRY data

# Get average temp and precip of each plot across all years
temp_prec_means <- klima %>%
  group_by(plotID, year) %>%
  summarize(temp_yrmean = mean(temp_abs), precip_yrmean = sum(precip_abs)) %>%
  ungroup %>% group_by(plotID) %>%
  summarize(temp_overallmean = mean(na.omit(temp_yrmean)), precip_overallmean = mean(na.omit(precip_yrmean)))

# Create climate bins. 3x3 temperature x precipitation.
temp_cuts <- quantile(temp_prec_means$temp, probs = c(1/3, 2/3))
precip_cuts <- quantile(temp_prec_means$precip, probs = c(1/3, 2/3))

tcuts <- Hmisc::cut2(temp_prec_means$temp, g=3, onlycuts=T)
pcuts <- Hmisc::cut2(temp_prec_means$precip, g=3, onlycuts=T)

try_ssd$tcut <- Hmisc::cut2(try_ssd$temp, cuts=tcuts, minmax=F) 
try_ssd$bint <- factor(try_ssd$tcut, labels=1:3)
try_ssd$pcut <- Hmisc::cut2(try_ssd$precip, cuts=pcuts, minmax=F) 
try_ssd$binp <- factor(try_ssd$pcut, labels=4:6)

try_sla$tcut <- Hmisc::cut2(try_sla$temp, cuts=tcuts, minmax=F) 
try_sla$bint <- factor(try_sla$tcut, labels=2:3) # we only have bins 2:3 in try data
try_sla$pcut <- Hmisc::cut2(try_sla$precip, cuts=pcuts, minmax=F)
try_sla$binp <- factor(try_sla$pcut, labels=4:5) # we only have bins 1:2 (=4:5) in try data

# IV. Binning and exporting to rdump --------------------------------------

# 1. Cut everything into bins

# 2. Get rid of negative basal area increments or convert to zeroes

# 3. Do subsampling if necessary

# 4. Export to rdump files
