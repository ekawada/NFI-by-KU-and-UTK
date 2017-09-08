# Different trait compilation code done at different times.
# Hopefully this script will organize that so we can figure out what the best one is to use.

########## SLA and SSD: combine field-sampled and TRY data.

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


################## try data: PCA on a large number of traits.

# the following code is from Chelsea

library(tidyr)

DTtr <- read.csv('Data/trait data/traits22Nov.csv', stringsAsFactors=F)

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
