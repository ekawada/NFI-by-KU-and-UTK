#packages
library(plyr)
library(dplyr)
library(tidyr)
library(reshape2) 

# set working directory to 'nfi'
# Read the very large data file we got from TRY.
dat <- read.delim('./data/trait data/457.txt', stringsAsFactors=F, fileEncoding="latin1")

# swap out names to norwegian
splist <- c('Picea abies', 'Pinus sylvestris', 'Betula pubescens', 'Populus tremula',
             'Quercus petraea', 'Quercus robur', 'Alnus incana', 'Sorbus aucuparia')

#filter dat by species name and DataID 4 and 12 (SLA and SSD)
traits <- dat %>%
  filter(AccSpeciesName %in% splist) %>%
  mutate(AccSpeciesName = revalue(AccSpeciesName, c("Quercus robur" = "Quercus","Quercus petraea" = "Quercus"))) %>%
  filter(DataID==c(4,12)) %>%
  select(AccSpeciesName, ObservationID, DataName, UnitName, StdValue)

#filter locs by obsids in traits
trylocs <- dat %>%
  filter(DataID==c(59,60)) %>% 
  select(ObservationID, DataName, StdValue) %>%
  spread(DataName, StdValue) %>%
  rename(longitude=Longitude, latitude=Latitude) 

#find traits of interest, create variables to filter
traitlocs <- left_join(traits, trylocs, by="ObservationID") 
trytraits <- traitlocs[complete.cases(traitlocs[6:7]),] #removes 9 with NAs

#get extra species
extra <- dat %>% filter(AccSpeciesName %in% c("Picea abies","Alnus incana","Sorbus aucuparia", "Betula pubescens")) %>%
  filter(DataName=="Wood density; stem specific density") %>% 
  select(AccSpeciesName, ObservationID, DataName, UnitName, StdValue)

tryall <- trytraits %>% bind_rows(., extra)
 

write.csv(tryall, './data/trait data/trytraits.csv')


