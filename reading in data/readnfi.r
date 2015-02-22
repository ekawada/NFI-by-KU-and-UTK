# initial exploration and processing of Norwegian data

load('./Data/tree data/Pre2015-02-13.RData') # Creates data frames tr, fl
spcode <- read.csv('./Data/tree data/nor_splist.csv')

# Translate column names from Bokmal to English!
tEnglishNames <- c('plotID','partialPlotClass','year','species','distance','azimuth','measHeight','estHeight','dbh','crownHeight','crownColor','crownDensity','harvestClass')
fEnglishNames <- c('plotID','partialPlotClass','partialPlotProp','year','commune','soilDepth','areaType','landUse','soilType','crownCover')

tre <- tr
fle <- fl
names(tre) <- tEnglishNames
names(fle) <- fEnglishNames