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