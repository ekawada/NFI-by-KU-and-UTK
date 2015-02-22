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
incr <- dlply(tre, .(treeID), function(x) {
  x <- x[order(x$year), ]
  list(treeID=x$treeID[1], incr=x$dbh[-1] - x$dbh[-nrow(x)], year=x$year[-1])
},
.progress='text')

incrdat <- matrix(NA, nrow=length(incr), ncol=length(1987:2013)+1)
for (i in 1:length(incr)) {
  incrdat[i, 1] <- incr[[i]]$treeID
  y <- incr[[i]]$year - 1985
  incrdat[i, y] <- incr[[i]]$incr
}

# get rid of "NA" row
incrdat <- incrdat[-i,]
incrnames <- c('treeID', paste0('incr', 1987:2013))
incrdat <- as.data.frame(incrdat)
names(incrdat) <- incrnames

write.table(incrdat, file='./Data/tree data/incr.csv', sep=',', row.names=FALSE)