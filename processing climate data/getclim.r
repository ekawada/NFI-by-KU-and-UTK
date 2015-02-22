# Extract climate data for a single coordinate.

# Convert the data frame to raster.
df2ras <- function(dataframe) {
  library(raster)
  coordinates(dataframe) <- ~ long + lat
  gridded(dataframe) <- TRUE
  brick(dataframe)
}

# These functions can be used on an extracted row.

ordc <- function(cdata) {
  ord <- NULL
  for (i in 1:45) ord <- c(ord, seq(i, 540, 45))
  cdata[ord]
}

yavg <- function(cdata) {
  res <- NULL
  for (i in seq(1, 540, 12)) res <- c(res, mean(cdata[i:(i+11)]))
  res
}

ysum <- function(cdata) {
  res <- NULL
  for (i in seq(1, 540, 12)) res <- c(res, sum(cdata[i:(i+11)]))
  res
}