# Code to interpolate (Norwegian) weather station data into a grid.
# See http://www.geo.ut.ee/aasa/LOOM02331/R_idw_interpolation.html

library(sp)
library(maptools)
library(ggplot2)
library(gstat)

nclimate <- transform(nclimate, x=longitude, y=latitude) # Duplicate nclimate
nclimate <- subset(nclimate, !is.na(x) & !is.na(y))

# Get data ranges and create an empty grid to fill the interpolation values in.
x.range <- range(nclimate$x, na.rm=T)  # min/max longitude of the interpolation area
y.range <- range(nclimate$y, na.rm=T)  # min/max latitude of the interpolation area

# Create grid from ranges
grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 0.1), 
                   y = seq(from = y.range[1], to = y.range[2], by = 0.1))                                                                          
coordinates(grd) <- ~x + y
gridded(grd) <- TRUE


# Now we need to split nclimate by year and month so we can make grids for each time point
nclimtime <- split(nclimate, f = list(nclimate$Year, nclimate$Month))

# Test this method using a randomly chosen month/year from nclimtime (May 1989)
test <- nclimtime[[200]]

test2 <- subset(test, !is.na(TAM))
test2 <- SpatialPointsDataFrame(coords=cbind(test2$x, test2$y), data=test2[,1:26])


# Diagnostic plot, looks great!
plot(grd, cex = 1.5, col = "grey")
points(test2, pch = 1, col = "red", cex = 1)

# Run the number crunching interpolation for one of the data coluns to test
# Inverse Distance Weighted interpolation (idw)
# try TAM (monthly average temperature)
idw.tam <- idw(formula = TAM ~ 1, locations = test2, newdata = grd, na.action = na.omit) # apply idw model for the data
idw.tam.out <- as.data.frame(idw.tam)
names(idw.tam.out) <- c('long', 'lat', 'TAM_May_1989', 'var')

# Diagnostic plot showing interpolated values on grid.

county <- read.csv('county.csv') # Load ggplot2-compatible county map that I created

ggplot() + geom_tile(data=idw.tam.out, aes(x=long, y=lat, fill = TAM_May_1989)) +
           scale_fill_gradient(low = 'cyan', high = 'orange') +
           geom_point(data=test, aes(x=longitude, y=latitude), shape = 21, color = 'red') +
           geom_path(data=county, aes(x=long, y=lat, group=group)) +
           coord_equal()



##################
# Now to do the interpolation for all the data columns.

interp <- function(DF, varname) {
  DF$V <- DF[, varname]
  DF2 <- subset(DF, !is.na(V))
  DF2 <- SpatialPointsDataFrame(coords=cbind(DF2$x, DF2$y), data=DF2[,!(names(DF2) %in% c('x', 'y'))])
  idw.1 <- idw(formula = V ~ 1, locations = DF2, newdata = grd, na.action = na.omit) 
  idw.out <- as.data.frame(idw.1)[,-4]
  longname <- paste(varname, DF2$Month[1], DF2$Year[1], sep='_')
  names(idw.out) <- c('long', 'lat', longname)
  cat('\n', longname)
  idw.out
}

# This works but makes a fucking huge object.
# Right now I will just use the following variables:
# RR (monthly precip, mm)
# TAM (monthly mean temp, C)
# TANM (monthly average min temp, C)
# TAXM (monthly average max temp, C)
tam.all <- llply(nclimtime, interp, varname = 'TAM')
tanm.all <- llply(nclimtime, interp, varname = 'TANM')
taxm.all <- llply(nclimtime, interp, varname = 'TAXM')
rr.all <- llply(nclimtime, interp, varname = 'RR')

# Now put everything back into gigantic data frames.
library(plyr)
tam.df <- join_all(tam.all, by = c('long', 'lat'))
write.table(tam.df, file = '~/monthlymeantemp.csv', row.names=FALSE, sep = ',')
tanm.df <- join_all(tanm.all, by = c('long', 'lat'))
write.table(tanm.df, file = '~/monthlyavgmintemp.csv', row.names=FALSE, sep = ',')
taxm.df <- join_all(taxm.all, by = c('long', 'lat'))
write.table(taxm.df, file = '~/monthlyavgmtemp.csv', row.names=FALSE, sep = ',')
rr.df <- join_all(rr.all, by = c('long', 'lat'))
write.table(rr.df, file = '~/monthlyprecip.csv', row.names=FALSE, sep = ',')