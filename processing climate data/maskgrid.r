# Can we clip these data frames to get rid of the ocean grid and make them smaller?
# Yes, yes we can.
library(raster)
norb <- getData('GADM', country = 'NOR', level = 0)

mmt <- read.csv('./Data/gridded/monthlymeantemp.csv')
mmint <- read.csv('./Data/gridded/monthlyavgmintemp.csv')
mmaxt <- read.csv('./Data/gridded/monthlyavgmaxtemp.csv')
precip <- read.csv('./Data/gridded/monthlyprecip.csv')



masknor <- function(DF, b=norb) {
  
  dfsp <- SpatialPixelsDataFrame(points=data.frame(x=DF$long, y=DF$lat), data=DF[,-(1:2)],
                                 proj4string=CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0'))
  dfbrick <- brick(dfsp)
  dfcrop <- mask(dfbrick, b)
  df2 <- cbind(data.frame(long=DF$long, lat=DF$lat), dfcrop@data@values)
  df2 <- df2[complete.cases(df2),]
  return(df2)
}

mmtdf2 <- masknor(DF=mmt)
mmintdf2 <- masknor(DF=mmint)
mmaxtdf2 <- masknor(DF=mmaxt)
precipdf2 <- masknor(DF=precip)

# for some reason these are all screwed up so we have to transform them.
trlat <- function(x) {x$lat <- max(x$lat)+min(x$lat)-x$lat; x}
mmtdf2<-trlat(mmtdf2)
mmintdf2<-trlat(mmintdf2)
mmaxtdf2<-trlat(mmaxtdf2)
precipdf2<-trlat(precipdf2)


write.table(mmtdf2, file='~/meant_mask.csv', row.names=FALSE, sep = ',')
write.table(mmintdf2, file='~/avgmint_mask.csv', row.names=FALSE, sep = ',')
write.table(mmaxtdf2, file='~/avgmaxt_mask.csv', row.names=FALSE, sep = ',')
write.table(precipdf2, file='~/precip_mask.csv', row.names=FALSE, sep = ',')


