# Read eKlima data

nclimate <- read.csv('./Data/nclimate.csv', header = TRUE)

# Plot locations of stations just to check them.

coords <- unique(cbind(nclimate$lat, nclimate$long))
plot(coords[,2], coords[,1])

#############################################
# Original combining of norwegian climate data
# Do not need to re-run if you load nclimate.csv
#############################################
d1 <- read.csv('./Data/norw climate data/counties0105.csv', header=TRUE)
d2 <- read.csv('./Data/norw climate data/counties0610.csv', header=TRUE)
d3 <- read.csv('./Data/norw climate data/counties1115.csv', header=TRUE)
d4 <- read.csv('./Data/norw climate data/counties1619.csv', header=TRUE)
d1b <- rbind(d2,d3,d4)
d1b <- d1b[, names(d1)]
nclimate <- rbind(d1, d1b)

source('~/R/myreadods.r')
cmeta <- read.ods('./Data/norw climate data/climate metadata.ods')
cmetaCodes <- read.ods('./Data/norw climate data/climate metadata.ods', sheet='Codes')

stations <- match(nclimate$St.no, cmeta$Stnr)
nclimate <- transform(nclimate,
                      altitude = cmeta$Altitude[stations],
                      latitude = cmeta$Latitude[stations],
                      longitude = cmeta$Longitude[stations],
                      stationName = cmeta$Name[stations],
                      Month = floor(Month),
                      Year = round((Month - floor(Month))*10000))

write.table(nclimate, file='./Data/nclimate.csv', sep = ',', row.names = FALSE)

