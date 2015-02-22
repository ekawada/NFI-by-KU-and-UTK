# This creates a ggplot2-compatible map of mainland Norway's counties.
# Useful for any plotting of Norway-wide data.

# Load Norwegian county map into R
library(raster)
library(plyr)
library(ggplot2)
norborder <- getData('GADM', country = 'NOR', level = 1) # level 1 is counties (highest administrative division), but you can also get townships and such.

# Convert the spatial polygons dataframe into something ggplot2 can make sense of.
# Code courtesy of Hadley.
norborder@data$id <- rownames(norborder@data)
norborder.points <- fortify(norborder, NAME_1="id")
norborder.df <- join(norborder.points, norborder@data, by = 'id')

# Save this border for future use because it's very useful.
write.table(norborder.df, file = 'county.csv', row.names = FALSE, sep = ',')

# No counties:
normap <- getData('GADM', country = 'NOR', level = 0)
normap@data$id <- rownames(normap@data)
normap.points <- fortify(normap, ID_0='id')

ggplot(normap.points, aes(x=long,y=lat,group=group)) + geom_path()