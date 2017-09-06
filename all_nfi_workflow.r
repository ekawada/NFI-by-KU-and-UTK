# Entire NFI workflow
# QDR 06 Sep 2017

# Steps required in analysis

### I. Processing tree data

# 1. compare treid with the azimuth and position of the tree. Get rid of plots that have too many discrepancies.
# 2. calculate basal area and annualized basal area increments from the dbh's of the trees
# 3. find mortality events
# 4. get rid of plots with the following issues: trees with missing locations or nonmatching IDs, partial plots, trees with the same DI in different locations
# 5. classify all but the most common 7 species as "other"
# 6. transform azimuth to degrees
# 7. calculate the distance from each tree to its neighbor
# 8. calculate sum of basal area/distance^2 for each neighbor of each tree, one sum for each species
# 9. adjust sums depending on the distance of the target tree from the center of the plot

### II. Processing climate data

### III. Processing trait data

### IV. Create bins for analyses that require binning

### V. Fit the neighbor trait model

### VI. Fit the (simpler) focal tree trait model

# Begin with new raw data

load('Data/tree data/Pre2017-09-01.RData')
