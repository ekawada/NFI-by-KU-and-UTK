# Plot some of the model output.
library(ggplot2)
library(plyr)
for (i in 1:length(M)) M[[i]]$time <- i
M2 <- do.call('rbind', M)
M2$basalA <- with(M2, w * pi * (dbh/2)^2) * (1/10000)

pba <- ggplot(ddply(M2, .(time, species), summarize, basalArea = sum(basalA),
                    abund = sum(w)))
pba + geom_line(aes(x=time, y=basalArea, color=factor(species)), size=2) + theme_bw()
pba + geom_line(aes(x=time, y=abund, color=factor(species)), size=2) + theme_bw()
