library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(cowplot)
library(gridExtra)

# import data
intradat <- read.csv('./Data/trait data/intratraits.csv')
oak <- read.csv('./Data/trait data/oak_leafextra.csv') #add in Norwegian oak sampled outside plots to supp.

#### INIDIVDUAL LEVEL PATTERNS ####

# bind together and rename species
sla_ind <- intradat %>% mutate(SLA = Area/Dryg*0.1) %>% group_by(species, plotID, treeID) %>% summarise(SLA_m=mean(SLA), SLA_sd=sd(SLA), SLA_n=n()) #removed long, lat, precip, temp from select
sla_ind <- droplevels(filter(sla_ind, species!= "B. pendula" & species!="S. caprea"))

oak_ind <- oak %>% mutate(SLA = Area/Dryg*0.1) %>% group_by(species, plotID, treeID) %>% summarise(SLA_m=mean(SLA), SLA_sd=sd(SLA), SLA_n=n()) #removed long, lat, precip, temp from select

all_ind <- bind_rows(sla_ind, oak_ind)
all_ind$species <- car::recode(all_ind$species, '"eik" = "Quercus"; "B. pubescens" = "Betula pubescens"; "P. sylvestris" = "Pinus sylvestris"; "P. abies" = "Picea abies"; "A. incana" = "Alnus incana"; "P. tremulus" = "Populus tremula"; "S. aucuparia" = "Sorbus aucuparia"')

write.csv(all_ind, './Data/trait data/indtraits_nor.csv')

#### SPECIES LEVEL PATTERNS ####
specvar <- indvar %>% group_by(species) %>% summarize(SLA_m=mean(mSLA), SLA_sd=sd(mSLA), LDMC_m=mean(mLDMC), LDMC_sd=sd(mLDMC), TH_m=mean(mTH), TH_sd=sd(mTH), N=n())


#### PLOT ####
## Means by temperature
p <- ggplot(indvar, aes(x=temp, y=mSLA, group=species)) +geom_point()
p1 <- p + geom_smooth(aes(group=species, color=species), method="lm", se=T) + theme_nor() +xlab('') + ylab('SLA (mm2/mg)') +
  scale_colour_brewer(type="qual", palette = "Dark2", guide=F)

p <- ggplot(indvar, aes(x=temp, y=mLDMC, group=species)) +geom_point()
p2 <- p + geom_smooth(aes(group=species, color=species), method="lm", se=T) + theme_nor() +xlab(expression(paste('Mean Annual Temperature ',~degree~C))) + ylab('LDMC (mg/g)') +
  scale_colour_brewer(type="qual", palette = "Dark2", guide=F)

p <- ggplot(indvar, aes(x=temp, y=mTH, group=species)) +geom_point()
p3 <- p + geom_smooth(aes(group=species, color=species), method="lm", se=T) + theme_nor() +xlab('') + ylab('Thickness (mm)') +
  scale_colour_brewer(type="qual", palette = "Dark2")

#get legend from p3
legend <- get_legend(p3)

#remove the legend from p3 (p1 and p2 already removed)
p3 <- p3 + theme(legend.position="none")

#arrange ggplot2 graphs with a specific width
grid.arrange(p1, p2, p3, legend, ncol=4, widths=c(1.5, 1.5, 1.5, 0.5))

## By precipitation
p <- ggplot(indvar, aes(x=precip, y=mSLA, group=species)) +geom_point()
p4 <- p + geom_smooth(aes(group=species, color=species), method="lm", se=T) + theme_nor() +xlab('') + ylab('SLA (mm2/mg)') +
  scale_colour_brewer(type="qual", palette = "Dark2", guide=F)

p <- ggplot(indvar, aes(x=precip, y=mLDMC, group=species)) +geom_point()
p5 <- p + geom_smooth(aes(group=species, color=species), method="lm", se=T) + theme_nor() +xlab('Mean Annual Precipitation (mm)') + ylab('LDMC (mg/g)') +
  scale_colour_brewer(type="qual", palette = "Dark2", guide=F)

p <- ggplot(indvar, aes(x=precip, y=mTH, group=species)) +geom_point()
p6 <- p + geom_smooth(aes(group=species, color=species), method="lm", se=T) + theme_nor() +xlab('') + ylab('Thickness (mm)') +
  scale_colour_brewer(type="qual", palette = "Dark2")

#get legend from p3
legend <- get_legend(p6)

#remove the legend from p3 (p1 and p2 already removed)
p6 <- p6 + theme(legend.position="none")

#arrange ggplot2 graphs with a specific width
grid.arrange(p4, p5, p6, legend, ncol=4, widths=c(1.5, 1.5, 1.5, 0.5))

#### FUNCTIONS ####
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}



 
