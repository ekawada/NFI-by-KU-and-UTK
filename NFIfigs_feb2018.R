#### FIGURES FOR NFI DRAFT FEB 20 2018 ####

library(ggplot2)
library(dplyr)
library(ggthemes)

#### DISTRIBUTION FIGS ####

tru <- read.csv('./Data/tree data/treused.csv')
covariates <- read.csv('./Data/tree data/covariates.csv')
spcode <- read.csv('./Data/tree data/nor_splist.csv')

tru$basalarea <- pi*(tru$dbh/2000)^2
plotba <- ddply(tru, .(plotID, year, species), summarize, basalarea = sum(basalarea, na.rm = TRUE), .progress = 'text')
plotba <- merge(plotba, covariates)
plotba$spname <- spcode$sp_lat[match(plotba$species, spcode$code)]
plotba$sp_nor <- spcode$sp_nor[match(plotba$species, spcode$code)]
plotbaCurrent <- subset(plotba, year >= 2009)

spp <- c(1,10,30,32,50,40,53,52,31,55)
spp <- c('Picea abies','Pinus sylvestris','Betula pubescens','Populus tremula','Alnus incana','Quercus','Sorbus aucuparia')
speng <- c('Norway spruce', 'Scots pine', 'White birch', 'Aspen', 'Grey alder', 'Oak', 'Rowan', 'Pussy willow', 'Silver birch', 'Hazelnut')
dat <- plotbaCurrent[plotbaCurrent$spname %in% spp,]
dat$spname <- factor(dat$spname, levels = spp)

no<-map_data("world", region="Norway(?!:Svalbard)")
gg <- ggplot() 
gg <- gg + geom_map(data=no, map=no, aes(map_id=region), color="darkgrey", fill="white", size=0.5)
gg <- gg + geom_point(data=dat, aes(x=long, y=lat), color="black", fill="darkgrey", size=0.5, alpha=1/10) + 
  coord_equal() + theme_bw() + theme(legend.position="none") + 
  theme(text = element_text(family = 'Helvetica'), strip.background = element_blank(), legend.position = 'none') +
  xlab("Longitude") + ylab("Latitude") + facet_wrap(~spname, ncol = 2) 
gg

ggsave('~/Dropbox/Projects/Norway Forests/nfi/manuscript/Feb2018/FigS1.pdf', height=7,width=4,dpi=400)

gg <- ggplot() 
gg <- gg + geom_map(data=no, map=no, aes(map_id=region), color="darkgrey", fill="white", size=0.5)
gg <- gg + geom_point(data=dat, aes(x=long, y=lat), color="black", fill="darkgrey", size=0.5, alpha=1/10) + 
  coord_equal() + theme_bw() + theme(legend.position="none") + 
  theme(text = element_text(family = 'Helvetica'), strip.background = element_blank(), legend.position = 'none') +
  xlab("Longitude") + ylab("Latitude")
gg

ggsave('~/Dropbox/Projects/Norway Forests/nfi/manuscript/Feb2018/Fig11.pdf', height=7,width=6,dpi=400)


datd <- dat %>% group_by(long, lat, plotID) %>% filter(year == max(year)) %>% summarize(stems=n())
no<-map_data("world", region="Norway(?!:Svalbard)")
gg <- ggplot() 
gg <- gg + geom_map(data=no, map=no, aes(map_id=region), color="darkgrey", fill="white")
gg <- gg + geom_hex(stat="binhex", data=datd, aes(x=long, y=lat, col=stems), bins=50) +
  coord_equal() + theme_bw() + theme(legend.position="none") +
  scale_bar(lon = 20, lat = 59,
            distance_lon = 50, distance_lat = 10,
            distance_legend = 50, dist_unit = "km", orientation = FALSE) +
  theme(text = element_text(family = 'Helvetica'), strip.background = element_blank(), legend.position = 'none') +
  xlab("Longitude") + ylab("Latitude")+
  scale_fill_gradient(low = "grey", high = "black")
gg

ggsave('~/Dropbox/Projects/Norway Forests/nfi/manuscript/Feb2018/Fig1.pdf', height=6,width=8,dpi=400)


#Older version with points
gg <- gg + geom_point(data=datd, aes(x=long, y=lat, size=stems/1000), color='black', alpha=0.25) + 
  coord_equal() + theme_bw() + theme(legend.position="none") +
  scale_bar(lon = 20, lat = 59,
            distance_lon = 50, distance_lat = 10,
            distance_legend = 50, dist_unit = "km", orientation = FALSE) +
  theme(text = element_text(family = 'Helvetica'), strip.background = element_blank(), legend.position = 'none') +
  xlab("Longitude") + ylab("Latitude") 

#### CLIMATE FIGS ####
# Load summaries
setwd('~/Dropbox/Projects/Norway Forests/nfi')
binsum <- list()

for (i in 1:9) {
  load(paste0('Cluster/stan/output/feb2018_better/indiv_summary/bin',i,'_summ.r'))
  binsum[[i]] <- summ
}

tempbin <- rep(c('lowt','midt','hight'),each=3)
precipbin <- rep(c('lowp','midp','highp'),times=3)

binsumabbr <- list()

for (i in 1:9) {
  binsumabbr[[i]] <- as.data.frame(binsum[[i]][[1]][grep('beta|gamma|lambda', row.names(binsum[[i]][[1]])), ])
  binsumabbr[[i]]$tempbin <- tempbin[i]
  binsumabbr[[i]]$precipbin <- precipbin[i]
}

binsumabbr <- lapply(binsumabbr, as.data.frame)
for (i in 1:9) {
  binsumabbr[[i]]$parname <- row.names(binsumabbr[[i]])
}

binsumall <- do.call('rbind', binsumabbr)

names(binsumall)[4:8] <- c('q025','q25','q50','q75','q975')

# Get rid of oak in low temp areas (it isn't there so coeff is meaningless)
#binsumall <- subset(binsumall, !(parname %in% c('beta_size[5]','beta_temp[5]','beta_prec[5]') & tempbin == 'low_temp'))

# Figures
hl <- geom_hline(linetype='dotted', color='darkgrey', yintercept = 0)
th <- theme_bw() + theme(text = element_text(family = 'Helvetica'), strip.background = element_blank(), legend.position = 'none')
scm <- scale_color_manual(values = c('FALSE'='darkgrey','TRUE'="black"))

spnames <- c('P. abies','P. sylvestris','B. pubescens','P. tremula','Quercus sp.','A. incana','S. aucuparia','Other hardwood')
spnumnames <- c('1'='P. abies','2'='P. sylvestris','3'='B. pubescens','4'='P. tremula','5'='Quercus sp.','6'='A. incana','7'='S. aucuparia','8'='Other hardwood')
binlab <- labeller(precipbin = c(highp = 'High precipitation', midp = 'Mid precipitation', lowp = 'Low precipitation'),
                   tempbin = c(hight = 'High temperature', midt = 'Mid temperature', lowt = 'Low temperature'))

dodge <- position_dodge(width=1.5)

# Reorder facets.
binsumall <- mutate(binsumall,
                    tempbin = factor(tempbin, levels = c('hight','midt','lowt')),
                    precipbin = factor(precipbin, levels = c('lowp','midp','highp')),
                    not_zero = (q025<0 & q975<0) | (q025>0 & q975>0)
)

binsumall$species <- factor(stringr::str_extract_all(binsumall$parname, pattern='[0-9]', simplify=TRUE), levels = as.character(8:1))

#Fig S4
pbetasize <- ggplot(subset(binsumall, grepl('beta_size', parname)), aes(y = q50, ymin = q025, ymax = q975, x = species)) +
  geom_hline(yintercept=0, linetype = 'dotted') +
  hl + scm +
  geom_pointrange(aes(color = not_zero), position=dodge) + th +
  labs(y = 'Tree size parameter estimate') +
  scale_x_discrete(name= 'Species', labels=spnumnames) +
  facet_grid(tempbin ~ precipbin, labeller=binlab) +
  coord_flip() 
pbetasize
ggsave('~/Dropbox/Projects/Norway Forests/nfi/manuscript/Feb2018/FigS4.pdf', height=6,width=7,dpi=400)

#Fig S2
pbetatemp <- ggplot(subset(binsumall, grepl('beta_temp', parname)), aes(x=species, ymin=q025, y=q50, ymax=q975)) +
  geom_hline(yintercept=0, linetype = 'dotted') +
  hl + scm +
  geom_pointrange(aes(color = not_zero), position=dodge) + th +
  labs(y = 'Temperature parameter estimate') +
  scale_x_discrete(name= 'Species', labels=spnumnames) +
  facet_grid(tempbin ~ precipbin, labeller=binlab) +
  coord_flip() 
pbetatemp
ggsave('~/Dropbox/Projects/Norway Forests/nfi/manuscript/Feb2018/FigS2.pdf', height=6,width=7,dpi=400)

#Fig S3
pbetaprec <- ggplot(subset(binsumall, grepl('beta_prec', parname)), aes(x=species, ymin=q025, y=q50, ymax=q975)) +
  geom_hline(yintercept=0, linetype = 'dotted') +
  hl + scm +
  geom_pointrange(aes(color = not_zero), position=dodge) + th +
  labs(y = 'Precipitation parameter estimate') +
  scale_x_discrete(name= 'Species', labels=spnumnames) +
  facet_grid(tempbin ~ precipbin, labeller=binlab) +
  coord_flip() 
pbetaprec

ggsave('~/Dropbox/Projects/Norway Forests/nfi/manuscript/Feb2018/FigS3.pdf', height=6,width=7,dpi=400)


#### COMPETITION COEFFICIENTS ####
binlab2 <- c('Low temp\nlow precip', 'Low temp\nhigh precip', 'High temp\nlow precip', 'High temp\nhigh precip')

ptraitcoef <- ggplot(subset(binsumall, tempbin!='all' & grepl('gamma', parname)), aes(x=interaction(precipbin, tempbin), ymin=q025, y=q50, ymax=q975)) +
  geom_hline(yintercept=0, linetype = 'dotted') +
  hl + scm +
  geom_pointrange(aes(color = not_zero), position=dodge) + th +
  coord_flip() +
  scale_x_discrete(name = 'Climate bin') +
  labs(y = 'Parameter estimate') +
  facet_grid(~ parname, labeller = labeller(parname = c('gamma[1]' = 'Specific\nleaf area', 'gamma[2]' = 'Stem\nspecific density')))


pcompcoef <- ggplot(subset(binsumall, tempbin!='all' & grepl('lambda', parname)), aes(x=interaction(precipbin, tempbin), ymin=q025, y=q50, ymax=q975)) +
  geom_hline(yintercept=0, linetype = 'dotted') +
  hl + scm +
  geom_pointrange(aes(color = not_zero), position=dodge) + th +
  coord_flip() +
  scale_x_discrete(name = 'Climate bin') +
  labs(y = 'Competition parameter estimate') +
  facet_grid(~ parname, labeller=labeller(parname=c('lambda' = ' ')))

#comps <- subset(binsumall, tempbin!='all' & grepl('lambda', parname))
#summary(lm(mean~tempbin*precipbin, data=comps))

# ADD LINES CODE TO TRAIT AND COMP EFFECTS #
library(gridExtra)
library(gtable)
library(grid)

#Fig 3 Trait Coefficients
plotobj <- ptraitcoef + 
  scale_x_discrete(name= "Climate bin", labels = rep(c('Low Precip.', 'Mid Precip.','High Precip.'), 3)) +
  theme(plot.margin=unit(c(1,1,1,1.05),"cm")) +
  theme(axis.title.y = element_text(vjust=150)) 

g=ggplotGrob(plotobj)

g<-gtable_add_grob(g, grobTree(textGrob(expression(paste(underline("Low Temperature"))), x=0.35, y=0.18, rot=90, gp = gpar(fontfamily = 'Helvetica', col="grey40", cex=0.8)),
                               textGrob(expression(paste(underline("Mid Temperature"))), x=0.35, rot=90, gp = gpar(fontfamily = 'Helvetica', col="grey40", cex=0.8)), 
                               textGrob(expression(paste(underline("High Temperature"))), x=0.35, y=0.83, rot=90, gp = gpar(fontfamily = 'Helvetica', col="grey40", cex=0.8))), t=7, l=2.25)

grid.draw(g)   


#Figure 4 Competition Coefficients
plotobj <- pcompcoef + 
  scale_x_discrete(name= "Climate bin", labels = rep(c('Low Precip.', 'Mid Precip.','High Precip.'), 3)) +
  theme(plot.margin=unit(c(1,1,1,1.05),"cm")) +
  theme(axis.title.y = element_text(vjust=150)) 

g=ggplotGrob(plotobj)

g<-gtable_add_grob(g, grobTree(textGrob(expression(paste(underline("Low Temperature"))), x=0.35, y=0.18, rot=90, gp = gpar(fontfamily = 'Helvetica', col="grey40", cex=0.8)),
                               textGrob(expression(paste(underline("Mid Temperature"))), x=0.35, rot=90, gp = gpar(fontfamily = 'Helvetica', col="grey40", cex=0.8)), 
                               textGrob(expression(paste(underline("High Temperature"))), x=0.35, y=0.83, rot=90, gp = gpar(fontfamily = 'Helvetica', col="grey40", cex=0.8))), t=7, l=2.25)

grid.draw(g) 



#### TABLE 1: INFORMATION CRITERIONS ####

IC <- read.csv('./Cluster/stan/output/feb2018_better/ICs.csv')
library(tidyr)
rar <- IC %>% group_by(species) %>% select(species, model, looic) %>%
  group_by(species) %>% spread(model, looic)

rar <- IC %>% group_by(species) %>% select(species, model, waic) %>%
  group_by(species) %>% spread(model, waic)

best_mod <- function(x) {
  data.frame(best_model_looic = x$model[which.min(x$looic)],
             best_model_waic = x$model[which.min(x$waic)],
             best_looic = min(x$looic),
             best_waic = min(x$waic))
}

bestmods <- IC %>%
  group_by(species) %>%
  do(best_mod(.))

ICs <- left_join(IC, bestmods)

