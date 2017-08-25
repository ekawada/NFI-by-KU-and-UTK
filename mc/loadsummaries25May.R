setwd('C:/Users/Q/Dropbox/nfi/Cluster/stan/output/may2017_subsample/summaries')
WAICs <- summaries <- list()
spp <- mods <- c()
for (i in dir()) {
  load(i)
  spp[i] <- as.numeric(substr(i,4,4))
  mods[i] <- as.numeric(substr(i,8,8))
  WAICs[[i]] <- WAIC
  summaries[[i]] <- summ
}

WAICvalues <- unlist(lapply(WAICs, '[', 1))
data.frame(sp=spp,mod=mods,waic=WAICvalues)


# extract data for species 

plot_data <- list()

for (i in 1:length(summaries)) {
  if (mods[i] >= 4) {
    Ts <- summaries[[i]]$summary[grep('T', dimnames(summaries[[i]]$summary)[[1]]), c('2.5%','50%','97.5%')]
    plot_data[[length(plot_data)+1]] <- data.frame(species=spp[i], model=mods[i], cimin=Ts[1], T=Ts[2], cimax=Ts[3])
  }
}

plot_data <- do.call('rbind',plot_data)

source('~/qutil.r')


library(ggplot2)
ggplot(plot_data, aes(y = T, ymin = cimin, ymax = cimax, x = species)) +
  geom_hline(yintercept=0, linetype = 'dotted') +
  geom_pointrange() +
  facet_wrap(~ model, scales='free_x', labeller = labeller(model = c('4'='relative fitness differences', '5'='stabilizing niche differences'))) +
  scale_x_continuous(labels=c('spruce','pine','birch','aspen','oak','alder','rowan'), breaks=1:7, expand=c(.2,.2)) +
  coord_flip() +
  ggtitle('Effects of neighbors on target-species tree growth') +
  theme_bw() + theme(panel.grid = element_blank())

ggsave('C:/Users/Q/Dropbox/nfi/figures/traitcoefficients2017May25.png', height=6,width=8,dpi=400)
