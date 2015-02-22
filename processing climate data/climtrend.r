# Climate trends over time in Norway

mmt <- read.csv('./Data/gridded/meant_mask.csv')
mmint <- read.csv('./Data/gridded/avgmint_mask.csv')
mmaxt <- read.csv('./Data/gridded/avgmaxt_mask.csv')
precip <- read.csv('./Data/gridded/precip_mask.csv')

getmonth <- function(dat, month, year) {
  cnames <- strsplit(names(dat)[-(1:2)], '_')
  dat[,c(FALSE,FALSE,unlist(lapply(cnames, function(x) x[2]==month & x[3]==year)))]
}

# get all the mean values for different months.
ord <- NULL
for (i in 1:45) ord <- c(ord, seq(i, 540, 45))

mmtmean <- apply(mmt[,-(1:2)], 2, mean)[ord]
mmintmean <- apply(mmint[,-(1:2)], 2, mean)[ord]
mmaxtmean <- apply(mmaxt[,-(1:2)], 2, mean)[ord]
precipmean <- apply(precip[,-(1:2)], 2, mean)[ord]

allprecipsum <- matrix(NA, nrow=nrow(precip), ncol=0) 
for (i in seq(1, 540, 12)) allprecipsum <- cbind(allprecipsum, apply(precip[-(1:2)], 1, function(x) sum(x[i:(i+11)])))

ymtmean <- NULL
for (i in seq(1, 540, 12)) ymtmean <- c(ymtmean, mean(mmtmean[i:(i+11)]))
summary(lm(ymtmean ~ I(1970:2014))) # Frequentist analysis says global warming is real!
ymintmean <- NULL
for (i in seq(1, 540, 12)) ymintmean <- c(ymintmean, mean(mmintmean[i:(i+11)]))
summary(lm(ymintmean ~ I(1970:2014)))
ymaxtmean <- NULL
for (i in seq(1, 540, 12)) ymaxtmean <- c(ymaxtmean, mean(mmaxtmean[i:(i+11)]))
summary(lm(ymaxtmean ~ I(1970:2014)))
yprecipmean <- NULL
for (i in seq(1, 540, 12)) yprecipmean <- c(yprecipmean, mean(precipmean[i:(i+11)]))
summary(lm(yprecipmean ~ I(1970:2014)))
yprecipsum <- NULL
for (i in seq(1, 540, 12)) yprecipsum <- c(yprecipsum, sum(precipmean[i:(i+11)]))
summary(lm(yprecipsum ~ I(1970:2014)))

plot(mmtmean, type = 'l')
plot(mmintmean, type = 'l')
plot(mmaxtmean, type = 'l')
plot(precipmean, type = 'l')

plot(1970:2014, ymtmean, type='l', main='Norwegian average land temperature')
abline(lm(ymtmean ~ I(1970:2014)), col='red', lwd=2)

library(ggplot2)
tdat <- data.frame(year=1970:2014, meantemp=ymtmean, precip=precipmean)
ggplot(tdat, aes(x=year, y=meantemp)) + geom_line(size=1.5) + stat_smooth(method='lm', size=1.5) +
  theme_bw() +
  ggtitle('Norwegian average land temperature 1970-2014')
ggsave('./figures/temptrend.pdf', height = 5, width = 6)

# Map figures.
county <- read.csv('county.csv') # Load ggplot2-compatible county map that I created

ggplot() + geom_tile(data=mmt, aes(x=long, y=lat, fill = TAM_7_2014)) +
  scale_fill_gradient(low = 'cyan', high = 'orange') +
  geom_path(data=county, aes(x=long, y=lat, group=group)) +
  coord_equal() +
  ggtitle('Average July 2014 mean temp')
ggsave('./figures/meantemp_Jul2014.pdf', height=6, width=6)

ggplot() + geom_tile(data=mmt, aes(x=long, y=lat, fill = TAM_12_2014)) +
  scale_fill_gradient(low = 'cyan', high = 'orange') +
  geom_path(data=county, aes(x=long, y=lat, group=group)) +
  coord_equal() +
  ggtitle('Average December 2014 mean temp')
ggsave('./figures/meantemp_Dec2014.pdf', height=6, width=6)


precip$precip2014 <- allprecipsum[,45]
ggplot() + geom_tile(data=precip, aes(x=long, y=lat, fill = precip2014)) +
  scale_fill_gradient(low = 'green', high = 'red') +
  geom_path(data=county, aes(x=long, y=lat, group=group)) +
  coord_equal() +
  ggtitle('Total 2014 precip in mm')
ggsave('./figures/precip_2014.pdf', height=6, width=6)



############### better figures, for powerpoint purposes ####################


ggplot() + geom_tile(data=mmt, aes(x=long, y=lat, fill = TAM_7_2014)) +
  scale_fill_gradient(limits=c(0,25), low = 'cyan', high = 'orange', name = 'Temperature (C)') +
  geom_path(data=normap.points, aes(x=long, y=lat, group=group)) +
  coord_equal() + theme_bw() + 
  theme(axis.title.x=element_blank(), axis.title.y=element_blank())
ggsave('./figures/NORjul2014temp.png', height = 3, width = 6)

ggplot() + geom_tile(data=mmt, aes(x=long, y=lat, fill = TAM_7_1970)) +
  scale_fill_gradient(limits = c(0, 25), low = 'cyan', high = 'orange', name = 'Temperature (C)') +
  geom_path(data=normap.points, aes(x=long, y=lat, group=group)) +
  coord_equal() + theme_bw() + 
  theme(axis.title.x=element_blank(), axis.title.y=element_blank())
ggsave('./figures/NORjul1970temp.png', height = 3, width = 6)


#precip$precip2014 <- allprecipsum[,45]
ggplot() + geom_tile(data=precip, aes(x=long, y=lat, fill = precip2014)) +
  scale_fill_gradient(low = 'green', high = 'purple', name = 'Precipitation (mm)') +
  geom_path(data=normap.points, aes(x=long, y=lat, group=group)) +
  coord_equal() + theme_bw() + 
  theme(axis.title.x=element_blank(), axis.title.y=element_blank())
ggsave('./figures/NOR2014precip.png', height = 3, width = 6)
