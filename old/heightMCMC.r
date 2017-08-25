# initial exploration and processing of Norwegian data

load('./Data/tree data/Pre2015-02-13.RData') # Creates data frames tr, fl
spcode <- read.csv('./Data/tree data/nor_splist.csv')

# Translate column names from Bokmal to English!
tEnglishNames <- c('plotID','partialPlotClass','year','species','distance','azimuth','measHeight','estHeight','dbh','crownHeight','crownColor','crownDensity','harvestClass')
fEnglishNames <- c('plotID','partialPlotClass','partialPlotProp','year','commune','soilDepth','areaType','landUse','soilType','crownCover')


# Graph showing allometric relationship for each species.
library(ggplot2)
ggplot(tr, aes(x = brysth.dia, y = trehoyde)) + geom_point() + facet_wrap(~ treslag) + theme_classic()

# Simple model to estimate parameters for height allometry.

heightmodel <- '
model {
  # likelihood
  for (i in 1:n) {
    height[i] ~ dnorm(x[i], tau[species[i]])
    x[i] <- alpha[species[i]] * dbh[i] ^ beta[species[i]]
  }
  # priors
  for (j in 1:nspp) {
    alpha[j] ~ dgamma(1,1)
    beta[j] ~ dgamma(1,1)
    tau[j] <- 1/(sigma[j]^2)
    sigma[j] ~ dunif(0, 100)
  }
}'

heightdata <- with(subset(tr, !is.na(trehoyde)), list(height=trehoyde, dbh=brysth.dia, species=plyr::mapvalues(factor(treslag), levels(factor(treslag)), 1:length(unique(treslag))), n=length(trehoyde), nspp=length(unique(treslag))))

library(rjags)
heightinits <- function(nspp) list(alpha=rgamma(nspp,1,1), beta=rgamma(nspp,1,1), sigma=runif(nspp,0,100))
heightfit <- jags.model(textConnection(heightmodel), data=heightdata, n.adapt=1000, n.chain=3, inits=heightinits(heightdata$nspp))
heightsample <- coda.samples(heightfit, c('alpha','beta','tau'), n.iter=9999, thin=10)
heightsumm <- summary(heightsample)

source('DBDA2E-utilities.r')
diagMCMC(heightsample, 'alpha[1]')

#####################
# Match the species IDs with the parameters output from the original MCMC chain.
growthpars <- as.data.frame(heightsumm[[2]])
names(growthpars) <- c('q025','q25','q50','q75','q975')
growthpars <- transform(growthpars, par=rep(c('alpha','beta','tau'),each=25),
                        species=levels(factor(subset(tr, !is.na(trehoyde))$treslag)))
library(ggplot2)
ggplot(growthpars, aes(x = species, y = q50, ymin = q025, ymax = q975)) + 
  geom_errorbar() + geom_point() +
  theme_bw() +
  facet_grid(par ~ ., scales = 'free_y')

######################
plot1 <- subset(tr, flateid=='B51075')
plot1$qcalcheight <- as.numeric(apply(plot1, 1, function (x) growthpars$q50[which(growthpars$par=='alpha' & growthpars$species==x[4])])) * plot1$brysth.dia ^ as.numeric(apply(plot1, 1, function (x) growthpars$q50[which(growthpars$par=='beta' & growthpars$species==x[4])]))

# tree abundance in plots across years
library(plyr)
nrows <- ddply(tr, .(flateid, sesong), nrow)
# number of years each plot is recorded.
table(ddply(nrows, .(flateid), nrow)$V1)

# calculate recruitment and mortality in a plot.
# first get matrices for each species by size class.
# define size classes as 50-mm wide bins (50-100; 100-150; 150-200; etc.)

cmins <- c(seq(50, 500, 50), 1000)
tr <- transform(tr, sizeclass = cut(brysth.dia, breaks=cmins, right=FALSE))
trwhole <- subset(tr, hel.delt.flate==0) # Uses only whole 250 m2 plots.

library(plyr)
trclass <- ddply(trwhole, .(flateid, sesong, treslag, sizeclass), nrow,
                 .progress = 'text')
levels(trclass$sizeclass) <- 1:10
trclass <- transform(trclass, 
                     plotnum = (1:length(unique(trclass$flateid)))[factor(flateid)])

#recruitment <- ddply(subset(trclass, classnum==1), .(flateid, treslag), function(x) {
#  recr <- NULL
#  for (i in 1:nrow(x)) {
#    if (x$sesong[i] != min(x$sesong)) {
#      recr <- c(recr, )
#    }
#  }
#}


####################
# mcmc for the itd parameters given all the data we have now.
# Species left out 11,20,21,57: only accounts for a total of 14 trees across the entire survey.

spused <- unique(trclass$treslag)[unique(trclass$treslag) %in% growthpars$species]
# growth parameters for the species
parslist <- list(spid = growthpars$species[1:25], alpha = growthpars$q50[1:25], beta = growthpars$q50[26:50])

trused <- trclass[trclass$treslag %in% spused, ]
# make species IDs from 1 to the number of species.
trused$spnum <- factor(trused$treslag)
levels(trused$spnum) <- 1:25

# Convert 

classdata <- with(trused, c(parslist, list(heights=cmins, A=250, N=nrow(trused), Nspp=length(spused), Nplot=max(plotnum), Nclass=10, year=sesong, species=as.numeric(spnum), w=V1, class=as.numeric(sizeclass), plot=plotnum)))

classmodel <- model {
  # Likelihood for height
  for (i in 1:N) {
    z[i] ~ dnorm(mu.z[i], tau.z)
    mu.z[i] <- alpha[species[i]] * heights[sizeclass[i]] ^ beta[species[i]]
  }
  # Loop across plots and years (likelihood for other parameters)
  for (i in 1:N) {
      # Calculation of zstar (parameter phi determines this)
      crownA[i] <- w[i] * pi * (1/10000) * (phi[species[i]] * heights[sizeclass[i]]) ^ 2
      # Implementation of growth (parameters GD, GL)
      w[i] ~ dnorm(mu.w[i], tau.w)
      mu.w[i] <- w[i] * ifelse(z[i] >= zstar[t - 1], (1 - muL[species[i]]), (1 - muD[species[i]]))
      # Implementation of mortality (parameters MD, ML)
      
      # Implementation of reproduction (parameter F)
    
  }
  # Priors by species
  for (s in 1:Nspp) {
    phi[s] ~ dgamma(1, 1)
    MD[s] ~ dgamma(1, 1)
    ML[s] ~ dgamma(1, 1)
    GD[s] ~ dgamma(1, 1)
    GL[s] ~ dgamma(1, 1)
    F[s] ~ dgamma(1, 1)
    }
  # Single prior for precision parameter on height.
  tau.z <- 1/sigma.z^2
  sigma.z ~ dunif(0, 100)
}