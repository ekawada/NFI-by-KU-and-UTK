# growth and mortality parameter estimation (now that we already have heights)

# Read tree and increment data
incr <- read.csv('./Data/tree data/incr.csv')
tru <- read.csv('.Data/tree data/treused.csv')

incru <- incr[incr$treeID %in% tru$treeID,]

# Set up data list to pass into MCMC
tru$spnum <- factor(tru$species)
levels(tru$spnum) <- 1:25
tru$plotnum <- tru$plotID
levels(tru$plotnum) <- 1:length(levels(tru$plotnum))

trudata <- with(tru, list(A = 250,
                          N = nrow(tru),
                          Nspp = length(levels(spnum)),
                          Nplot = length(levels(plotnum)),
                          plot = as.numeric(plotnum),
                          height = estHeight,
                          species = as.numeric(spnum),
                          year = year, 
                          dbh = dbh,
                          treeID = treeID,
                          incr = as.matrix(incr[,-1])))

# Specification of model
gmodel <- model {
  # Likelihood:
  # Loop across individuals to calculate crown area (phi)
  for (i in 1:N) {
    Ac[i] ~ dnorm(mu.Ac[i], tau.Ac)
    mu.Ac[i] <- pi * (1/10000) * (phi[species[i]] * height[i] / 10) ^ 2
  }
  # Calculate z-star given the crown areas.
  # Loop across individuals: growth (GD,GL), mortality (MD,ML), and reproduction (F)
  for (i in 1:N) {
    # Growth
    incr[treeID[i], year-1986] ~ dnorm(GD[species[i]], tau.GD[species[i]])
  }
  
  # Priors
  for (s in 1:Nspp) {
    phi[s] ~ dgamma(1, 1)
    MD[s] ~ dgamma(1, 1)
    ML[s] ~ dgamma(1, 1)
    GD[s] ~ dgamma(1, 1)
    GL[s] ~ dgamma(1, 1)
    F[s] ~ dgamma(1, 1)
  }
  # Priors on precision parameter
  tau.Ac <- sigma.Ac^-2
  sigma.Ac ~ dunif(0, 100)
}