
itd.z <- '
model {
  #likelihood
  for (t in 2:nt) {
    
    for (1 in 1:nindiv(t)) {
      
      # height of individual i at time t depends on parameters alpha and beta
      z[i, t] ~ dnorm(truez[i, t], tauz)
      truez[i, t] <- alpha[species[i]] * dbh[i, t] ^ beta[species[i]]
      # diameter growth rate at time t can take on 2 values depending on threshold height
      dbh[i, t] ~ dnorm(truedbh[i, t], taudbh)
      truedbh[i, t] <- dbh[i, t - 1] + ifelse(z[i, t - 1] >= zstar[t - 1], GL[species[i]], GD[species[i]])
      # mortality rate at time t can take on 2 values depending on threshold value
      w[i, t] ~ dnorm(truew[i, t], tauw)
      truew[i, t] <- w[i, t - 1] * ifelse(z[i, t - 1] >= zstar[t - 1], (1 - muL[species[i]]), (1 - muD[species[i]]))
      
    }
  }
  # calculation of z*
  for (t in 1:nt) {
    for (i in 1:nindiv[t]) {
      # get new crown area for each cohort with crown area-dbh allometry
      crownA[i, t] <- w[i, t] * pi * (1/10000) * (phi[species[i]] * dbh[i, t]) ^ 2
    }
      # sort the list of cohorts by height
      z.order[, t] <- order(z[, t])
      crownA.order[, t] <- crownA[z.order[, t]]
      for (k in nindiv[t]:1) {
        cumul.crownA[k, t] <- sum(crownA.order[1:k, t])
        zstar.index[k, t] <- ifelse((cumul.crownA[k, t] < A), 1, 0)
      }
        
    
    # find the height of the last cohort added before plot area was reached
    # if plot area is not reached, all cohorts are in the canopy
    zstar[t] <- ifelse( (sum(zstar.index[, t]) > 0), z.order[sum(zstar.index[, t]) + 1 , t], 0)
  }
  
  # reproduction not implemented yet.
  
  # prior dists for parameters
  tauz <- 1/(sigmaz^2)
  sigmaz ~ dunif(0, 100)
  taudbh <- 1/(sigmadbh^2)
  sigmadbh ~ dunif(0, 100)
  tauw <- 1/(sigmaw^2)
  sigmaw ~ dunif(0, 100)
  for (j in 1:nspp) {
    GL[j] ~ dgamma(shapegl[j], rategl[j])
    GD[j] ~ dgamma(shapegd[j], rategd[j])
    muL[j] ~ dgamma(shapemul[j], ratemul[j])
    muD[j] ~ dgamma(shapemud[j], ratemud[j])
    alpha[j] ~ dgamma(shapealpha[j], ratealpha[j])
    beta[j] ~ dgamma(shapebeta[j], ratebeta[j])
    phi[j] ~ dgamma(shapephi[j], ratephi[j])
    shapegl[j] ~ dunif(0, 100)
    rategl[j] ~ dunif(0, 100)
    shapegd[j] ~ dunif(0, 100)
    rategd[j] ~ dunif(0, 100)
    shapemul[j] ~ dunif(0, 100)
    ratemul[j] ~ dunif(0, 100)
    shapemud[j] ~ dunif(0, 100)
    ratemud[j] ~ dunif(0, 100)
    shapealpha[j] ~ dunif(0, 100)
    ratealpha[j] ~ dunif(0, 100)
    shapebeta[j] ~ dunif(0, 100)
    ratebeta[j] ~ dunif(0, 100)
    shapephi[j] ~ dunif(0, 100)
    ratephi[j] ~ dunif(0, 100)
  }
  
  #F ~ dgamma(shapef, ratef)
  #shapef ~ dunif(0, 100)
  #ratef ~ dunif(0, 100)
  
}' # end model specification.


##############
# improved model using as input a matrix with all the output data.
itd2list <- function(x) {
  startrow <- which(x$time > 1)[1]
  prevdbh <- prevw <- recruit <- rep(0, nrow(x))
  for (i in 1:nrow(x)) {
    rowidx <- which(x$cohort == x$cohort[i] & x$species == x$species[i] & x$time == x$time[i] - 1)
    prevdbh[i] <- ifelse(length(rowidx) == 0, NA, x$dbh[rowidx])
    prevw[i] <- ifelse(length(rowidx) == 0, NA, x$w[rowidx])
    recruit[i] <- ifelse(x$cohort[i] == x$time[i], 1, 0)
    if (i%%1000 == 0) print(i)
  }
  spareas <- with(subset(x, overstory), tapply(crownA, list(time, species), sum))
  with(x, list(species=species, cohort=cohort, dbh=dbh, prevdbh=prevdbh, w=w, prevw=prevw, z=z, crownA=crownA, overstory=as.numeric(overstory), recruit=recruit, time=time, nspp = max(species), nt = max(time), speciesareas = spareas/rowSums(spareas), n = nrow(x), start = startrow))
}

itdfull <- '
  model {
    # Likelihood
    for (i in start:n) {
      # 1. height (for estimation of alpha and beta)
      z[i] ~ dnorm(truez[i], tauz)
      truez[i] <- alpha[species[i]] * dbh[i] ^ beta[species[i]]
      # 2. crown area (for estimation of phi)
      crownA[i] ~ dnorm(truecrownA[i], taucrownA)
      crownA[i] <- w[i] * pi * (1/10000) * (phi[species[i]] * dbh[i]) ^ 2
      # 3. growth (for estimation of GD and GL)
      dbh[i] ~ dnorm(truedbh[i], taudbh)
      truedbh[i] <- prevdbh[i] + ifelse(equals(overstory[i], 1), GL[species[i]], GD[species[i]])
      # 2. mortality and reproduction (for estimation of F, muD and muL)
      w[i] ~ dnorm(truew[i], tauw)
      mortality[i] <- prevw[i] * ifelse(equals(overstory[i], 1), (1 - muL[species[i]]), (1 - muD[species[i]]))
      reproduction[i] <- F[species[i]] * speciesareas[time[i]-1, species[i]]
      truew[i, t] <- recruit[i] * reproduction[i] + -(recruit[i] - 1) * mortality[i]
    }
    # Priors
    tauz <- 1/(sigmaz^2)
    sigmaz ~ dunif(0, 100)
    taudbh <- 1/(sigmadbh^2)
    sigmadbh ~ dunif(0, 100)
    tauw <- 1/(sigmaw^2)
    sigmaw ~ dunif(0, 100)
    for (j in 1:nspp) {
      alpha[j] ~ dgamma(shapealpha, ratealpha)
      beta[j] ~ dgamma(shapebeta, ratebeta)
      phi[j] ~ dgamma(shapephi, ratephi)
      GD[j] ~ dgamma(shapeGD, rateGD)
      GL[j] ~ dgamma(shapeGL, rateGL)
      muD[j] ~ dgamma(shapemuD, ratemuD)
      muL[j] ~ dgamma(shapemuL, ratemuL)
      F[j] ~ dgamma(shapeF, rateF)
      shapealpha[j] ~ dunif(0, 100)
      shapebeta[j] ~ dunif(0, 100)
      shapephi[j] ~ dunif(0, 100)
      shapeGD[j] ~ dunif(0, 100)
      shapeGL[j] ~ dunif(0, 100)
      shapemuD[j] ~ dunif(0, 100)
      shapemuL[j] ~ dunif(0, 100)
      shapeF[j] ~ dunif(0, 100)
      ratealpha[j] ~ dunif(0, 100)
      ratebeta[j] ~ dunif(0, 100)
      ratephi[j] ~ dunif(0, 100)
      rateGD[j] ~ dunif(0, 100)
      rateGL[j] ~ dunif(0, 100)
      ratemuD[j] ~ dunif(0, 100)
      ratemuL[j] ~ dunif(0, 100)
      rateF[j] ~ dunif(0, 100)
      
    }
      
  }' # end model specification

library(rjags)
Mlist <- itd2list(M2)
itdfit <- jags.model(textConnection(itdfull), data=Mlist, n.adapt = 1000)
itdsample <- coda.samples(itdfit, c('alpha','beta','phi','GD','GL','muD','muL','F'),
                          n.iter = 99999, thin = 10)
itdsumm <- summary(itdsample)
#######
diagMCMC(itdsample, parName = 'alpha[1]')