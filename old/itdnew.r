# the previous ITD function had problems.
# here is a new attempt from scratch.

ITD1 <- function(P, M, A) {
  # 1. calculate critical canopy height z*
  
    # add the crown areas of each cohort starting with the tallest
    M <- M[order(M$z, decreasing = TRUE), ]
    totalcrownA <- cumsum(M$crownA)
    # find the height of the last cohort added before plot area was reached
    # if plot area is not reached, all cohorts are in the canopy
    zstar <- if (sum(M$crownA) >= A) M$z[which(totalcrownA >= A)[1]]
             else min(M$z)
    # create logical for canopy status
    M$overstory <- M$z >= zstar
    
  # 2. implement mortality
  
    # assign canopy or understory mortality parameter to each cohort in each species
    mu <- apply(cbind(M$overstory, P$muL[M$species], P$muD[M$species]), 1, function(x) if (x[1]) x[2] else x[3])
    # get new abundance for each cohort
    M$w <- M$w * (1 - mu)
  
  # 3. implement growth
    
    # assign canopy or understory mortality parameter to each cohort in each species
    G <- apply(cbind(M$overstory, P$GL[M$species], P$GD[M$species]), 1, function(x) if (x[1]) x[2] else x[3])
    # get new diameter for each cohort
    M$dbh <- M$dbh + G
    # get new height for each cohort with height-dbh allometry
    M$z <- P$alpha[M$species] * M$dbh ^ (P$beta[M$species])
    # get new crown area for each cohort with crown area-dbh allometry
    M$crownA <- M$w * pi * (1/10000) * (P$phi[M$species] * M$dbh) ^ 2
    # sort the list of cohorts by height
    M <- M[order(M$z, decreasing = TRUE), ]
    # add the crown areas of each cohort starting with the tallest
    totalcrownA <- cumsum(M$crownA)
    # find the height of the last cohort added before plot area was reached
    # if plot area is not reached, all cohorts are in the canopy
    zstar <- if (sum(M$crownA) >= A) M$z[which(totalcrownA >= A)[1]]
    else min(M$z)
    # create logical for canopy status
    M$overstory <- M$z >= zstar
  
  # 4. implement reproduction
    
    # calculate proportion exposed crown area for each species
    ACanopy <- with(subset(M, overstory == TRUE), sum(crownA))
    AjCanopy <- rep(0, nrow(P))
    for (j in 1:nrow(P)) {
      Aj <- with(subset(M, species == j), sum(crownA))
      AjCanopy[j] <- Aj - with(subset(M, species == j & overstory == FALSE), sum(crownA))
    }
    AjCanopy[is.na(AjCanopy)] <- 0
    # multiply proportion by fecundity to get number of individuals in next cohort
    wnew <- P$F * (AjCanopy/ACanopy)
    seedlings <- data.frame(species = 1:nrow(P),
                            cohort = max(M$cohort) + 1,
                            dbh = 0,
                            w = wnew,
                            z = 0,
                            crownA = 0,
                            overstory = FALSE)
    # add new cohorts to output matrix
    M <- rbind(M, seedlings)
    # remove all cohorts with <1 tree
    M <- subset(M, w >= 1)
}

### end function

# Analytical solutions for the equilibrium canopy closure height in monoculture for each species (z hat), indicates late-successional performance

zhat <- with(P, alpha * (GD/muD)^beta * log(2*pi*(phi^2)*F*(GL^2)*(muL^-3))^beta)

# Height of 20-year-old open-grown tree for each species (H20), indicates early-successional performance

H20 <- with(P, alpha * (20*GL)^beta)



# test:
P <- data.frame(GD = c(0.3, 0.01),
                GL = c(1, 1.5),
                muD = c(0.05, 0.5),
                muL = c(0.005, 0.04),
                phi = c(sqrt(7), sqrt(5)),
                alpha = c(5, 4),
                beta = c(0.2, 0.3),
                F = c(250,250))
M1 <- data.frame(species=c(1,2),
                 cohort=c(1,1),
                 dbh=P$GL,
                 w=c(10,10),
                 z=with(P, alpha*GL^beta),
                 crownA=10 * pi * (1/10000) * (P$phi * P$GL) ^ 2,
                 overstory=c(TRUE,TRUE))

A <- 2500

M <- list(M1)
for (i in 1:500) M[[i+1]] <- ITD1(P, M[[i]], A)

# Dynamic parameters (P as F(temp, precip))
nt <- 500
E <- data.frame(T = 20 + cumsum(sample(c(-0.5, 0.5), nt, replace=TRUE)),
                P = 500 + cumsum(sample(c(-10, 10), nt, replace=TRUE)))
Pd <- function(E) {
  data.frame(GD = c(),
             GL = c(),
             muD = c(),
             muL = c(),
             phi = c(),
             alpha = c(),
             beta = c(),
             F = c())
}