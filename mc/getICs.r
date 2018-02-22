# Read summaries for Feb 2018 models

fp <- 'C:/Users/Q/Dropbox/nfi/Cluster/stan/output/feb2018_better'

# Read neighbor model summaries in
dir(file.path(fp, 'neigh_summary'))

neigh_summ <- replicate(5, list())
neigh_waic <- replicate(5, list())

for (i in 1:5) {
  for (j in 1:7) {
    load(file.path(fp, 'neigh_summary', paste0('dat',j,'mod',i,'_summ.r')))
    neigh_summ[[i]][[j]] <- summ
    neigh_waic[[i]][[j]] <- WAIC
  }
}

sapply(neigh_waic, function(x) sapply(x, function(z) z$waic))


#########################
# code to calculate waic 
# run on cluster

library(rstan)
library(loo)

sm <- expand.grid(s = 1:7, m = 1:5)

mc_loo <- list()
mc_waic <- list()

for (i in 1:nrow(sm)) {

  print(paste(sm$s[i], sm$m[i]))  
    
  name <- paste0('dat',sm$s[i],'mod',sm$m[i])
  filenames <- dir(pattern = name)
  mc_fit <- read_stan_csv(filenames)
  
  mc_ll <- extract_log_lik(mc_fit)
  mc_loo[[i]] <- loo(mc_ll)
  mc_waic[[i]] <- waic(mc_ll)

}

# Put information from each IC into a data frame.
extr_loo <- function(x) {
  unlist(x[1:6])
}

all_loo <- lapply(mc_loo, extr_loo)
all_waic <- lapply(mc_waic, extr_loo)

ICs <- data.frame(species = sm$s, model = sm$m, 
                  do.call(rbind, all_loo),
                  do.call(rbind, all_waic))

write.csv(ICs, 'ICs.csv')


# Read and view locally
ICs <- read.csv('Cluster/stan/output/feb2018_better/ICs.csv')

library(cowplot)
library(dplyr)

# Calculate deltas

best_mod <- function(x) {
  data.frame(best_model_looic = x$model[which.min(x$looic)],
             best_model_waic = x$model[which.min(x$waic)],
             best_looic = min(x$looic),
             best_waic = min(x$waic))
}

bestmods <- ICs %>%
  group_by(species) %>%
  do(best_mod(.))

ICs <- left_join(ICs, bestmods)

ggplot(ICs, aes(x = model, y = looic - best_looic, color = factor(species), group = factor(species))) + 
  geom_line() +
  geom_point(data = subset(ICs, looic == best_looic))

ggplot(ICs, aes(x = model, y = waic - best_waic, color = factor(species), group = factor(species))) + 
  geom_line() +
  geom_point(data = subset(ICs, waic == best_waic))


# Added 22 Feb.
# Extract T coefficients from model summaries

t_list <- list()

for (i in 4:5) {
  for (j in 1:7) {
    t_list[[length(t_list) + 1]] <- c(model = i, species = j, neigh_summ[[i]][[j]][[1]]["T", ]) 
 
  }
}

t_list <- do.call(rbind, t_list)
t_list <- as.data.frame(t_list)
names(t_list)[6:10] <- c('q025','q25','q50','q75','q975')

library(ggplot2)
ggplot(t_list, aes(x=species,y=q50,ymin=q025,ymax=q975)) +
  geom_pointrange() + facet_wrap(~ model, scales = 'free')
