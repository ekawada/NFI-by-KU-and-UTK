# Pull T's

fp <- 'C:/Users/Q/Dropbox/nfi/Cluster/stan/output/feb2018_better'

# Read neighbor model summaries in
dir(file.path(fp, 'neigh_summary'))

neigh_summ <- replicate(5, list())

for (i in 1:5) {
  for (j in 1:7) {
    load(file.path(fp, 'neigh_summary', paste0('dat',j,'mod',i,'_summ.r')))
    neigh_summ[[i]][[j]] <- summ
  }
}

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
  geom_pointrange() + facet_wrap(~ model, scales = 'free', labeller = labeller(model = c('4'='relative','5'='stabilizing')))
