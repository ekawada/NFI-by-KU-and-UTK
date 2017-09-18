# Test stan models 

library(rstan)

fp <- '~/GitHub/NFI-by-KU-and-UTK/mc/mc_18sep2017/'

indiv_stan <- stan_model(file = file.path(fp, 'individualmodel.stan')) # Works but only if we get rid of the vectorization. (if truncated, can't do it.)
mod1_stan <- stan_model(file = file.path(fp, 'mc1_nullmodel.stan'))
mod2_stan <- stan_model(file = file.path(fp, 'mc2_neutralmodel.stan'))
mod3_stan <- stan_model(file = file.path(fp, 'mc3_speciesmodel.stan'))
mod4_stan <- stan_model(file = file.path(fp, 'mc4_traitmodelsimple.stan'))
mod5_stan <- stan_model(file = file.path(fp, 'mc5_nichemodelsimple.stan'))
