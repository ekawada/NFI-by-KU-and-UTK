trace_summ <- function(species, modelno) {
	library(rstan)
	name <- paste0('dat',species,'mod',modelno)
	filenames <- dir()[grep(name, dir())]
	mc_fit <- read_stan_csv(filenames)
	
	# Calculate WAIC
	source('~/stan/nfi/mc2.0/waic.r')
	WAIC <- waic(mc_fit)
	
	# Make trace plots
	par_list <- c('beta', 'alpha[1]', 'alpha[2]', 'alpha[3]', 'sigma_g', 'mu_alpha', 'sigma_alpha', 'lambda[1]', 'lambda[2]', 'lambda[3]', 'T')
	pdf(paste0('./figs/', name, '_traceplots.pdf'), height = 6, width = 10)
		print(traceplot(mc_fit, pars = par_list[1]))
		print(traceplot(mc_fit, pars = par_list[2:4]))
		print(traceplot(mc_fit, pars = par_list[5:7]))
	if (modelno>=3) {
		print(traceplot(mc_fit, pars = par_list[8:10]))
	}
	if (modelno>=4) {
		print(traceplot(mc_fit, pars = par_list[11]))
	}
	dev.off()
	
	# Make summaries
	par_list <- list(c('beta','mu_alpha','sigma_alpha','sigma_g','alpha'),
					 c('beta', 'mu_alpha','sigma_alpha','sigma_g','alpha'),
					 c('beta', 'mu_alpha','sigma_alpha','sigma_g','lambda','alpha'),
					 c('beta','lambda','T','mu_alpha','sigma_alpha','sigma_g','alpha'),
					 c('beta','lambda','T','mu_alpha','sigma_alpha','sigma_g','alpha'))
	
	
	summ <- summary(mc_fit, pars = par_list[[modelno]])
	save(WAIC, summ, file = paste0('./summaries/', name, '_summ.r'))
	
	return(list(WAIC=WAIC, summary=summ))
}


sm <- expand.grid(s=1:7,m=1:5)
for (i in 15:nrow(sm)) {
	print(i)
	trace_summ(species=sm[i,1], modelno=sm[i,2])
}
