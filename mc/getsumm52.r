trace_summ <- function(species, modelno) {
	library(rstan)
	name <- paste0('dat',species,'mod',modelno)
	filenames <- dir()[grep(name, dir())]
	mc_fit <- read_stan_csv(filenames)
	
	# Calculate WAIC
	source('~/stan/nfi/mc2.0/waic.r')
	WAIC <- waic(mc_fit)
	
	# Make trace plots
	library(bayesplot)
	
	par_list <- c('beta', 'alpha[1]', 'alpha[2]', 'alpha[3]', 'sigma_g', 'mu_alpha', 'sigma_alpha', 'lambda[1]', 'lambda[2]', 'lambda[3]', 'T')
	pdf(paste0('./figs/', name, '_traceplots.pdf'), height = 6, width = 10)
		print(mcmc_trace(as.array(mc_fit, pars = par_list[1])))
		print(mcmc_trace(as.array(mc_fit, pars = par_list[2:4])))
		print(mcmc_trace(as.array(mc_fit, pars = par_list[5:7])))
	if (modelno>=3) {
		print(mcmc_trace(as.array(mc_fit, pars = par_list[8:10])))
	}
	if (modelno>=4) {
		print(mcmc_trace(as.array(mc_fit, pars = par_list[11])))
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
for (i in 1:nrow(sm)) {
	print(i)
	trace_summ(species=sm[i,1], modelno=sm[i,2])
}

trace_summ_indivmodel <- function(bin, nchains) {
	filenames <- paste0('bin', bin, 'samples', 1:nchains, '.csv')
	library(rstan)
	library(bayesplot)
	fit_list <- list()
	for (i in filenames) fit_list[[length(fit_list) + 1]] <- read_stan_csv(i)
	mc_fit <- sflist2stanfit(fit_list)
	
	# Make trace plots
	par_list <- c('lambda','beta_size','beta_temp','beta_prec','gamma', 'alpha[1]', 'alpha[2]', 'alpha[3]', 'sigma_g','mu_alpha', 'sigma_alpha', 'mu[1]','mu[2]','mu[3]','sigma_i[1]','sigma_i[2]','sigma_i[3]')
	pdf(paste0('./figs/bin', bin,  '_traceplots.pdf'), height = 6, width = 10)
		print(mcmc_trace(as.array(mc_fit, pars = par_list[1])))
		print(mcmc_trace(as.array(mc_fit, pars = par_list[2])))
		print(mcmc_trace(as.array(mc_fit, pars = par_list[3])))
		print(mcmc_trace(as.array(mc_fit, pars = par_list[4])))
		print(mcmc_trace(as.array(mc_fit, pars = par_list[5])))
		print(mcmc_trace(as.array(mc_fit, pars = par_list[6:8])))
		print(mcmc_trace(as.array(mc_fit, pars = par_list[10:11])))
		print(mcmc_trace(as.array(mc_fit, pars = par_list[12:14])))
		print(mcmc_trace(as.array(mc_fit, pars = par_list[15:17])))
	dev.off()
	
	summ <- summary(mc_fit, pars = c('beta_size', 'beta_temp', 'beta_prec', 'gamma', 'sigma_g', 'mu_alpha', 'sigma_alpha', 'lambda', 'alpha'))
	save(summ, file = paste0('./summaries/bin', bin, '_summ.r'))
}
