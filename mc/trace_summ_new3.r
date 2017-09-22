trace_summ_new <- function(bin, nchains) {
	filenames <- paste0('bin', bin, 'chain', 1:nchains, '.csv')
	library(rstan)
	fit_list <- list()
	for (i in filenames) fit_list[[length(fit_list) + 1]] <- read_stan_csv(i)
	mc_fit <- sflist2stanfit(fit_list)
	
	# Calculate WAIC
	# source('~/stan/nfi/mc2.0/waic.r')
	# WAIC <- waic(mc_fit)
	
	# Make trace plots
	par_list <- c('lambda','beta_size','beta_temp','beta_prec','gamma', 'alpha[1]', 'alpha[2]', 'alpha[3]', 'sigma_g','mu_alpha', 'sigma_alpha', 'mu[1]','mu[2]','mu[3]','sigma_i[1]','sigma_i[2]','sigma_i[3]')
	pdf(paste0('./figs/bin', bin,  '_traceplots.pdf'), height = 6, width = 10)
		print(traceplot(mc_fit, pars = par_list[1]))
		print(traceplot(mc_fit, pars = par_list[2]))
		print(traceplot(mc_fit, pars = par_list[3]))
		print(traceplot(mc_fit, pars = par_list[4]))
		print(traceplot(mc_fit, pars = par_list[5]))
		print(traceplot(mc_fit, pars = par_list[6:8]))
	#	print(traceplot(mc_fit, pars = par_list[9]))
		print(traceplot(mc_fit, pars = par_list[10:11]))
		print(traceplot(mc_fit, pars = par_list[12:14]))
		print(traceplot(mc_fit, pars = par_list[15:17]))
	dev.off()
	
	summ <- summary(mc_fit, pars = c('beta_size', 'beta_temp', 'beta_prec', 'gamma', 'sigma_g', 'mu_alpha', 'sigma_alpha', 'lambda', 'alpha'))
	save(summ, file = paste0('./summaries/bin', bin, '_summ.r'))
}
