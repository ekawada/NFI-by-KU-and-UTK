# trace_summ_new <- function(bin) {
	filenames <- paste0('bin', bin, 'chain', 1:3, '.csv')
	library(rstan)
	fit_list <- list()
	for (i in filenames) fit_list[[length(fit_list) + 1]] <- read_stan_csv(i)
	mc_fit <- sflist2stanfit(fit_list)
	
	# Calculate WAIC
	source('~/stan/nfi/mc2.0/waic.r')
	WAIC <- waic(mc_fit)
	
	# Make trace plots
	par_list <- c('lambda','beta','gamma', 'alpha[1,1]', 'alpha[1,2]', 'alpha[1,3]', 'sigma_g[1]', 'sigma_g[2]', 'sigma_g[3]', 'mu_alpha[1]', 'mu_alpha[2]', 'mu_alpha[3]', 'sigma_alpha[1]', 'sigma_alpha[2]', 'sigma_alpha[3]')
	pdf(paste0('./figs/bin', bin, '_traceplots.pdf'), height = 6, width = 10)
		print(traceplot(mc_fit, pars = par_list[1]))
		print(traceplot(mc_fit, pars = par_list[2]))
		print(traceplot(mc_fit, pars = par_list[3]))
		print(traceplot(mc_fit, pars = par_list[4:6]))
		print(traceplot(mc_fit, pars = par_list[7:9]))
		print(traceplot(mc_fit, pars = par_list[10:12]))
		print(traceplot(mc_fit, pars = par_list[13:15]))
	dev.off()
	
	summ <- summary(mc_fit, pars = c('beta', 'gamma', 'sigma_g', 'mu_alpha', 'sigma_alpha', 'lambda', 'alpha'))
	save(WAIC, summ, file = paste0('./summaries/bin', bin, '_summ.r'))
# }