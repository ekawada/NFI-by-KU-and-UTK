// Model 1: Null Model with all interaction coefficients set to zero

data {
	int N; 			// Number of measurements for target species in dataset
	int Ntree;		// Number of individuals in target species
	int Nspp;		// Number of species (including "other")
	int Nplot;		// Number of plots containing target species
	int Nyear;		// Number of years in study
	
	int year[N];		// Year for each measurement
	int plot[N];		// Plot ID for each measurement
	int tree[N];		// Tree ID for each measurement
	real ba[N];			// Basal area (cm2 / 100) for each measurement
	real bainc[N];		// Basal area increment (cm2) for each measurement
	real gdd[N];		// Growing degree days for each measurement
	real precip[N];		// Yearly precipitation for each measurement
	
	vector[Nspp] areaxdist[N];	// Area by squared distance for each neighbor species
	vector[2] trait[Nspp];		// Trait PCA1 and PCA2 for each species
}

parameters {
	real<lower=0> alpha[Nplot];	// Plot-level random effect
	real<lower=0, upper=2> beta[3];				// Coefficients of dbh, temperature, and precipitation respectively.
	real<lower=0, upper=0.05> lambda;				// Single parameter on crowding index for the neutral model.
	real<lower=0> sigma_g;		// Dispersion of growth rate
	real mu_alpha;				// Hyperpriors on the plot-level random effect.
	real<lower=0> sigma_alpha;
}

model {
	// Likelihood
	for (i in 1:N) {
		bainc[i] ~ normal((alpha[plot[i]] * ba[i] ^ beta[1] * gdd[i] ^ beta[2] * precip[i] ^ beta[3]) / (1 + sum(lambda * areaxdist[i])), sigma_g);
	}
	  
	// Priors
	lambda ~ normal(0, 0.02);
	alpha ~ lognormal(mu_alpha, sigma_alpha);
	// Hyperpriors
	mu_alpha ~ normal(0, 3);
	// Did not specify priors for the sigmas since Gelman claims that uniform flat priors are better for them in Stan.
}

generated quantities {
	vector[N] log_lik; // Log-likelihood
	for (i in 1:N) {
		log_lik[i] <- normal_log(bainc[i], (alpha[plot[i]] * ba[i] ^ beta[1] * gdd[i] ^ beta[2] * precip[i] ^ beta[3]) / (1 + sum(lambda * areaxdist[i])), sigma_g);
	}
}