// Model 1: Null Model with all interaction coefficients set to zero
// Edited 18 Sep 2017 to include truncated normal.

data {
	int N; 			// Number of measurements for target species in dataset
	int Ntree;		// Number of individuals in target species
	int Nspp;		// Number of species (including "other")
	int Nplot;		// Number of plots containing target species
	int Nyear;		// Number of years in study
	
	int year[N];		// Year for each measurement
	int plot[N];		// Plot ID for each measurement
	int tree[N];		// Tree ID for each measurement
	real<lower=0> ba[N];		// Basal area (cm2 / 100) for each measurement
	real<lower=0> bainc[N];		// Basal area increment (cm2) for each measurement
	real<lower=0> gdd[N];		// Growing degree days for each measurement
	real<lower=0> precip[N];	// Yearly precipitation for each measurement
	
	vector<lower=0>[Nspp] areaxdist[N];	// Area by squared distance for each neighbor species
	vector[2] trait[Nspp];		// Trait PCA1 and PCA2 for each species
}

transformed data {
	vector[Nspp] lambda;
	for (i in 1:Nspp) {
	// Interaction coefficients all set to zero in the null model.
		lambda[i] = 0;
	}
}

parameters {
	real<lower=0> alpha[Nplot];	// Plot-level random effect
	real<lower=0, upper=2> beta[3];				// Coefficients of dbh, temperature, and precipitation respectively.
	real<lower=0> sigma_g;		// Dispersion of growth rate
	real mu_alpha;				// Hyperpriors on the plot-level random effect.
	real<lower=0> sigma_alpha;
}

transformed parameters {
	vector<lower=0>[N] mu;			// Given lower=0 to truncate the distribution to a half-normal.
	for (i in 1:N) {
		mu[i] = (alpha[plot[i]] * ba[i] ^ beta[1] * gdd[i] ^ beta[2] * precip[i] ^ beta[3]) / (1 + sum(lambda .* areaxdist[i]));
	}
}

model {
	// Likelihood
	for (i in 1:N) {
		bainc[i] ~ normal(mu[i], sigma_g) T[0,];
	}
	  
	// Priors
	alpha ~ lognormal(mu_alpha, sigma_alpha);
	// Hyperpriors
	mu_alpha ~ normal(0, 3);
	// Did not specify priors for the sigmas since Gelman claims that uniform flat priors are better for them in Stan.
}

generated quantities {
	vector[N] log_lik; // Log-likelihood
	for (i in 1:N) {
		log_lik[i] = normal_log(bainc[i], mu[i], sigma_g);
	}
}
