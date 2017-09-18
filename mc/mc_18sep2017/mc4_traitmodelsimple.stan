// Model 4: Relative fitness difference model
// Simplified on 2017 May 24 (only 1 trait parameter)

data {
	int N; 			// Number of measurements for target species in dataset
	int Ntree;		// Number of individuals in target species
	int Nspp;		// Number of species (including "other")
	int Nplot;		// Number of plots containing target species
	int Nyear;		// Number of years in study
	int targetsp;	// ID number of target species (for traits)
	
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

parameters {
	real<lower=0> alpha[Nplot];	// Plot-level random effect
	real<lower=0, upper=2> beta[3];				// Coefficients of dbh, temperature, and precipitation respectively.
	real<lower=0> sigma_g;		// Dispersion of growth rate
	real<lower=0> sigma_lambda;	// Dispersion of interaction coefficients
	real mu_alpha;				// Hyperpriors on the plot-level random effect.
	real<lower=0> sigma_alpha;
	
	real T;				// Coefficient for single trait term.
	vector<lower=0, upper=0.05>[Nspp] lambda;	// Interaction coefficients for each neighbor species' effect ON the target species.
}

transformed parameters {
	vector<lower=0>[N] mu;			// Given lower=0 to truncate the distribution to a half-normal.
	for (i in 1:N) {
		mu[i] = (alpha[plot[i]] * ba[i] ^ beta[1] * gdd[i] ^ beta[2] * precip[i] ^ beta[3]) / (1 + sum(lambda .* areaxdist[i]));
	}
}


model {
	// Likelihood
	for (j in 1:Nspp) {
		// Pairwise interaction coefficients that depend on relative fitness of each species, measured as traits.
		// Goes off the assumption that there is greater competitive effect if the neighbor has higher SLA and lower SSD
		lambda[j] ~ normal(T * (-trait[targetsp][1] + trait[j][1]) + (trait[targetsp][2] - trait[j][2]), sigma_lambda);	
	}	
	for (i in 1:N) {
		bainc[i] ~ normal(mu[i], sigma_g) T[0,];
	}
	
	// Priors
	T ~ normal(0, 0.1);
	lambda ~ normal(0, 0.02);
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
