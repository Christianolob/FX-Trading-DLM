
data {
  int<lower=0> N;
  vector[N] y;
  real<lower=0> sigma_price;
  real<lower=0> sigma_alpha;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real alpha[N];
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  // y ~ normal(alpha, sigma_price);
  for (n in 2:N)
    y[n] ~ normal(alpha[n], sigma_price);
  for (n in 2:N)
    alpha[n] ~ normal(alpha[-1], sigma_alpha);
}

