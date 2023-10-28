//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=1> N;
  vector[N] price;

}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  // real alfa;
  vector[N] alfa;
  real<lower=0,upper=1> theta;
  // real<lower=0,upper=1> theta;
  real<lower=0> sigma;
  real<lower=0> sigma_param_alfa;
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  // priors
  // theta ~ beta(1,1);
  // theta ~ pareto(0.1,2);
  // theta ~ exponential(1/0.2);
  // sigma_param_alfa ~ inv_gamma(0.0001, 0.0001);
  // sigma ~ inv_gamma(0.0001, 0.0001);
  // alfa ~ normal(1,1);
  
  // priors - all uniforms
  theta ~ beta(1,1);
  // sigma_param_alfa ~ uniform(0, 10000);
  // sigma ~ uniform(0, 10000);
  alfa ~ normal(150,100);
  sigma_param_alfa ~ cauchy(0, 25);
  sigma ~ cauchy(0, 25);
  
  for (n in 3:N)
    // alfa[n] ~ normal(alfa[n-1]+(alfa[n-1]-alfa[n-2])*theta[n],sigma_param_alfa); // Colocar price[N] pega apenas o ultimo ponto
    alfa[n] ~ normal(alfa[n-1]+(alfa[n-1]-alfa[n-2])*theta,sigma_param_alfa); // Colocar price[N] pega apenas o ultimo ponto
  for (n in 1:N)
    price[n] ~ normal(alfa[n],sigma); // Colocar price[N] pega apenas o ultimo ponto
  // for (n in 2:N)
  //   theta[n] ~ normal(theta[n-1],0.3); // Colocar price[N] pega apenas o ultimo ponto
}

// make sure Stan code ends with a blank line
