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
  // real<lower=0> sigma;
  // real<lower=0> constante_k;
  // real<lower=0> sigma_param_alfa;
  
  // int<lower=1> N_new;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  // real alfa;
  vector[N] alfa;
   real<lower=0> theta_0;
   real<lower=0> sigma;
   real<lower=0> sigma_param_alfa;
   real<lower=0,upper=1> constante_k;
   
   // vector[N_new] alfa_new;
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  
  // sigma ~ cauchy(0, 500);
  // sigma_param_alfa ~ cauchy(0, 200);
  
  for (n in 2:N)
    alfa[n] ~ normal(alfa[n-1]+constante_k*(theta_0-alfa[n-1]),sigma_param_alfa); // Colocar price[N] pega apenas o ultimo ponto
    // alfa[n] ~ normal(alfa[n-1],sigma_param_alfa); // Colocar price[N] pega apenas o ultimo ponto
  for (n in 1:N)
    price[n] ~ normal(alfa[n],sigma); // Colocar price[N] pega apenas o ultimo ponto
  // for (n in 2:N)
  //   theta[n] ~ normal(theta[n-1],0.3); // Colocar price[N] pega apenas o ultimo ponto
}

