
# remove.packages("rstan")
# install.packages("StanHeaders", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
# install.packages("rstan", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))

library(rstan)

n = 100

erro_price = rnorm(n)
erro_alpha = rnorm(n)
sigma_price = 2
sigma_alpha = 1


price = c(100)
alpha = c(100)
for(tempo in 2:n){
  # tempo = 2
  
  new_alfa = alpha[tempo-1] + sigma_alpha^2*erro_alpha[tempo]
  new_price = new_alfa + sigma_price^2*erro_price[tempo]

  alpha <- c(alpha,new_alfa)
  price <- c(price,new_price)
  
  
}

plot(price,type='l')
lines(alpha,col="red")

data_stan <- list(y=price,
                  N=n,
                  sigma_price = sigma_price,
                  sigma_alpha = sigma_alpha
)

model_a = stan(file = "C:/Users/chris/Documents/dissertacao/reversion_model.stan",
               data = data_stan,
               iter = 1000,
               warmup = 500,
               chains = 4)

resumo = summary(model_a)

alpha_param = resumo$summary[2:n,1]

plot(price,type="l")
lines(c(100,alpha_param),col="red")


