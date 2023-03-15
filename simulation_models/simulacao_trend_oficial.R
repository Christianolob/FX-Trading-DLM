rm(list = ls())
options(scipen = 999)
options(max.print=1000000)

# install.packages("data.table")
# install.packages("Rtools")
# install.packages("this.path")
# install.packages("tictoc")

library(data.table)
library(rstan)
library(this.path)
library(shinystan)
library(tictoc)
# library(Rtool)

# PARALELIZA
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

setwd(file.path(this.path(),".."))

linhas = 100
erro=rnorm(linhas)
erro_param_alfa=rnorm(linhas)
erro_param_theta=rnorm(linhas)

alfa_0 = 155
theta_0 = 0.5

sigma = 2
sigma_param_alfa = 1
sigma_param_theta = 0.3

serie_alfa = c(alfa_0,alfa_0)
serie_theta = c(theta_0,theta_0)
serie_preco_trend = c(alfa_0,alfa_0)

regressive_effect = c()

for(linha in 3:linhas){
  # linha = 2
  
  # novo_theta = serie_theta[linha-1]*(1+(sigma_param_theta^2)*erro_param_theta[linha])
  novo_theta = serie_theta[linha-1]
  
  novo_alfa = serie_alfa[linha-1]+(serie_alfa[linha-1]-serie_alfa[linha-2])*novo_theta+(sigma_param_alfa^2)*erro_param_alfa[linha]
  # novo_alfa = serie_alfa[linha-1]+(sigma_param_alfa^2)*erro_param_alfa[linha]
  # novo_alfa = serie_alfa[linha-1]
  # novo_alfa = serie_alfa[linha-1]+(serie_alfa[linha-1]-serie_alfa[linha-1])*novo_theta
  
  regressive_effect = c(regressive_effect,100*((serie_alfa[linha-1]-serie_alfa[linha-2])*novo_theta)/serie_alfa[linha-1])  
  
  novo_preco_revert = novo_alfa + (sigma^2)*erro[linha]
  # novo_preco_revert = novo_alfa

  # gera a serie do parametro
  serie_alfa = c(serie_alfa,novo_alfa)
  serie_theta = c(serie_theta,novo_theta)
  serie_preco_trend = c(serie_preco_trend,novo_preco_revert)
}

windows()
par(mfrow=c(2,1))
plot(serie_preco_trend,type = "l")
lines(serie_alfa,col="red")
abline(v=20, lty=2)
abline(v=40, lty=2)
abline(v=60, lty=2)
abline(v=80, lty=2)
# plot(serie_theta,type = "l")
plot(regressive_effect,type = "l")
abline(h=0)
abline(v=20, lty=2)
abline(v=40, lty=2)
abline(v=60, lty=2)
abline(v=80, lty=2)


# Agora vamos estimar esse modelo com STAN

# Vamos comecar com a vol da equacao de estados e de precos conhecido e constante
data_stan <- list(N = length(serie_preco_trend),
                  price = serie_preco_trend
                  # ,sigma = sigma
                  # ,sigma_param_alfa = sigma_param_alfa
                  )

print(mean(serie_preco_trend))

iteracoes = 5000

tic()
modelo_trend <- stan(file = "simulacao_trend_oficial.stan",
                     data = data_stan,
                     iter = iteracoes,
                     warmup = floor(iteracoes/2),
                     control = list(stepsize = 0.00001))
toc()

teste = summary(modelo_trend)
vetor_params_alfa = teste$summary[1:100,1]
# vetor_params_theta = teste$summary[101:200,1]
resumo = teste$c_summary
# resumo[,1]

# windows()
# traceplot(modelo_trend, pars = "sigma")
# windows()
# traceplot(modelo_trend, pars = "sigma_param_alfa")
# windows()
# traceplot(modelo_trend, pars = "theta")


par(mfrow=c(2,1))
plot(serie_preco_trend,type = "l")
lines(serie_alfa,col="red")
title("Simulated data:")
abline(v=20, lty=2)
abline(v=40, lty=2)
abline(v=60, lty=2)
abline(v=80, lty=2)
plot(serie_preco_trend,type = "l")
lines(vetor_params_alfa,col="red")
title("Stan model output:")
abline(v=20, lty=2)
abline(v=40, lty=2)
abline(v=60, lty=2)
abline(v=80, lty=2)


# windows()
# par(mfrow=c(1,1))
# plot(serie_theta,type = "l")
# lines(vetor_params_theta,col="red")
# title("Simulated data:")
# abline(v=20, lty=2)
# abline(v=40, lty=2)
# abline(v=60, lty=2)
# abline(v=80, lty=2)


launch_shinystan(modelo_trend)

