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

alfa_0 = 160
# media constante
theta_0 = 155

sigma = 2
# sigma_param_alfa = 2
sigma_param_theta = 1

# taxa de convergencia
constante_k = 0.1

serie_alfa = c(alfa_0)
serie_theta = c(theta_0)
serie_preco_revert = c(alfa_0)


for(linha in 2:linhas){
  # linha = 2
  
  novo_theta = serie_theta[linha-1]+(sigma_param_theta^2)*erro_param_theta[linha]
  # novo_theta = serie_theta[linha-1]

  # novo_alfa = serie_alfa[linha-1]+(serie_alfa[linha-1]-serie_alfa[linha-2])*novo_theta+(sigma_param_alfa^2)*erro_param_alfa[linha]
  # novo_alfa = serie_alfa[linha-1]+constante_k*(theta_0-serie_alfa[linha-1])+(sigma_param_alfa^2)*erro_param_alfa[linha]
  # novo_alfa = serie_alfa[linha-1]
  # novo_alfa = serie_alfa[linha-1]+(serie_alfa[linha-1]-serie_alfa[linha-1])*novo_theta
  
  # regressive_effect = c(regressive_effect,100*((serie_alfa[linha-1]-serie_alfa[linha-2])*novo_theta)/serie_alfa[linha-1])  
  # novo_preco_revert = novo_alfa + (sigma^2)*erro[linha]
  novo_preco_revert = serie_preco_revert[linha-1] +constante_k*(theta_0-serie_preco_revert[linha-1])+ (sigma^2)*erro[linha]
  # novo_preco_revert = novo_alfa

  # gera a serie do parametro
  # serie_alfa = c(serie_alfa,novo_alfa)
  serie_theta = c(serie_theta,novo_theta)
  serie_preco_revert = c(serie_preco_revert,novo_preco_revert)
}

windows()
par(mfrow=c(2,1))
plot(serie_preco_revert,type = "l")
# lines(serie_theta,col="red")
abline(h=theta_0,col="blue")
abline(v=20, lty=2)
abline(v=40, lty=2)
abline(v=60, lty=2)
abline(v=80, lty=2)
# plot(serie_theta,type = "l")
# plot(regressive_effect,type = "l")
# abline(h=0)
# abline(v=20, lty=2)
# abline(v=40, lty=2)
# abline(v=60, lty=2)
# abline(v=80, lty=2)


# Agora vamos estimar esse modelo com STAN

# Vamos comecar com a vol da equacao de estados e de precos conhecido e constante
data_stan <- list(N = length(serie_preco_revert),
                  price = serie_preco_revert
                  ,sigma = sigma
                  ,sigma_param_alfa = sigma_param_alfa
                  # ,sigma_param_theta = sigma_param_theta
                  ,constante_k=constante_k
                  )

print(mean(serie_preco_revert))

iteracoes = 5000

tic()
modelo_reversao <- stan(file = "simulacao_reversao_oficial.stan",
                     data = data_stan,
                     iter = iteracoes,
                     warmup = floor(iteracoes/2),
                     control = list(stepsize = 0.00001))
toc()

teste = summary(modelo_reversao)
# vetor_params_alfa = teste$summary[1:100,1]
vetor_params_theta = teste$summary[1:100,1]
resumo = teste$c_summary
# resumo[,1]

# windows()
# traceplot(modelo_reversao, pars = "sigma")
# windows()
# traceplot(modelo_reversao, pars = "sigma_param_alfa")
# windows()
# traceplot(modelo_reversao, pars = "theta")


par(mfrow=c(2,1))
plot(serie_preco_revert,type = "l")
lines(serie_theta,col="red")
title("Simulated data:")
abline(v=20, lty=2)
abline(v=40, lty=2)
abline(v=60, lty=2)
abline(v=80, lty=2)
plot(serie_preco_revert,type = "l")
lines(vetor_params_alfa,col="red")
# lines(vetor_params_theta,col="red")
abline(h=resumo[101],col="blue")
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


launch_shinystan(modelo_reversao)

