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

plota_tudo = FALSE

tabelas_resultado = NULL
for (iteracao_codigo in 1:1000) {
  # iteracao_codigo=1    

  print(iteracao_codigo)
  
  linhas = 100
  erro=rnorm(linhas)
  erro_param_alfa=rnorm(linhas)
  erro_param_theta=rnorm(linhas)
  
  alfa_0 = 155
  theta_0 = 0.5
  
  remocoes=3
  
  sigma = 2
  sigma_param_alfa = 1
  sigma_param_theta = 0.3
  
  serie_alfa = c(alfa_0,alfa_0)
  serie_theta = c(theta_0,theta_0)
  serie_preco = c(alfa_0,alfa_0)
  
  regressive_effect = c()
  
  proximo_alfa = function(alfa_anterior, alfa_anterior_anterior, novo_theta, sigma_param_alfa, erro){
    novo_alfa = alfa_anterior+(alfa_anterior-alfa_anterior_anterior)*novo_theta+(sigma_param_alfa^2)*erro
    return(novo_alfa)
  }
  
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
    serie_preco = c(serie_preco,novo_preco_revert)
  }
  
  if (plota_tudo != FALSE){
    windows()
    par(mfrow=c(2,1))
    plot(serie_preco,type = "l")
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
  }
  
  # Agora vamos estimar esse modelo com STAN
  
  # Vamos comecar com a vol da equacao de estados e de precos conhecido e constante
  data_stan <- list(N = length(serie_preco)-remocoes,
                    price = serie_preco[1:(length(serie_preco)-remocoes)]
                    # ,sigma = sigma
                    # ,sigma_param_alfa = sigma_param_alfa
                    )
  
  print(mean(serie_preco))
  
  iteracoes = 5000
  
  tic()
  modelo_trend <- stan(file = "simulacao_trend_oficial.stan",
                       data = data_stan,
                       iter = iteracoes,
                       warmup = floor(iteracoes/2),
                       control = list(stepsize = 0.00001))
  toc()
  
  resumo = summary(modelo_trend)
  tbl_parametros = resumo$summary
  rownames(tbl_parametros)
  
  novo_theta = tbl_parametros["theta","mean"]
  alfa_anterior = tbl_parametros["alfa[97]","mean"]
  alfa_anterior_anterior = tbl_parametros["alfa[96]","mean"]
  sigma_param_alfa = tbl_parametros["sigma_param_alfa","mean"]

  forecast_1 = proximo_alfa(alfa_anterior = alfa_anterior,
                            alfa_anterior_anterior = alfa_anterior_anterior,
                            novo_theta = novo_theta,
                            sigma_param_alfa = sigma_param_alfa,
                            erro = 0)

  forecast_2 = proximo_alfa(alfa_anterior = forecast_1,
                            alfa_anterior_anterior = alfa_anterior,
                            novo_theta = novo_theta,
                            sigma_param_alfa = sigma_param_alfa,
                            erro = 0)
  
  forecast_3 = proximo_alfa(alfa_anterior = forecast_2,
                            alfa_anterior_anterior = forecast_1,
                            novo_theta = novo_theta,
                            sigma_param_alfa = sigma_param_alfa,
                            erro = 0)
  
  if (plota_tudo != FALSE){
    par(mfrow=c(2,1))
    plot(serie_preco,type = "l")
    lines(serie_alfa,col="red")
    title("Simulated data:")
    abline(v=20, lty=2)
    abline(v=40, lty=2)
    abline(v=60, lty=2)
    abline(v=80, lty=2)
    plot(serie_preco,type = "l")
    lines(vetor_params_alfa,col="red")
    title("Stan model output:")
    abline(v=20, lty=2)
    abline(v=40, lty=2)
    abline(v=60, lty=2)
    abline(v=80, lty=2)
  }
  

  ultimo_preco = serie_preco[length(serie_preco)-remocoes]
  preco_1 = serie_preco[length(serie_preco)-remocoes+1]
  preco_2 = serie_preco[length(serie_preco)-remocoes+2]
  preco_3 = serie_preco[length(serie_preco)-remocoes+3]
  
  tabelas_resultado_intermediario = NULL
  tabelas_resultado_intermediario = cbind(tabelas_resultado_intermediario,ultimo_preco)
  tabelas_resultado_intermediario = cbind(tabelas_resultado_intermediario,forecast_1)
  tabelas_resultado_intermediario = cbind(tabelas_resultado_intermediario,forecast_2)
  tabelas_resultado_intermediario = cbind(tabelas_resultado_intermediario,forecast_3)
  tabelas_resultado_intermediario = cbind(tabelas_resultado_intermediario,preco_1)
  tabelas_resultado_intermediario = cbind(tabelas_resultado_intermediario,preco_2)
  tabelas_resultado_intermediario = cbind(tabelas_resultado_intermediario,preco_3)
  
  tabelas_resultado = rbind(tabelas_resultado,tabelas_resultado_intermediario)
  
  toc()
  
}

save.image(file='simulacao_trend_oficial.RData')

# launch_shinystan(modelo_trend)

