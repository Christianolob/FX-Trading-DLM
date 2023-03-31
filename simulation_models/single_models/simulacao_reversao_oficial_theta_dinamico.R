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
for (iteracao_codigo in 1:100) {
  # iteracao_codigo = 1
  
  linhas = 100
  erro=rnorm(linhas)
  erro_param_alfa=rnorm(linhas)
  erro_param_theta=rnorm(linhas)
  
  alfa_0 = 160
  # media constante
  theta_0 = 155
  
  remocoes = 3
  
  sigma = 1.5
  # sigma_param_alfa = 1
  sigma_param_theta = 1
  
  # taxa de convergencia
  constante_k = 0.1
  
  serie_alfa = c(alfa_0)
  serie_theta = c(theta_0)
  serie_preco_revert = c(alfa_0)
  
  proximo_theta = function(theta_anterior, sigma_param_theta, erro){
    novo_theta = theta_anterior+(sigma_param_theta^2)*erro
    return(novo_theta)
  }
  proximo_preco = function(preco_anterior, constante_k, novo_theta, sigma, erro){
    novo_preco = preco_anterior +constante_k*(novo_theta-preco_anterior) + (sigma^2) * erro
    return(novo_preco)
  }
  
  
  for(linha in 2:linhas){
    # linha = 2
    
    novo_theta = proximo_theta(serie_theta[linha-1],sigma_param_theta,erro_param_theta[linha])
    # novo_theta = serie_theta[linha-1]
  
    # novo_alfa = serie_alfa[linha-1]+(serie_alfa[linha-1]-serie_alfa[linha-2])*novo_theta+(sigma_param_alfa^2)*erro_param_alfa[linha]
    # novo_alfa = serie_alfa[linha-1]+constante_k*(theta_0-serie_alfa[linha-1])+(sigma_param_alfa^2)*erro_param_alfa[linha]
    # novo_alfa = serie_alfa[linha-1]
    # novo_alfa = serie_alfa[linha-1]+(serie_alfa[linha-1]-serie_alfa[linha-1])*novo_theta
    
    # regressive_effect = c(regressive_effect,100*((serie_alfa[linha-1]-serie_alfa[linha-2])*novo_theta)/serie_alfa[linha-1])  
    # novo_preco_revert = novo_alfa + (sigma^2)*erro[linha]
    novo_preco_revert = serie_preco_revert[linha-1] +constante_k*(novo_theta-serie_preco_revert[linha-1])+ (sigma^2)*erro[linha]
    # novo_preco_revert = novo_alfa
  
    # gera a serie do parametro
    # serie_alfa = c(serie_alfa,novo_alfa)
    serie_theta = c(serie_theta,novo_theta)
    serie_preco_revert = c(serie_preco_revert,novo_preco_revert)
  }
  
  if (plota_tudo != FALSE) {
    windows()
    par(mfrow=c(2,1))
    plot(serie_preco_revert,type = "l")
    lines(serie_theta,col="red")
    # abline(h=serie_theta,col="blue")
    abline(v=20, lty=2)
    abline(v=40, lty=2)
    abline(v=60, lty=2)
    abline(v=80, lty=2)
  }
  
  # Agora vamos estimar esse modelo com STAN
  
  # Vamos comecar com a vol da equacao de estados e de precos conhecido e constante
  data_stan <- list(N = length(serie_preco_revert)-remocoes,
                    price = serie_preco_revert[1:(length(serie_preco_revert)-remocoes)]
                    ,sigma = sigma
                    # ,sigma_param_alfa = sigma_param_alfa
                    ,sigma_param_theta = sigma_param_theta
                    ,constante_k=constante_k
                    )
  
  print(mean(serie_preco_revert))
  
  iteracoes = 5000
  
  tic()
  modelo_reversao <- stan(file = "simulacao_reversao_oficial_theta_dinamico.stan",
                       data = data_stan,
                       iter = iteracoes,
                       warmup = floor(iteracoes/2),
                       control = list(stepsize = 0.00001))
  toc()
  
  ultimo_preco = serie_preco_revert[length(serie_preco_revert)-remocoes]
  
  resumo = summary(modelo_reversao)
  tbl_parametros = resumo$summary
  rownames(tbl_parametros)

  sigma_theta = tbl_parametros["sigma_param_theta","mean"]
  last_theta = tbl_parametros["theta[97]","mean"]
  constante_k_estimado = tbl_parametros["constante_k","mean"]
  sigma = tbl_parametros["sigma","mean"]
  
  forecast_1 = proximo_preco(preco_anterior = ultimo_preco,
                             constante_k = constante_k_estimado,
                             novo_theta = last_theta,
                             sigma = sigma,
                             erro = 0)
  
  forecast_2 = proximo_preco(preco_anterior = forecast_1,
                             constante_k = constante_k_estimado,
                             novo_theta = last_theta,
                             sigma = sigma,
                             erro = 0)
  
  forecast_3 = proximo_preco(preco_anterior = forecast_2,
                             constante_k = constante_k_estimado,
                             novo_theta = last_theta,
                             sigma = sigma,
                             erro = 0)

  if (plota_tudo != FALSE) {
    par(mfrow=c(2,1))
    plot(serie_preco_revert,type = "l")
    lines(serie_theta,col="red")
    # abline(h=serie_theta,col="blue")
    title("Simulated data:")
    abline(v=20, lty=2)
    abline(v=40, lty=2)
    abline(v=60, lty=2)
    abline(v=80, lty=2)
    plot(serie_preco_revert,type = "l")
    # lines(vetor_params_alfa,col="red")
    lines(vetor_params_theta,col="red")
    # abline(h=resumo[101],col="blue")
    title("Stan model output:")
    abline(v=20, lty=2)
    abline(v=40, lty=2)
    abline(v=60, lty=2)
    abline(v=80, lty=2)
  }
  
  ultimo_preco = serie_preco_revert[length(serie_preco_revert)-remocoes]
  preco_1 = serie_preco_revert[length(serie_preco_revert)-remocoes+1]
  preco_2 = serie_preco_revert[length(serie_preco_revert)-remocoes+2]
  preco_3 = serie_preco_revert[length(serie_preco_revert)-remocoes+3]
  
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

save.image(file='simulacao_reversao_oficial_theta_dinamico.RData')

# launch_shinystan(modelo_reversao)

