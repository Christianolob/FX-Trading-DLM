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

model <- stan_model(file = 'simulacao_reversao_oficial_theta_dinamico.stan')

n_iteracoes = 100

tabelas_resultado = NULL
media_erro_sigma_theta = NULL
media_erro_last_theta = NULL
media_erro_sigma = NULL
for (iteracao_codigo in 1:n_iteracoes) {
  # iteracao_codigo = 1
  
  print(iteracao_codigo)
  
  tic()
  
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
  sigma_theta = 1
  
  # taxa de convergencia
  constante_k = 0.1
  
  serie_alfa = c(alfa_0)
  serie_theta = c(theta_0)
  serie_preco_revert = c(alfa_0)
  
  proximo_theta = function(theta_anterior, sigma_theta, erro){
    novo_theta = theta_anterior+(sigma_theta^2)*erro
    return(novo_theta)
  }
  proximo_preco = function(preco_anterior, constante_k, novo_theta, sigma, erro){
    novo_preco = preco_anterior +constante_k*(novo_theta-preco_anterior) + (sigma^2) * erro
    return(novo_preco)
  }
  
  
  for(linha in 2:linhas){
    # linha = 2
    
    novo_theta = proximo_theta(serie_theta[linha-1],sigma_theta,erro_param_theta[linha])
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
                    # ,sigma = sigma
                    # ,sigma_param_alfa = sigma_param_alfa
                    # ,sigma_theta = sigma_theta
                    # ,constante_k = constante_k
                    )
  
  print(mean(serie_preco_revert))
  
  iteracoes = 5000
  
  tic()
  modelo_reversao <- sampling(object = model,
                              data = data_stan,
                              iter = iteracoes,
                              warmup = floor(iteracoes/2),
                              control = list(stepsize = 0.00001))
  toc()
  
  ultimo_preco = serie_preco_revert[length(serie_preco_revert)-remocoes]
  
  resumo = summary(modelo_reversao)
  tbl_parametros = resumo$summary
  rownames(tbl_parametros)

  sigma_theta_estimado = tbl_parametros["sigma_theta","mean"]
  last_theta_estimado = tbl_parametros["theta[97]","mean"]
  constante_k_estimado = tbl_parametros["constante_k","mean"]
  sigma_estimado = tbl_parametros["sigma","mean"]
  
  forecast_1 = proximo_preco(preco_anterior = ultimo_preco,
                             constante_k = constante_k_estimado,
                             novo_theta = last_theta_estimado,
                             sigma = sigma_estimado,
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

  tabelas_resultado_intermediario = NULL
  tabelas_resultado_intermediario = cbind(tabelas_resultado_intermediario,ultimo_preco)
  tabelas_resultado_intermediario = cbind(tabelas_resultado_intermediario,forecast_1)
  tabelas_resultado_intermediario = cbind(tabelas_resultado_intermediario,preco_1)
  
  # comeca os parametros estimados
  tabelas_resultado_intermediario = cbind(tabelas_resultado_intermediario,sigma_theta_estimado)
  tabelas_resultado_intermediario = cbind(tabelas_resultado_intermediario,constante_k_estimado)
  tabelas_resultado_intermediario = cbind(tabelas_resultado_intermediario,sigma_estimado)
  tabelas_resultado_intermediario = cbind(tabelas_resultado_intermediario,last_theta_estimado)

  # os parametros reais  
  tabelas_resultado_intermediario = cbind(tabelas_resultado_intermediario,sigma_theta)
  tabelas_resultado_intermediario = cbind(tabelas_resultado_intermediario,constante_k)
  tabelas_resultado_intermediario = cbind(tabelas_resultado_intermediario,sigma)
  last_theta = serie_theta[97]
  tabelas_resultado_intermediario = cbind(tabelas_resultado_intermediario,last_theta)
  
  
  tabelas_resultado = rbind(tabelas_resultado,tabelas_resultado_intermediario)

  # par(mfrow=c(3,1))
  # plot(media_erro_sigma_theta)
  # abline(h=0,col="red")
  # plot(media_erro_last_theta)
  # abline(h=0,col="red")
  # plot(media_erro_sigma)
  # abline(h=0,col="red")

  toc()
  
}

tabelas_resultado_dt = data.table(tabelas_resultado)

tabelas_resultado_dt[,erro_sigma_theta:=sigma_theta-sigma_theta_estimado]
tabelas_resultado_dt[,erro_last_theta:=last_theta_estimado-last_theta]
tabelas_resultado_dt[,erro_sigma:=sigma-sigma_estimado]
tabelas_resultado_dt[,erro_constante_k:=constante_k-constante_k_estimado]

save.image(file='simulacao_reversao_oficial_theta_dinamico.RData')

media_erro_sigma_theta = NULL
media_erro_last_theta = NULL
media_erro_sigma = NULL
media_erro_constante_k = NULL
for(iteracao_codigo in 1:n_iteracoes){
  media_erro_sigma_theta = c(media_erro_sigma_theta,sum(tabelas_resultado_dt$erro_sigma_theta[1:iteracao_codigo])/iteracao_codigo)
  media_erro_last_theta = c(media_erro_last_theta,sum(tabelas_resultado_dt$erro_last_theta[1:iteracao_codigo])/iteracao_codigo)
  media_erro_sigma = c(media_erro_sigma,sum(tabelas_resultado_dt$erro_sigma[1:iteracao_codigo])/iteracao_codigo)
  media_erro_constante_k = c(media_erro_constante_k,sum(tabelas_resultado_dt$erro_constante_k[1:iteracao_codigo])/iteracao_codigo)
  }

# setnames(tabelas_resultado,"V11","last_theta")

par(mfrow=c(3,1))
plot(media_erro_sigma_theta)
abline(h=0,col="red")
plot(media_erro_last_theta)
abline(h=0,col="red")
plot(media_erro_constante_k)
abline(h=0,col="red")

