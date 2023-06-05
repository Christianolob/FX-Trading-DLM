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
flag_theta_fixo = FALSE

if (flag_theta_fixo) {
  modelo_utilizado = 'simulacao_trend_oficial_theta_fixo'
} else {
  modelo_utilizado = 'simulacao_trend_oficial'
}

model <- stan_model(file = paste0(modelo_utilizado,".stan"))

n_iteracoes = 1

tabelas_resultado = NULL
for (iteracao_codigo in 1:n_iteracoes) {
  # iteracao_codigo=1    

  print(iteracao_codigo)
  
  linhas = 100
  erro=rnorm(linhas)
  erro_param_alfa=rnorm(linhas)
  erro_param_theta=rnorm(linhas)
  
  alfa_0 = 155
  theta_0 = 0.4
  
  remocoes=3
  
  sigma = 2
  sigma_param_alfa = 1
  sigma_param_theta = 0.3
  
  serie_alfa = c(alfa_0,alfa_0)
  serie_theta = c(theta_0,theta_0)
  serie_preco = c(alfa_0,alfa_0)
  
  regressive_effect = c()
  
  proximo_alfa = function(alfa_anterior, alfa_anterior_anterior, novo_theta, sigma_param_alfa, erro){
    novo_alfa = alfa_anterior+(alfa_anterior-alfa_anterior_anterior)*novo_theta+(sigma_param_alfa)*erro
    return(novo_alfa)
  }
  
  for(linha in 3:linhas){
    # linha = 2
    
    # novo_theta = serie_theta[linha-1]*(1+(sigma_param_theta)*erro_param_theta[linha])
    novo_theta = serie_theta[linha-1]
    
    novo_alfa = serie_alfa[linha-1]+(serie_alfa[linha-1]-serie_alfa[linha-2])*novo_theta+(sigma_param_alfa)*erro_param_alfa[linha]
    # novo_alfa = serie_alfa[linha-1]+(sigma_param_alfa)*erro_param_alfa[linha]
    # novo_alfa = serie_alfa[linha-1]
    # novo_alfa = serie_alfa[linha-1]+(serie_alfa[linha-1]-serie_alfa[linha-1])*novo_theta
    
    regressive_effect = c(regressive_effect,100*((serie_alfa[linha-1]-serie_alfa[linha-2])*novo_theta)/serie_alfa[linha-1])  
    
    novo_preco_revert = novo_alfa + (sigma)*erro[linha]
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
  if (flag_theta_fixo) {
    data_stan <- list(N = length(serie_preco)-remocoes,
                      price = serie_preco[1:(length(serie_preco)-remocoes)]
                      ,theta = theta_0
                      # ,sigma_param_alfa = sigma_param_alfa
    )
  } else {
    data_stan <- list(N = length(serie_preco)-remocoes,
                      price = serie_preco[1:(length(serie_preco)-remocoes)]
                      # ,sigma = sigma
                      # ,sigma_param_alfa = sigma_param_alfa
    )
  }
  
  print(mean(serie_preco))
  
  iteracoes = 5000

  tic()
  modelo_trend <- sampling(object = model,
                            data = data_stan,
                            iter = iteracoes,
                            warmup = floor(iteracoes/2),
                            control = list(stepsize = 0.00001))
  toc()
  
  
  resumo = summary(modelo_trend)
  tbl_parametros = resumo$summary
  rownames(tbl_parametros)
  
  if (flag_theta_fixo) {
    theta_estimado = theta_0
  } else {
    theta_estimado = tbl_parametros["theta","mean"]
  }
  
  alfa_anterior_estimado = tbl_parametros["alfa[97]","mean"]
  alfa_anterior_anterior_estimado = tbl_parametros["alfa[96]","mean"]
  sigma_param_alfa_estimado = tbl_parametros["sigma_param_alfa","mean"]

  forecast_1 = proximo_alfa(alfa_anterior = alfa_anterior_estimado,
                            alfa_anterior_anterior = alfa_anterior_anterior_estimado,
                            novo_theta = theta_estimado,
                            sigma_param_alfa = sigma_param_alfa_estimado,
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
  
  tabelas_resultado_intermediario = NULL
  tabelas_resultado_intermediario = cbind(tabelas_resultado_intermediario,ultimo_preco)
  tabelas_resultado_intermediario = cbind(tabelas_resultado_intermediario,forecast_1)
  tabelas_resultado_intermediario = cbind(tabelas_resultado_intermediario,preco_1)
  
  # comeca os parametros estimados
  tabelas_resultado_intermediario = cbind(tabelas_resultado_intermediario,theta_estimado)
  tabelas_resultado_intermediario = cbind(tabelas_resultado_intermediario,alfa_anterior_estimado)
  tabelas_resultado_intermediario = cbind(tabelas_resultado_intermediario,alfa_anterior_anterior_estimado)
  tabelas_resultado_intermediario = cbind(tabelas_resultado_intermediario,sigma_param_alfa_estimado)
  
  # os parametros reais
  theta = serie_theta[97]
  alfa_anterior = serie_alfa[97]
  alfa_anterior_anterior = serie_alfa[96]
  tabelas_resultado_intermediario = cbind(tabelas_resultado_intermediario,theta)
  tabelas_resultado_intermediario = cbind(tabelas_resultado_intermediario,alfa_anterior)
  tabelas_resultado_intermediario = cbind(tabelas_resultado_intermediario,alfa_anterior_anterior)
  tabelas_resultado_intermediario = cbind(tabelas_resultado_intermediario,sigma_param_alfa)
  
  tabelas_resultado = rbind(tabelas_resultado,tabelas_resultado_intermediario)
  
  toc()
  
}

save.image(file=paste0(modelo_utilizado,'.RData'))

launch_shinystan(modelo_trend)

tabelas_resultado_dt = data.table(tabelas_resultado)

tabelas_resultado_dt[,erro_theta:=theta_estimado-theta]
tabelas_resultado_dt[,erro_alfa_anterior:=alfa_anterior_estimado-alfa_anterior]
tabelas_resultado_dt[,erro_alfa_anterior_anterior:=alfa_anterior_anterior_estimado-alfa_anterior_anterior]
tabelas_resultado_dt[,erro_sigma_param_alfa:=sigma_param_alfa_estimado-sigma_param_alfa]

# save.image(file='simulacao_trend_oficial.RData')
# load('C:/Users/chris/Documents/FX-Trading-DLM/simulation_models/single_models/simulacao_trend_oficial.RData')

media_erro_theta = NULL
media_erro_alfa_anterior = NULL
media_erro_alfa_anterior_anterior = NULL
media_erro_sigma_param_alfa = NULL
for(iteracao_codigo in 1:n_iteracoes){
  media_erro_theta = c(media_erro_theta,sum(tabelas_resultado_dt$erro_theta[1:iteracao_codigo])/iteracao_codigo)
  media_erro_alfa_anterior = c(media_erro_alfa_anterior,sum(tabelas_resultado_dt$erro_alfa_anterior[1:iteracao_codigo])/iteracao_codigo)
  media_erro_alfa_anterior_anterior = c(media_erro_alfa_anterior_anterior,sum(tabelas_resultado_dt$erro_alfa_anterior_anterior[1:iteracao_codigo])/iteracao_codigo)
  media_erro_sigma_param_alfa = c(media_erro_sigma_param_alfa,sum(tabelas_resultado_dt$erro_sigma_param_alfa[1:iteracao_codigo])/iteracao_codigo)
}

# setnames(tabelas_resultado,"V11","last_theta")

par(mfrow=c(4,1))
plot(media_erro_theta)
abline(h=0,col="red")
plot(media_erro_alfa_anterior)
abline(h=0,col="red")
plot(media_erro_alfa_anterior_anterior)
abline(h=0,col="red")
plot(media_erro_sigma_param_alfa)
abline(h=0,col="red")

