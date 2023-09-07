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

plota_tudo = TRUE

model <- stan_model(file = 'simulacao_reversao_oficial.stan')

tabelas_resultado = NULL
for (iteracao_codigo in 1:1000) {
  iteracao_codigo=1

  print(iteracao_codigo)
  
  tic()
  
  linhas = 100
  erro=rnorm(linhas)
  erro_param_alfa=rnorm(linhas)
  erro_param_theta=rnorm(linhas)
  
  alfa_0 = 1600
  # media constante
  theta_0 = 1550
  
  sigma = 160
  sigma_param_alfa = 80
  # sigma_param_theta = 1
  
  remocoes = 3
  
  # taxa de convergencia
  constante_k = 0.05
  
  serie_alfa = c(alfa_0)
  serie_theta = c(theta_0)
  serie_preco_revert = c(alfa_0)
  
  proximo_alpha = function(alpha_anterior, theta, constante_k,sigma_param_alfa,erro){
    novo_alfa = alpha_anterior+constante_k*(theta-alpha_anterior)+(sigma_param_alfa^2)*erro
    return(novo_alfa)
    }
  
  for(linha in 2:linhas){
    # linha = 2
    
    # novo_theta = serie_theta[linha-1]+(sigma_param_theta^2)*erro_param_theta[linha]
    novo_theta = serie_theta[linha-1]
  
    # novo_alfa = serie_alfa[linha-1]+(serie_alfa[linha-1]-serie_alfa[linha-2])*novo_theta+(sigma_param_alfa^2)*erro_param_alfa[linha]
    novo_alfa = serie_alfa[linha-1]+constante_k*(theta_0-serie_alfa[linha-1])+(sigma_param_alfa^2)*erro_param_alfa[linha]
    func_novo_alfa = proximo_alpha(alpha_anterior = serie_alfa[linha-1],
                  theta = theta_0,
                  constante_k = constante_k,
                  sigma_param_alfa = sigma_param_alfa,
                  erro = erro_param_alfa[linha])
    stopifnot(round(novo_alfa,digits = 5) == round(func_novo_alfa,digits = 5))
    # novo_alfa = serie_alfa[linha-1]
    # novo_alfa = serie_alfa[linha-1]+(serie_alfa[linha-1]-serie_alfa[linha-1])*novo_theta
    
    # regressive_effect = c(regressive_effect,100*((serie_alfa[linha-1]-serie_alfa[linha-2])*novo_theta)/serie_alfa[linha-1])  
    novo_preco_revert = novo_alfa + (sigma^2)*erro[linha]
    # novo_preco_revert = serie_preco_revert[linha-1] +constante_k*(theta_0-serie_preco_revert[linha-1])+ (sigma^2)*erro[linha]
    # novo_preco_revert = novo_alfa
  
    # gera a serie do parametro
    serie_alfa = c(serie_alfa,novo_alfa)
    serie_theta = c(serie_theta,novo_theta)
    serie_preco_revert = c(serie_preco_revert,novo_preco_revert)
  }
  
  if (plota_tudo != FALSE) {
    windows()
    par(mfrow=c(2,1))
    plot(serie_preco_revert,type = "l")
    lines(serie_alfa,col="red")
    abline(h=serie_theta,col="blue")
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
                    ,sigma_param_alfa = sigma_param_alfa
                    # ,sigma_param_theta = sigma_param_theta
                    ,constante_k=constante_k
                    )
  
  print(mean(serie_preco_revert))
  
  iteracoes = 2500
  
  tic()
  modelo_reversao <- sampling(object = model,
                       data = data_stan,
                       iter = iteracoes,
                       warmup = floor(iteracoes/2),
                       control = list(stepsize = 0.0001))
  toc()
  
  launch_shinystan(modelo_reversao)
  
  # Aqui comecamos a previsao
  resumo = summary(modelo_reversao)
  tbl_parametros = resumo$summary
  rownames(tbl_parametros)
  
  sigma_estimado = tbl_parametros["sigma_param_alfa","mean"]
  theta_estimado = tbl_parametros["theta_0","mean"]
  constante_k_estimado = tbl_parametros["constante_k","mean"]
  last_alpha = tbl_parametros["alfa[97]","mean"]
  
  forecast_1 = proximo_alpha(alpha_anterior = last_alpha,
                theta = theta_estimado,
                constante_k = constante_k_estimado,
                sigma_param_alfa = sigma_estimado,
                erro = 0)
  
  forecast_2 = proximo_alpha(alpha_anterior = forecast_1,
                             theta = theta_estimado,
                             constante_k = constante_k_estimado,
                             sigma_param_alfa = sigma_estimado,
                             erro = 0)
  
  forecast_3 = proximo_alpha(alpha_anterior = forecast_2,
                             theta = theta_estimado,
                             constante_k = constante_k_estimado,
                             sigma_param_alfa = sigma_estimado,
                             erro = 0)
  

  # resumo[,1]
  
  # windows()
  # traceplot(modelo_reversao, pars = "sigma")
  # windows()
  # traceplot(modelo_reversao, pars = "sigma_param_alfa")
  # windows()
  # traceplot(modelo_reversao, pars = "theta")
  
  if (plota_tudo != FALSE) {
    vetor_params_alfa = teste$summary[1:97,1]
    # vetor_params_theta = teste$summary[1:100,1]
    resumo = teste$c_summary
    
    par(mfrow=c(2,1))
    plot(serie_preco_revert,type = "l")
    lines(serie_alfa,col="red")
    abline(h=serie_theta,col="blue")
    title("Simulated data:")
    abline(v=20, lty=2)
    abline(v=40, lty=2)
    abline(v=60, lty=2)
    abline(v=80, lty=2)
    plot(serie_preco_revert,type = "l")
    lines(vetor_params_alfa,col="red")
    # lines(vetor_params_theta,col="red")
    abline(h=resumo[98],col="blue")
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

tabelas_resultado = data.table(tabelas_resultado)

tabelas_resultado[,sinal_trade := sign(forecast_1-ultimo_preco)]
tabelas_resultado[,retorno_ativo := preco_1/ultimo_preco-1]                 

tabelas_resultado[,retorno_strat :=retorno_ativo*sinal_trade]
hit_ratio = nrow(tabelas_resultado[retorno_strat>0])/nrow(tabelas_resultado)


# launch_shinystan(modelo_reversao)
save.image(file='simulacao_reversao.RData')
