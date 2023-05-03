
library(data.table)
library(this.path)

# name_file = "simulacao_trend_oficial.RData"
name_file = "simulacao_reversao_oficial_theta_dinamico.RData"

path_extract = file.path(this.path(),"../..","simulation_models","single_models",name_file)

load(path_extract)

tabelas_resultado_dt[,sinal:=sign(forecast_1-ultimo_preco)]
tabelas_resultado_dt[,retorno:=sinal*(preco_1/ultimo_preco-1)]

hit_ratio = nrow(tabelas_resultado_dt[retorno>0])/nrow(tabelas_resultado_dt)
print(paste0("O hit ratio é de ",hit_ratio*100,"%"))

sharpe_ratio = sqrt(252)*mean(tabelas_resultado_dt$retorno)/sd(tabelas_resultado_dt$retorno)
print(paste0("O sharpe ratio é de ",sharpe_ratio))
