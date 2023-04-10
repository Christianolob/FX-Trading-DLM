
library(data.table)
library(this.path)


name_file = "simulacao_reversao.RData"

path_extract = file.path(this.path(),"../..","simulation_models","single_models",name_file)

load(path_extract)

tabelas_resultado = data.table(tabelas_resultado)

tabelas_resultado[,sinal:=sign(forecast_1-preco_1)]
tabelas_resultado[,retorno:=sinal*(preco_1/ultimo_preco-1)]

hit_ratio = nrow(tabelas_resultado[retorno>0])/nrow(tabelas_resultado)
print(paste0("O hit ratio é de ",hit_ratio*100,"%"))

sharpe_ratio = sqrt(252)*mean(tabelas_resultado$retorno)/sd(tabelas_resultado$retorno)
print(paste0("O sharpe ratio é de ",sharpe_ratio))
