
library(data.table)
library(this.path)


name_file = "simulacao_reversao.RData"

path_extract = file.path(this.path(),"../..","simulation_models","single_models",name_file)

load(path_extract)

tabelas_resultado = data.table(tabelas_resultado)

tabelas_resultado[,caso_1:=forecast_1-V4]
tabelas_resultado[,caso_2:=forecast_2-V5]
tabelas_resultado[,caso_3:=forecast_3-V6]

