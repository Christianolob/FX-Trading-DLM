




cria_grafico_parametro = function(summary_stan_model,
                                  parameter_name,
                                  sigmas = 1,
                                  true_mean=NA){
  # summary_stan_model=resumo
  # parameter_name = "theta"
  # true_mean=serie_theta_real
  
  library(data.table)
  library(plotly)
  
  summary_stan_model = data.frame(summary_stan_model)
  dt <- data.table(Name = rownames(summary_stan_model), summary_stan_model)
  
  dt_filtrado = dt[like(Name,pattern = paste0(parameter_name,"\\["))]
  # plot(dt_filtrado$mean,type="l")
  
  df <- dt_filtrado[,Mean_plus_SD := mean + sigmas*sd]
  df <- dt_filtrado[,Mean_minus_SD := mean - sigmas*sd]

  if (length(true_mean)!=0) {

    stopifnot(length(true_mean)==nrow(df))

    plot_ly(df) %>%
      add_lines(x = ~1:nrow(df), y = ~mean, name = "Mean", line = list(color = 'rgb(205, 12, 24)', width = 4)) %>%
      add_lines(x = ~1:nrow(df), y = ~true_mean, name = "True Mean", line = list(color = 'rgb(0, 0, 255)', width = 4)) %>%
      add_ribbons(x = ~1:nrow(df), ymin = ~Mean_minus_SD, ymax = ~Mean_plus_SD, name = "Mean ± SD",
                  fillcolor = 'rgba(205, 12, 24, 0.2)', line = list(color = 'transparent')) %>%
      layout(title = "Mean and Standard Deviation Over Time",
             xaxis = list(title = "Time"),
             yaxis = list(title = "Value"))
  } else {
    
  }
}


calcula_score_pctual_n_sigmas = function(summary_stan_model,
                                         parameter_name,
                                         true_mean,
                                         sigmas = 1){
  # summary_stan_model=tbl_parametros
  # parameter_name = "theta"
  # true_mean=serie_theta_real
  # sigmas = 2
  
  library(data.table)
  
  summary_stan_model = data.frame(summary_stan_model)
  dt <- data.table(Name = rownames(summary_stan_model), summary_stan_model)
  
  dt_filtrado = dt[like(Name,pattern = paste0(parameter_name,"\\["))]
  # plot(dt_filtrado$mean,type="l")
  
  df <- dt_filtrado[,Mean_plus_SD := mean + sigmas*sd]
  df <- dt_filtrado[,Mean_minus_SD := mean - sigmas*sd]
  
  df_mergeado <- data.table(df,true_mean)
  df_mergeado[,passou_pra_cima:=true_mean>Mean_plus_SD]
  df_mergeado[,passou_pra_baixo:=true_mean<Mean_minus_SD]
  
  score = (sum(df_mergeado$passou_pra_cima)+sum(df_mergeado$passou_pra_baixo))/nrow(df_mergeado)
  
}








