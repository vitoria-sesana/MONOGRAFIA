x <- 
  cobertura_logbin_freq_df %>% 
  filter(amostra_categoria == 50 & parametro == "b0") 


x$ind_cobertura %>% sum()
x$ind_n_na %>% sum()


438/458
