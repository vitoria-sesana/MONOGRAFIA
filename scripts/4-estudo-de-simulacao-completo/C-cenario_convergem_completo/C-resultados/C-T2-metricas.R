# rotina ------------------------------------------------------------------
source("scripts/0-rotina.R", encoding = "UTF-8")


# leitura -----------------------------------------------------------------
coefs <- read.csv("saidas/3-saida-simulacao-completo/C-saidas/C-resultados/C-media_sd.csv", )
vies <- read.csv("saidas/3-saida-simulacao-completo/C-saidas/C-resultados/C-vies_reqm.csv")
cob_amp <- read.csv("saidas/3-saida-simulacao-completo/C-saidas/C-resultados/C-pc_am.csv")


# tratamento --------------------------------------------------------------

coefs_long <- 
  coefs %>%
  pivot_longer(
    cols = c(
      media_lgb,
      sd_lgb,
      media_pois,
      sd_pois,
      media_lgb_bayes_media,
      sd_lgb_bayes_media,
      media_lgb_bayes_mediana,
      sd_lgb_bayes_mediana
    ),
    names_to = c(".value", "modelo"),
    names_pattern = "(media|sd)_(.*)"
  ) %>% 
  select(-X) %>% 
  mutate(
    modelo = case_when(
      modelo == "lgb" ~ "Log-binomial frequentista",
      modelo == "lgb_bayes_media" ~ "Log-binomial Bayesiano (Média)",
      modelo == "lgb_bayes_mediana" ~ "Log-binomial Bayesiano (Mediana)",
      modelo == "pois" ~ "Poisson robusto",
      TRUE ~ modelo
    ),
    modelo = factor(
      modelo,
      levels = c(
        "Log-binomial frequentista",
        "Poisson robusto",
        "Log-binomial Bayesiano (Média)",
        "Log-binomial Bayesiano (Mediana)"
      )
    )
  ) %>% 
  select(modelo, amostra_categoria, parametro, everything()) %>% 
  arrange(modelo, amostra_categoria, parametro)


vies_long <- 
  vies %>%
  pivot_longer(
    cols = c(
      vies_lgb,
      RMSE_lgb,
      vies_pois,
      RMSE_pois,
      vies_bayes_media,
      RMSE_bayes_media,
      vies_bayes_mediana,
      RMSE_bayes_mediana
    ),
    names_to = c(".value", "modelo"),
    names_pattern = "(vies|RMSE)_(.*)"
  ) %>% 
  select(-X) %>% 
  mutate(
    modelo = case_when(
      modelo == "lgb" ~ "Log-binomial frequentista",
      modelo == "bayes_media" ~ "Log-binomial Bayesiano (Média)",
      modelo == "bayes_mediana" ~ "Log-binomial Bayesiano (Mediana)",
      modelo == "pois" ~ "Poisson robusto",
      TRUE ~ modelo
    ),
    modelo = factor(
      modelo,
      levels = c(
        "Log-binomial frequentista",
        "Poisson robusto",
        "Log-binomial Bayesiano (Média)",
        "Log-binomial Bayesiano (Mediana)"
      )
    )
  ) %>% 
  select(modelo, amostra_categoria, parametro, everything()) %>% 
  arrange(modelo, amostra_categoria, parametro)

cob_amp_long <- 
  cob_amp %>%
  rename(
    "prob_c_lgb_freq" = prob_c_freq,
    "prob_c_pois" = prob_c_poiss,
    "prob_c_bayes_quantilica" = probabilidade_cobertura_quantilica,
    "prob_c_bayes_HPD" = probabilidade_cobertura_HPD
         ) %>% 
  pivot_longer(
    cols = c(
      prob_c_lgb_freq,
      amplitude_lgb_freq,
      prob_c_pois,
      amplitude_pois,
      prob_c_bayes_quantilica,
      amplitude_bayes_quantilica,
      prob_c_bayes_HPD,
      amplitude_bayes_HPD
    ),
    names_to = c(".value", "modelo"),
    names_pattern = "(prob_c|amplitude)_(.*)"
  ) %>%  
  select(-X) %>% 
  mutate(
    modelo = case_when(
      modelo == "lgb_freq" ~ "Log-binomial frequentista",
      modelo == "bayes_quantilica" ~ "Log-binomial Bayesiano (Quantis)",
      modelo == "bayes_HPD" ~ "Log-binomial Bayesiano (HPD)",
      modelo == "pois" ~ "Poisson robusto",
      TRUE ~ modelo
    ),
    modelo = factor(
      modelo,
      levels = c(
        "Log-binomial frequentista",
        "Poisson robusto",
        "Log-binomial Bayesiano (Quantis)",
        "Log-binomial Bayesiano (HPD)"
      )
    )
  ) %>% 
  select(modelo, amostra_categoria, parametro, everything()) %>% 
  arrange(modelo, amostra_categoria, parametro)

coefs_vies <- coefs_long %>%
  left_join(
    vies_long,
    by = c("modelo", "amostra_categoria", "parametro")
  )



## 

# coefs_long$modelo %>% unique()
# vies_long$modelo %>% unique()
# cob_amp_long$modelo %>% unique()

##
cob_amp_long

# saída latex -------------------------------------------------------------

kbl(
  coefs_vies,
  format = "latex",
  booktabs = TRUE,
  align = "ccccc",
  caption = "Média, viés, RMSE, probabilidade de cobertura e amplitude média para o cenário 3."
) %>%
  kable_styling(
    latex_options = c("hold_position", "scale_down")
  ) #%>%
  # kable_styling(
  #   latex_options = c("hold_position"),
  #   stripe_color = "gray!15"
  # ) %>%
  # row_spec(1:3, background = "gray!15") %>%
  # row_spec(4:6, background = "white") %>%
  # row_spec(7:9, background = "gray!15")
