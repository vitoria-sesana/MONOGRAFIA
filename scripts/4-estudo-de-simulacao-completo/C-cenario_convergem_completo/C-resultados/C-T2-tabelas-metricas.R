# rotina ------------------------------------------------------------------
source("scripts/0-rotina.R", encoding = "UTF-8")


# leitura -----------------------------------------------------------------
mean_sd <- read.csv("saidas/3-saida-simulacao-completo/C-saidas/C-resultados/C-media_sd.csv")
vies_reqm <- read.csv("saidas/3-saida-simulacao-completo/C-saidas/C-resultados/C-vies_reqm.csv")
pc_am <- read.csv("saidas/3-saida-simulacao-completo/C-saidas/C-resultados/C-pc_am.csv")


# wider -------------------------------------------------------------------

mean_sd_wider <- 
  mean_sd 

vies_reqm_wider <- 
  vies_reqm 

pc_am_wider <- 
  pc_am %>% 
  select(-X) %>% 
  mutate(cenario = '1') %>% 
  select(cenario, parametro, everything()) %>% 
  arrange(parametro)

mean_sd_vies_reqm_wider <-
  mean_sd_wider %>%
  select(-X) %>%
  left_join(
    vies_reqm_wider,
    by = c("amostra_categoria", "parametro")
  )  %>%
  select(
    amostra_categoria, parametro, 
    media_lgb, vies_lgb,  sd_lgb, RMSE_lgb, 
    media_pois, vies_pois, sd_pois, RMSE_pois,
    media_lgb_bayes_media, vies_bayes_media, sd_lgb_bayes_media, RMSE_bayes_media,
    media_lgb_bayes_mediana, vies_bayes_mediana, sd_lgb_bayes_mediana, RMSE_bayes_mediana
  )


mean_vies_wider <-
  mean_sd_vies_reqm_wider %>%
  select(
    amostra_categoria, parametro, 
    media_lgb, vies_lgb,
    media_pois, vies_pois, 
    media_lgb_bayes_media, vies_bayes_media,
    media_lgb_bayes_mediana, vies_bayes_mediana
  ) %>% 
  mutate(cenario = '1') %>% 
  select(cenario, parametro, everything()) %>% 
  arrange(parametro)

sd_reqm_wider <-
  mean_sd_vies_reqm_wider %>%
  select(
    amostra_categoria, parametro, 
    sd_lgb, RMSE_lgb,
    sd_pois, RMSE_pois,
    sd_lgb_bayes_media, RMSE_bayes_media,
    sd_lgb_bayes_mediana, RMSE_bayes_mediana
  ) %>% 
  mutate(cenario = '1') %>% 
  select(cenario, parametro, everything()) %>% 
  arrange(parametro)


# longer ------------------------------------------------------------------

# mean_sd_long <-
#   mean_sd %>%
#   pivot_longer(
#     cols = c(
#       # media_lgb,
#       # sd_lgb,
#       media_pois,
#       sd_pois,
#       media_lgb_bayes_media,
#       sd_lgb_bayes_media,
#       media_lgb_bayes_mediana,
#       sd_lgb_bayes_mediana
#     ),
#     names_to = c(".value", "modelo"),
#     names_pattern = "(media|sd)_(.*)"
#   ) %>%
#   select(-X) %>%
#   mutate(
#     modelo = case_when(
#       # modelo == "lgb" ~ "Log-binomial frequentista",
#       modelo == "lgb_bayes_media" ~ "Log-binomial Bayesiano (Média)",
#       modelo == "lgb_bayes_mediana" ~ "Log-binomial Bayesiano (Mediana)",
#       modelo == "pois" ~ "Poisson robusto",
#       TRUE ~ modelo
#     ),
#     modelo = factor(
#       modelo,
#       levels = c(
#         # "Log-binomial frequentista",
#         "Poisson robusto",
#         "Log-binomial Bayesiano (Média)",
#         "Log-binomial Bayesiano (Mediana)"
#       )
#     )
#   ) %>%
#   select(modelo, amostra_categoria, parametro, everything()) %>%
#   arrange(modelo, amostra_categoria, parametro)
# 
# vies_reqm_long <-
#   vies_reqm %>%
#   pivot_longer(
#     cols = c(
#       # vies_reqm_lgb,
#       # RMSE_lgb,
#       vies_pois,
#       RMSE_pois,
#       vies_bayes_media,
#       RMSE_bayes_media,
#       vies_bayes_mediana,
#       RMSE_bayes_mediana
#     ),
#     names_to = c(".value", "modelo"),
#     names_pattern = "(vies_reqm|RMSE)_(.*)"
#   ) %>%
#   select(-X) %>%
#   mutate(
#     modelo = case_when(
#       # modelo == "lgb" ~ "Log-binomial frequentista",
#       modelo == "bayes_media" ~ "Log-binomial Bayesiano (Média)",
#       modelo == "bayes_mediana" ~ "Log-binomial Bayesiano (Mediana)",
#       modelo == "pois" ~ "Poisson robusto",
#       TRUE ~ modelo
#     ),
#     modelo = factor(
#       modelo,
#       levels = c(
#         # "Log-binomial frequentista",
#         "Poisson robusto",
#         "Log-binomial Bayesiano (Média)",
#         "Log-binomial Bayesiano (Mediana)"
#       )
#     )
#   ) %>%
#   select(modelo, amostra_categoria, parametro, everything()) %>%
#   arrange(modelo, amostra_categoria, parametro)
# 
# pc_am_long <-
#   pc_am %>%
#   rename(
#     # "prob_c_lgb_freq" = prob_c_freq,
#     "prob_c_pois" = prob_c_poiss,
#     "prob_c_bayes_quantilica" = probabilidade_cobertura_quantilica,
#     "prob_c_bayes_HPD" = probabilidade_cobertura_HPD
#   ) %>%
#   pivot_longer(
#     cols = c(
#       # prob_c_lgb_freq,
#       # amlitude_lgb_freq,
#       prob_c_pois,
#       amplitude_pois,
#       prob_c_bayes_quantilica,
#       amplitude_bayes_quantilica,
#       prob_c_bayes_HPD,
#       amplitude_bayes_HPD
#     ),
#     names_to = c(".value", "modelo"),
#     names_pattern = "(prob_c|amlitude)_(.*)"
#   ) %>%
#   select(-X) %>%
#   mutate(
#     modelo = case_when(
#       # modelo == "lgb_freq" ~ "Log-binomial frequentista",
#       modelo == "bayes_quantilica" ~ "Log-binomial Bayesiano (Quantis)",
#       modelo == "bayes_HPD" ~ "Log-binomial Bayesiano (HPD)",
#       modelo == "pois" ~ "Poisson robusto",
#       TRUE ~ modelo
#     ),
#     modelo = factor(
#       modelo,
#       levels = c(
#         # "Log-binomial frequentista",
#         "Poisson robusto",
#         "Log-binomial Bayesiano (Quantis)",
#         "Log-binomial Bayesiano (HPD)"
#       )
#     )
#   ) %>%
#   select(modelo, amostra_categoria, parametro, everything()) %>%
#   arrange(modelo, amostra_categoria, parametro)
# 
# mutate(
#   parametro = recode(
#     parametro,
#     "b0" = "Intercepto",
#     "b1" = "Centro 1",
#     "b2" = "Tratamento P",
#     "b3" = "Estado inicial 0"
#   )
# )


# saida tabela ------------------------------------------------------------

write.csv(
  mean_vies_wider,
  "saidas/3-saida-simulacao-completo/C-saidas/C-resultados/C-T1_apendice_mean_vies.csv"
)


write.csv(
  sd_reqm_wider,
  "saidas/3-saida-simulacao-completo/C-saidas/C-resultados/C-T2_apendice_sd_reqm.csv"
)


write.csv(
  pc_am_wider,
  "saidas/3-saida-simulacao-completo/C-saidas/C-resultados/C-T3_apendice_pc_am.csv"
)

