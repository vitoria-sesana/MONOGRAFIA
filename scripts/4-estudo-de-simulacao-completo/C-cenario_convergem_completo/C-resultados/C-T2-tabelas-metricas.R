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


# função auxiliar ---------------------------------------------------------

gerar_tabela_1 <- function(dados, caption, label) {
  
  tabela <- dados |>
    dplyr::mutate(
      media = sprintf("%.4f", media),
      sd    = sprintf("%.4f", sd),
      vies  = sprintf("%.4f", vies),
      RMSE  = sprintf("%.4f", RMSE)
    ) |>
    knitr::kable(
      format = "latex",
      booktabs = TRUE,
      escape = FALSE,
      caption = caption,
      label = label,
      align = "llllrrr",
      col.names = c(
           "Modelo",
           "\\textit{n}",
           "Parâmetro",
           "Média",
           "SD",
           "Viés",
           "RMSE"
         )
    ) |>
    kableExtra::kable_styling(
      latex_options = "hold_position",
      font_size = 7
    )
  
  tabela <- gsub("\\\\addlinespace", "", tabela)
  
  tabela
}


gerar_tabela_2 <- function(dados, caption, label) {
  
  tabela <- dados |>
    dplyr::mutate(
      prob_c = sprintf("%.4f", prob_c),
      amplitude    = sprintf("%.4f", amplitude)
    ) |>
    knitr::kable(
      format = "latex",
      booktabs = TRUE,
      escape = FALSE,
      caption = caption,
      label = label,
      align = "ccccc",
      col.names = c(
        "Modelo",
        "\\textit{n}",
        "Parâmetro",
        "Probabilidade de Cobertura",
        "Amplitude Média"
        )
    ) |>
    kableExtra::kable_styling(
      latex_options = "hold_position",
      font_size = 7
    )
  
  tabela <- gsub("\\\\addlinespace", "", tabela)
  
  tabela
}



# saída latex -------------------------------------------------------------

gerar_tabela_1(
  coefs_vies,
  "Média, SD, viés e RMSE para o cenário 1.",
  "tab:apend_1_cenario1"
)

gerar_tabela_2(
  cob_amp_long,
  "Probabilidade de cobertura e amplitude média para o cenário 1.",
  "tab:apend_2_cenario1"
)


# saida tabela ------------------------------------------------------------

write.csv(
  coefs_vies,
  "saidas/3-saida-simulacao-completo/C-saidas/C-resultados/C-T1-tabela_apendice_4metricas.csv"
)


write.csv(
  cob_amp_long,
  "saidas/3-saida-simulacao-completo/C-saidas/C-resultados/C-T2-tabela_apendice_2metricas.csv"
)
