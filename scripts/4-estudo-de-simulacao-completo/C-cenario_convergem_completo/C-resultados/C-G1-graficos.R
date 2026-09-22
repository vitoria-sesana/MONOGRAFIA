# rotina ------------------------------------------------------------------
source("scripts/0-rotina.R", encoding = "UTF-8")


# leitura -----------------------------------------------------------------
coefs <- read.csv("saidas/3-saida-simulacao-completo/C-saidas/C-resultados/C-media_sd.csv")
vies <- read.csv("saidas/3-saida-simulacao-completo/C-saidas/C-resultados/C-vies_reqm.csv")
cob_amp <- read.csv("saidas/3-saida-simulacao-completo/C-saidas/C-resultados/C-pc_am.csv")

# valores reais dos parâmetros --------------------------------------------

modelo_base <- 
  readRDS("saidas/1-saida-aplicacao/ajuste_logbin_frequentista.rds") 

valores_reais <- 
  modelo_base$modelo_logbin_frequentista$coefficients %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column() %>% 
  rename(parametro_descricao = "rowname", valor = ".") %>% 
  mutate(
    parametro =  case_when(
      parametro_descricao == "(Intercept)" ~ "b0",
      parametro_descricao == "center1" ~ "b1",
      parametro_descricao == "treatP" ~ "b2",
      parametro_descricao == "baseline0" ~ "b3",
      .default = "NA"
    )
  ) %>% 
  select(parametro, valor) %>% 
  as_tibble()

valor_b0 <- valores_reais %>%
  filter(parametro == "b0") %>%
  pull(valor)

valor_b1 <- valores_reais %>%
  filter(parametro == "b1") %>%
  pull(valor)

valor_b2 <- valores_reais %>%
  filter(parametro == "b2") %>%
  pull(valor)

valor_b3 <- valores_reais %>%
  filter(parametro == "b3") %>%
  pull(valor)


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

tabela_2 <- 
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

tabela_1 <- coefs_long %>%
  left_join(
    vies_long,
    by = c("modelo", "amostra_categoria", "parametro")
  ) %>% 
  mutate(
    amostra_categoria = factor(
      amostra_categoria,
      levels = c(50, 100, 200)
    )
  ) 
  

tabela_2 <- tabela_2 %>% 
  mutate(
    amostra_categoria = factor(
      amostra_categoria,
      levels = c(50, 100, 200)
    )
  ) 


# B0 ----------------------------------------------------------------------


## media -----
gg_b0_media <-
  tabela_1 %>% 
    filter(parametro == "b0") %>% 
    ggplot(aes(x = amostra_categoria, y = media, color = modelo)) +
    geom_line(aes(group = modelo), linewidth = 0.65) +
    geom_point(size = 1.1) +
    geom_hline(yintercept = valor_b0, 
               linetype = "longdash", 
               color = "#333333", 
               linewidth = 0.9) +
    annotate(
      "text",
      x = 1,
      y = valor_b0,
      label = "Valor real",
      hjust = 0,
      vjust = 1.5,
      color = "#333333",
      size = 3
    ) +
    labs(
      title = TeX("$\\beta_{0}$: Intercepto"),
      x = "Tamanho amostral (n)",
      y = "Valor",
      color = "Modelos"
    ) +
    theme_minimal() +
    theme(
      panel.background =  element_rect(color = "grey70"),
      panel.border = element_rect(color = "grey70", fill = NA, size = 0.5),
      axis.text.x   = element_text(color = "grey20",size = 7),
      axis.text.y   = element_text(color = "grey20",size = 7),
      axis.title.x  = element_text(color = "grey20",face = "bold", size = 8),
      axis.title.y  = element_text(color = "grey20",face = "bold", size = 8),
      title =  element_text(color = "grey10",face = "bold", size = 14),
      legend.title = element_text(size = 8),
      legend.text = element_text(color = "grey20", size = 7),
      panel.grid.minor.x = element_blank()
    ) +
    scale_color_manual(values = c(
      'Log-binomial frequentista' = "#D55E00",         
      'Poisson robusto' = "#0072B2",                  
      'Log-binomial Bayesiano (Média)' = "#e7298a",     
      'Log-binomial Bayesiano (Mediana)' = "#009E73"    
    )) +
    scale_x_discrete(
      expand = expansion(mult = c(0.02, 0.02))
    ) + 
   ylim(-0.7, 0)

## vies -----
gg_b0_vies <-
  tabela_1 %>% 
    filter(parametro == "b0") %>% 
    ggplot(aes(x = amostra_categoria, y = vies, color = modelo)) +
    geom_line(aes(group = modelo), linewidth = 0.65) +
    geom_point(size = 1.1) +
    geom_hline(yintercept = 0, 
               linetype = "longdash", 
               color = "#333333", 
               linewidth = 0.9) +
    # annotate(
    #   "text",
    #   x = 1,
    #   y = 0,
    #   # label = "Valor real",
    #   hjust = 0,
    #   vjust = 1.5,
    #   color = "#333333",
    #   size = 3
    # ) +
    labs(
      title = TeX("$\\beta_{0}$: Intercepto"),
      x = "Tamanho amostral (n)",
      y = "Valor",
      color = "Modelos"
    ) +
    theme_minimal() +
    theme(
      panel.background =  element_rect(color = "grey70"),
      panel.border = element_rect(color = "grey70", fill = NA, size = 0.5),
      axis.text.x   = element_text(color = "grey20",size = 7),
      axis.text.y   = element_text(color = "grey20",size = 7),
      axis.title.x  = element_text(color = "grey20",face = "bold", size = 8),
      axis.title.y  = element_text(color = "grey20",face = "bold", size = 8),
      title =  element_text(color = "grey10",face = "bold", size = 14),
      legend.title = element_text(size = 8),
      legend.text = element_text(color = "grey20", size = 7),
      panel.grid.minor.x = element_blank()
    ) +
    scale_color_manual(values = c(
    'Log-binomial frequentista' = "#D55E00",         
    'Poisson robusto' = "#0072B2",                  
    'Log-binomial Bayesiano (Média)' = "#e7298a",     
    'Log-binomial Bayesiano (Mediana)' = "#009E73"    
    )) +
    scale_x_discrete(
      expand = expansion(mult = c(0.02, 0.02))
    ) +
  ylim(-0.3, 0.1)

## sd -----
gg_b0_sd <-
  tabela_1 %>% 
    filter(parametro == "b0") %>% 
    ggplot(aes(x = amostra_categoria, y = sd, color = modelo)) +
    geom_line(aes(group = modelo), linewidth = 0.65) +
    geom_point(size = 1.1) +
    geom_hline(yintercept = 0, 
               linetype = "longdash", 
               color = "#333333", 
               linewidth = 0.9) +
    # annotate(
    #   "text",
    #   x = 1,
    #   y = 0,
    #   # label = "Valor real",
    #   hjust = 0,
    #   vjust = 1.5,
    #   color = "#333333",
    #   size = 3
    # ) +
    labs(
      title = TeX("$\\beta_{0}$: Intercepto"),
      x = "Tamanho amostral (n)",
      y = "Valor",
      color = "Modelos"
    ) +
    theme_minimal() +
    theme(
      panel.background =  element_rect(color = "grey70"),
      panel.border = element_rect(color = "grey70", fill = NA, size = 0.5),
      axis.text.x   = element_text(color = "grey20",size = 7),
      axis.text.y   = element_text(color = "grey20",size = 7),
      axis.title.x  = element_text(color = "grey20",face = "bold", size = 8),
      axis.title.y  = element_text(color = "grey20",face = "bold", size = 8),
      title =  element_text(color = "grey10",face = "bold", size = 14),
      legend.title = element_text(size = 8),
      legend.text = element_text(color = "grey20", size = 7),
      panel.grid.minor.x = element_blank()
    ) +
    scale_color_manual(values = c(
      'Log-binomial frequentista' = "#D55E00",
      'Poisson robusto' = "#0072B2",
      'Log-binomial Bayesiano (Média)' = "#e7298a",
      'Log-binomial Bayesiano (Mediana)' = "#009E73"
    )) +
    scale_x_discrete(
      expand = expansion(mult = c(0.02, 0.02))
    ) +
  ylim(0, 0.35)


## reqm -----
gg_b0_reqm <-
  tabela_1 %>% 
  filter(parametro == "b0") %>% 
  ggplot(aes(x = amostra_categoria, y = RMSE, color = modelo)) +
  geom_line(aes(group = modelo), linewidth = 0.65) +
  geom_point(size = 1.1) +
  geom_hline(yintercept = 0, 
             linetype = "longdash", 
             color = "#333333", 
             linewidth = 0.9) +
  # annotate(
  #   "text",
  #   x = 1,
  #   y = 0,
  #   # label = "Valor real",
  #   hjust = 0,
  #   vjust = 1.5,
  #   color = "#333333",
  #   size = 3
  # ) +
  labs(
    title = TeX("$\\beta_{0}$: Intercepto"),
    x = "Tamanho amostral (n)",
    y = "Valor",
    color = "Modelos"
  ) +
  theme_minimal() +
  theme(
    panel.background =  element_rect(color = "grey70"),
    panel.border = element_rect(color = "grey70", fill = NA, size = 0.5),
    axis.text.x   = element_text(color = "grey20",size = 7),
    axis.text.y   = element_text(color = "grey20",size = 7),
    axis.title.x  = element_text(color = "grey20",face = "bold", size = 8),
    axis.title.y  = element_text(color = "grey20",face = "bold", size = 8),
    title =  element_text(color = "grey10",face = "bold", size = 14),
    legend.title = element_text(size = 8),
    legend.text = element_text(color = "grey20", size = 7),
    panel.grid.minor.x = element_blank()
  ) +
  scale_color_manual(values = c(
    'Log-binomial frequentista' = "#D55E00",         
    'Poisson robusto' = "#0072B2",                  
    'Log-binomial Bayesiano (Média)' = "#e7298a",     
    'Log-binomial Bayesiano (Mediana)' = "#009E73"    
  )) +
  scale_x_discrete(
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  ylim(0, 0.35)



## prob -----
gg_b0_prob <-
  tabela_2 %>% 
  filter(parametro == "b0") %>% 
  ggplot(aes(x = amostra_categoria, y = prob_c , color = modelo)) +
  geom_line(aes(group = modelo), linewidth = 0.65) +
  geom_point(size = 1.1) +
  geom_hline(yintercept = 1, 
             linetype = "longdash", 
             color = "#333333", 
             linewidth = 0.9) +
  # annotate(
  #   "text",
  #   x = 1,
  #   y = 0,
  #   # label = "Valor real",
  #   hjust = 0,
  #   vjust = 1.5,
  #   color = "#333333",
  #   size = 3
  # ) +
  labs(
    title = TeX("$\\beta_{0}$: Intercepto"),
    x = "Tamanho amostral (n)",
    y = "Valor",
    color = "Modelos"
  ) +
  theme_minimal() +
  theme(
    panel.background =  element_rect(color = "grey70"),
    panel.border = element_rect(color = "grey70", fill = NA, size = 0.5),
    axis.text.x   = element_text(color = "grey20",size = 7),
    axis.text.y   = element_text(color = "grey20",size = 7),
    axis.title.x  = element_text(color = "grey20",face = "bold", size = 8),
    axis.title.y  = element_text(color = "grey20",face = "bold", size = 8),
    title =  element_text(color = "grey10",face = "bold", size = 14),
    legend.title = element_text(size = 8),
    legend.text = element_text(color = "grey20", size = 7),
    panel.grid.minor.x = element_blank()
  ) +
  scale_color_manual(values = c(
    'Log-binomial frequentista' = "#D55E00",
    'Poisson robusto' = "#0072B2",
    'Log-binomial Bayesiano (Quantis)' = "#FF0000",
    'Log-binomial Bayesiano (HPD)' = "#AA4499"
  )) +
  scale_x_discrete(
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  ylim(0.80, 1)



## amplitude ---------
gg_b0_ampli <-
  tabela_2 %>% 
  filter(parametro == "b0") %>% 
  ggplot(aes(x = amostra_categoria, y = amplitude , color = modelo)) +
  geom_line(aes(group = modelo), linewidth = 0.65) +
  geom_point(size = 1.1) +
  labs(
    title = TeX("$\\beta_{0}$: Intercepto"),
    x = "Tamanho amostral (n)",
    y = "Valor",
    color = "Modelos"
  ) +
  theme_minimal() +
  theme(
    panel.background =  element_rect(color = "grey70"),
    panel.border = element_rect(color = "grey70", fill = NA, size = 0.5),
    axis.text.x   = element_text(color = "grey20",size = 7),
    axis.text.y   = element_text(color = "grey20",size = 7),
    axis.title.x  = element_text(color = "grey20",face = "bold", size = 8),
    axis.title.y  = element_text(color = "grey20",face = "bold", size = 8),
    title =  element_text(color = "grey10",face = "bold", size = 14),
    legend.title = element_text(size = 8),
    legend.text = element_text(color = "grey20", size = 7),
    panel.grid.minor.x = element_blank()
  ) +
  scale_color_manual(values = c(
    'Log-binomial frequentista' = "#D55E00",
    'Poisson robusto' = "#0072B2",
    'Log-binomial Bayesiano (Quantis)' = "#FF0000",
    'Log-binomial Bayesiano (HPD)' = "#AA4499"
  )) +
  scale_x_discrete(
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  ylim(-0.1, 1.25)

# gg_b0_media
# gg_b0_sd
# gg_b0_vies
# gg_b0_reqm
# gg_b0_prob
# gg_b0_ampli



# B1 ----------------------------------------------------------------------

## media -----
gg_b1_media <-
  tabela_1 %>% 
  filter(parametro == "b1") %>% 
  ggplot(aes(x = amostra_categoria, y = media, color = modelo)) +
  geom_line(aes(group = modelo), linewidth = 0.65) +
  geom_point(size = 1.1) +
  geom_hline(yintercept = valor_b1, 
             linetype = "longdash", 
             color = "#333333", 
             linewidth = 0.9) +
  annotate(
    "text",
    x = 1,
    y = valor_b1,
    label = "Valor real",
    hjust = 0,
    vjust = 1.5,
    color = "#333333",
    size = 3
  ) +
  labs(
    title = TeX("$\\beta_{1}$: Centro"),
    x = "Tamanho amostral (n)",
    y = "Valor",
    color = "Modelos"
  ) +
  theme_minimal() +
  theme(
    panel.background =  element_rect(color = "grey70"),
    panel.border = element_rect(color = "grey70", fill = NA, size = 0.5),
    axis.text.x   = element_text(color = "grey20",size = 7),
    axis.text.y   = element_text(color = "grey20",size = 7),
    axis.title.x  = element_text(color = "grey20",face = "bold", size = 8),
    axis.title.y  = element_text(color = "grey20",face = "bold", size = 8),
    title =  element_text(color = "grey10",face = "bold", size = 14),
    legend.title = element_text(size = 8),
    legend.text = element_text(color = "grey20", size = 7),
    panel.grid.minor.x = element_blank()
  ) +
  scale_color_manual(values = c(
    'Log-binomial frequentista' = "#D55E00",
    'Poisson robusto' = "#0072B2",
    'Log-binomial Bayesiano (Média)' = "#e7298a",
    'Log-binomial Bayesiano (Mediana)' = "#009E73"
  )) +
  scale_x_discrete(
    expand = expansion(mult = c(0.02, 0.02))
  ) + 
   ylim(-0.7, 0)

## vies -----
gg_b1_vies <-
  tabela_1 %>% 
  filter(parametro == "b1") %>% 
  ggplot(aes(x = amostra_categoria, y = vies, color = modelo)) +
  geom_line(aes(group = modelo), linewidth = 0.65) +
  geom_point(size = 1.1) +
  geom_hline(yintercept = 0, 
             linetype = "longdash", 
             color = "#333333", 
             linewidth = 0.9) +
  # annotate(
  #   "text",
  #   x = 1,
  #   y = 0,
  #   # label = "Valor real",
  #   hjust = 0,
  #   vjust = 1.5,
  #   color = "#333333",
  #   size = 3
  # ) +
  labs(
    title = TeX("$\\beta_{1}$: Centro"),
    x = "Tamanho amostral (n)",
    y = "Valor",
    color = "Modelos"
  ) +
  theme_minimal() +
  theme(
    panel.background =  element_rect(color = "grey70"),
    panel.border = element_rect(color = "grey70", fill = NA, size = 0.5),
    axis.text.x   = element_text(color = "grey20",size = 7),
    axis.text.y   = element_text(color = "grey20",size = 7),
    axis.title.x  = element_text(color = "grey20",face = "bold", size = 8),
    axis.title.y  = element_text(color = "grey20",face = "bold", size = 8),
    title =  element_text(color = "grey10",face = "bold", size = 14),
    legend.title = element_text(size = 8),
    legend.text = element_text(color = "grey20", size = 7),
    panel.grid.minor.x = element_blank()
  ) +
  scale_color_manual(values = c(
    'Log-binomial frequentista' = "#D55E00",
    'Poisson robusto' = "#0072B2",
    'Log-binomial Bayesiano (Média)' = "#e7298a",
    'Log-binomial Bayesiano (Mediana)' = "#009E73"
  )) +
  scale_x_discrete(
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  ylim(-0.3, 0.1)

## sd -----
gg_b1_sd <-
  tabela_1 %>% 
  filter(parametro == "b1") %>% 
  ggplot(aes(x = amostra_categoria, y = sd, color = modelo)) +
  geom_line(aes(group = modelo), linewidth = 0.65) +
  geom_point(size = 1.1) +
  geom_hline(yintercept = 0, 
             linetype = "longdash", 
             color = "#333333", 
             linewidth = 0.9) +
  # annotate(
  #   "text",
  #   x = 1,
  #   y = 0,
  #   # label = "Valor real",
  #   hjust = 0,
  #   vjust = 1.5,
  #   color = "#333333",
  #   size = 3
  # ) +
  labs(
    title = TeX("$\\beta_{1}$: Centro"),
    x = "Tamanho amostral (n)",
    y = "Valor",
    color = "Modelos"
  ) +
  theme_minimal() +
  theme(
    panel.background =  element_rect(color = "grey70"),
    panel.border = element_rect(color = "grey70", fill = NA, size = 0.5),
    axis.text.x   = element_text(color = "grey20",size = 7),
    axis.text.y   = element_text(color = "grey20",size = 7),
    axis.title.x  = element_text(color = "grey20",face = "bold", size = 8),
    axis.title.y  = element_text(color = "grey20",face = "bold", size = 8),
    title =  element_text(color = "grey10",face = "bold", size = 14),
    legend.title = element_text(size = 8),
    legend.text = element_text(color = "grey20", size = 7),
    panel.grid.minor.x = element_blank()
  ) +
  scale_color_manual(values = c(
    'Log-binomial frequentista' = "#D55E00",
    'Poisson robusto' = "#0072B2",
    'Log-binomial Bayesiano (Média)' = "#e7298a",
    'Log-binomial Bayesiano (Mediana)' = "#009E73"
  )) +
  scale_x_discrete(
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  ylim(0, 0.35)


## reqm -----
gg_b1_reqm <-
  tabela_1 %>% 
  filter(parametro == "b1") %>% 
  ggplot(aes(x = amostra_categoria, y = RMSE, color = modelo)) +
  geom_line(aes(group = modelo), linewidth = 0.65) +
  geom_point(size = 1.1) +
  geom_hline(yintercept = 0, 
             linetype = "longdash", 
             color = "#333333", 
             linewidth = 0.9) +
  # annotate(
  #   "text",
  #   x = 1,
  #   y = 0,
  #   # label = "Valor real",
  #   hjust = 0,
  #   vjust = 1.5,
  #   color = "#333333",
  #   size = 3
  # ) +
  labs(
    title = TeX("$\\beta_{1}$: Centro"),
    x = "Tamanho amostral (n)",
    y = "Valor",
    color = "Modelos"
  ) +
  theme_minimal() +
  theme(
    panel.background =  element_rect(color = "grey70"),
    panel.border = element_rect(color = "grey70", fill = NA, size = 0.5),
    axis.text.x   = element_text(color = "grey20",size = 7),
    axis.text.y   = element_text(color = "grey20",size = 7),
    axis.title.x  = element_text(color = "grey20",face = "bold", size = 8),
    axis.title.y  = element_text(color = "grey20",face = "bold", size = 8),
    title =  element_text(color = "grey10",face = "bold", size = 14),
    legend.title = element_text(size = 8),
    legend.text = element_text(color = "grey20", size = 7),
    panel.grid.minor.x = element_blank()
  ) +
  scale_color_manual(values = c(
    'Log-binomial frequentista' = "#D55E00",
    'Poisson robusto' = "#0072B2",
    'Log-binomial Bayesiano (Média)' = "#e7298a",
    'Log-binomial Bayesiano (Mediana)' = "#009E73"
  )) +
  scale_x_discrete(
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  ylim(0, 0.35)


## prob -----
gg_b1_prob <-
  tabela_2 %>% 
  filter(parametro == "b1") %>% 
  ggplot(aes(x = amostra_categoria, y = prob_c , color = modelo)) +
  geom_line(aes(group = modelo), linewidth = 0.65) +
  geom_point(size = 1.1) +
  geom_hline(yintercept = 1, 
             linetype = "longdash", 
             color = "#333333", 
             linewidth = 0.9) +
  # annotate(
  #   "text",
  #   x = 1,
  #   y = 0,
  #   # label = "Valor real",
  #   hjust = 0,
  #   vjust = 1.5,
  #   color = "#333333",
  #   size = 3
  # ) +
  labs(
    title = TeX("$\\beta_{1}$: Centro"),
    x = "Tamanho amostral (n)",
    y = "Valor",
    color = "Modelos"
  ) +
  theme_minimal() +
  theme(
    panel.background =  element_rect(color = "grey70"),
    panel.border = element_rect(color = "grey70", fill = NA, size = 0.5),
    axis.text.x   = element_text(color = "grey20",size = 7),
    axis.text.y   = element_text(color = "grey20",size = 7),
    axis.title.x  = element_text(color = "grey20",face = "bold", size = 8),
    axis.title.y  = element_text(color = "grey20",face = "bold", size = 8),
    title =  element_text(color = "grey10",face = "bold", size = 14),
    legend.title = element_text(size = 8),
    legend.text = element_text(color = "grey20", size = 7),
    panel.grid.minor.x = element_blank()
  ) +
  scale_color_manual(values = c(
    'Log-binomial frequentista' = "#D55E00",
    'Poisson robusto' = "#0072B2",
    'Log-binomial Bayesiano (Quantis)' = "#FF0000",
    'Log-binomial Bayesiano (HPD)' = "#AA4499"
  )) +
  scale_x_discrete(
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  ylim(0.80, 1)



## amplitude ---------
gg_b1_ampli <-
  tabela_2 %>% 
  filter(parametro == "b1") %>% 
  ggplot(aes(x = amostra_categoria, y = amplitude , color = modelo)) +
  geom_line(aes(group = modelo), linewidth = 0.65) +
  geom_point(size = 1.1) +
  labs(
    title = TeX("$\\beta_{1}$: Centro"),
    x = "Tamanho amostral (n)",
    y = "Valor",
    color = "Modelos"
  ) +
  theme_minimal() +
  theme(
    panel.background =  element_rect(color = "grey70"),
    panel.border = element_rect(color = "grey70", fill = NA, size = 0.5),
    axis.text.x   = element_text(color = "grey20",size = 7),
    axis.text.y   = element_text(color = "grey20",size = 7),
    axis.title.x  = element_text(color = "grey20",face = "bold", size = 8),
    axis.title.y  = element_text(color = "grey20",face = "bold", size = 8),
    title =  element_text(color = "grey10",face = "bold", size = 14),
    legend.title = element_text(size = 8),
    legend.text = element_text(color = "grey20", size = 7),
    panel.grid.minor.x = element_blank()
  ) +
  scale_color_manual(values = c(
    'Log-binomial frequentista' = "#D55E00",
    'Poisson robusto' = "#0072B2",
    'Log-binomial Bayesiano (Quantis)' = "#FF0000",
    'Log-binomial Bayesiano (HPD)' = "#AA4499"
  )) +
  scale_x_discrete(
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  ylim(-0.1, 1.25)


# gg_b1_media
# gg_b1_sd
# gg_b1_vies
# gg_b1_reqm
# gg_b1_prob
# gg_b1_ampli


# B2 ----------------------------------------------------------------------

## media -----
gg_b2_media <-
  tabela_1 %>% 
  filter(parametro == "b2") %>% 
  ggplot(aes(x = amostra_categoria, y = media, color = modelo)) +
  geom_line(aes(group = modelo), linewidth = 0.65) +
  geom_point(size = 1.1) +
  geom_hline(yintercept = valor_b2, 
             linetype = "longdash", 
             color = "#333333", 
             linewidth = 0.9) +
  annotate(
    "text",
    x = 1,
    y = valor_b2,
    label = "Valor real",
    hjust = 0,
    vjust = 1.5,
    color = "#333333",
    size = 3
  ) +
  labs(
    title = TeX("$\\beta_{2}$: Tratamento"),
    x = "Tamanho amostral (n)",
    y = "Valor",
    color = "Modelos"
  ) +
  theme_minimal() +
  theme(
    panel.background =  element_rect(color = "grey70"),
    panel.border = element_rect(color = "grey70", fill = NA, size = 0.5),
    axis.text.x   = element_text(color = "grey20",size = 7),
    axis.text.y   = element_text(color = "grey20",size = 7),
    axis.title.x  = element_text(color = "grey20",face = "bold", size = 8),
    axis.title.y  = element_text(color = "grey20",face = "bold", size = 8),
    title =  element_text(color = "grey10",face = "bold", size = 14),
    legend.title = element_text(size = 8),
    legend.text = element_text(color = "grey20", size = 7),
    panel.grid.minor.x = element_blank()
  ) +
  scale_color_manual(values = c(
    'Log-binomial frequentista' = "#D55E00",
    'Poisson robusto' = "#0072B2",
    'Log-binomial Bayesiano (Média)' = "#e7298a",
    'Log-binomial Bayesiano (Mediana)' = "#009E73"
  )) +
  scale_x_discrete(
    expand = expansion(mult = c(0.02, 0.02))
  ) + 
   ylim(-0.7, 0)

## vies -----
gg_b2_vies <-
  tabela_1 %>% 
  filter(parametro == "b2") %>% 
  ggplot(aes(x = amostra_categoria, y = vies, color = modelo)) +
  geom_line(aes(group = modelo), linewidth = 0.65) +
  geom_point(size = 1.1) +
  geom_hline(yintercept = 0, 
             linetype = "longdash", 
             color = "#333333", 
             linewidth = 0.9) +
  # annotate(
  #   "text",
  #   x = 1,
  #   y = 0,
  #   # label = "Valor real",
  #   hjust = 0,
  #   vjust = 1.5,
  #   color = "#333333",
  #   size = 3
  # ) +
  labs(
    title = TeX("$\\beta_{2}$: Tratamento"),
    x = "Tamanho amostral (n)",
    y = "Valor",
    color = "Modelos"
  ) +
  theme_minimal() +
  theme(
    panel.background =  element_rect(color = "grey70"),
    panel.border = element_rect(color = "grey70", fill = NA, size = 0.5),
    axis.text.x   = element_text(color = "grey20",size = 7),
    axis.text.y   = element_text(color = "grey20",size = 7),
    axis.title.x  = element_text(color = "grey20",face = "bold", size = 8),
    axis.title.y  = element_text(color = "grey20",face = "bold", size = 8),
    title =  element_text(color = "grey10",face = "bold", size = 14),
    legend.title = element_text(size = 8),
    legend.text = element_text(color = "grey20", size = 7),
    panel.grid.minor.x = element_blank()
  ) +
  scale_color_manual(values = c(
    'Log-binomial frequentista' = "#D55E00",
    'Poisson robusto' = "#0072B2",
    'Log-binomial Bayesiano (Média)' = "#e7298a",
    'Log-binomial Bayesiano (Mediana)' = "#009E73"
  )) +
  scale_x_discrete(
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  ylim(-0.3, 0.1)

## sd -----
gg_b2_sd <-
  tabela_1 %>% 
  filter(parametro == "b2") %>% 
  ggplot(aes(x = amostra_categoria, y = sd, color = modelo)) +
  geom_line(aes(group = modelo), linewidth = 0.65) +
  geom_point(size = 1.1) +
  geom_hline(yintercept = 0, 
             linetype = "longdash", 
             color = "#333333", 
             linewidth = 0.9) +
  # annotate(
  #   "text",
  #   x = 1,
  #   y = 0,
  #   # label = "Valor real",
  #   hjust = 0,
  #   vjust = 1.5,
  #   color = "#333333",
  #   size = 3
  # ) +
  labs(
    title = TeX("$\\beta_{2}$: Tratamento"),
    x = "Tamanho amostral (n)",
    y = "Valor",
    color = "Modelos"
  ) +
  theme_minimal() +
  theme(
    panel.background =  element_rect(color = "grey70"),
    panel.border = element_rect(color = "grey70", fill = NA, size = 0.5),
    axis.text.x   = element_text(color = "grey20",size = 7),
    axis.text.y   = element_text(color = "grey20",size = 7),
    axis.title.x  = element_text(color = "grey20",face = "bold", size = 8),
    axis.title.y  = element_text(color = "grey20",face = "bold", size = 8),
    title =  element_text(color = "grey10",face = "bold", size = 14),
    legend.title = element_text(size = 8),
    legend.text = element_text(color = "grey20", size = 7),
    panel.grid.minor.x = element_blank()
  ) +
  scale_color_manual(values = c(
    'Log-binomial frequentista' = "#D55E00",
    'Poisson robusto' = "#0072B2",
    'Log-binomial Bayesiano (Média)' = "#e7298a",
    'Log-binomial Bayesiano (Mediana)' = "#009E73"
  )) +
  scale_x_discrete(
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  ylim(0, 0.35)


## reqm -----
gg_b2_reqm <-
  tabela_1 %>% 
  filter(parametro == "b2") %>% 
  ggplot(aes(x = amostra_categoria, y = RMSE, color = modelo)) +
  geom_line(aes(group = modelo), linewidth = 0.65) +
  geom_point(size = 1.1) +
  geom_hline(yintercept = 0, 
             linetype = "longdash", 
             color = "#333333", 
             linewidth = 0.9) +
  # annotate(
  #   "text",
  #   x = 1,
  #   y = 0,
  #   # label = "Valor real",
  #   hjust = 0,
  #   vjust = 1.5,
  #   color = "#333333",
  #   size = 3
  # ) +
  labs(
    title = TeX("$\\beta_{2}$: Tratamento"),
    x = "Tamanho amostral (n)",
    y = "Valor",
    color = "Modelos"
  ) +
  theme_minimal() +
  theme(
    panel.background =  element_rect(color = "grey70"),
    panel.border = element_rect(color = "grey70", fill = NA, size = 0.5),
    axis.text.x   = element_text(color = "grey20",size = 7),
    axis.text.y   = element_text(color = "grey20",size = 7),
    axis.title.x  = element_text(color = "grey20",face = "bold", size = 8),
    axis.title.y  = element_text(color = "grey20",face = "bold", size = 8),
    title =  element_text(color = "grey10",face = "bold", size = 14),
    legend.title = element_text(size = 8),
    legend.text = element_text(color = "grey20", size = 7),
    panel.grid.minor.x = element_blank()
  ) +
  scale_color_manual(values = c(
    'Log-binomial frequentista' = "#D55E00",
    'Poisson robusto' = "#0072B2",
    'Log-binomial Bayesiano (Média)' = "#e7298a",
    'Log-binomial Bayesiano (Mediana)' = "#009E73"
  )) +
  scale_x_discrete(
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  ylim(0, 0.35)


## prob -----
gg_b2_prob <-
  tabela_2 %>% 
  filter(parametro == "b2") %>% 
  ggplot(aes(x = amostra_categoria, y = prob_c , color = modelo)) +
  geom_line(aes(group = modelo), linewidth = 0.65) +
  geom_point(size = 1.1) +
  geom_hline(yintercept = 1, 
             linetype = "longdash", 
             color = "#333333", 
             linewidth = 0.9) +
  # annotate(
  #   "text",
  #   x = 1,
  #   y = 0,
  #   # label = "Valor real",
  #   hjust = 0,
  #   vjust = 1.5,
  #   color = "#333333",
  #   size = 3
  # ) +
  labs(
    title = TeX("$\\beta_{2}$: Tratamento"),
    x = "Tamanho amostral (n)",
    y = "Valor",
    color = "Modelos"
  ) +
  theme_minimal() +
  theme(
    panel.background =  element_rect(color = "grey70"),
    panel.border = element_rect(color = "grey70", fill = NA, size = 0.5),
    axis.text.x   = element_text(color = "grey20",size = 7),
    axis.text.y   = element_text(color = "grey20",size = 7),
    axis.title.x  = element_text(color = "grey20",face = "bold", size = 8),
    axis.title.y  = element_text(color = "grey20",face = "bold", size = 8),
    title =  element_text(color = "grey10",face = "bold", size = 14),
    legend.title = element_text(size = 8),
    legend.text = element_text(color = "grey20", size = 7),
    panel.grid.minor.x = element_blank()
  ) +
  scale_color_manual(values = c(
    'Log-binomial frequentista' = "#D55E00",
    'Poisson robusto' = "#0072B2",
    'Log-binomial Bayesiano (Quantis)' = "#FF0000",
    'Log-binomial Bayesiano (HPD)' = "#AA4499"
  )) +
  scale_x_discrete(
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  ylim(0.80, 1)



## amplitude ---------
gg_b2_ampli <-
  tabela_2 %>% 
  filter(parametro == "b2") %>% 
  ggplot(aes(x = amostra_categoria, y = amplitude , color = modelo)) +
  geom_line(aes(group = modelo), linewidth = 0.65) +
  geom_point(size = 1.1) +
  labs(
    title = TeX("$\\beta_{2}$: Tratamento"),
    x = "Tamanho amostral (n)",
    y = "Valor",
    color = "Modelos"
  ) +
  theme_minimal() +
  theme(
    panel.background =  element_rect(color = "grey70"),
    panel.border = element_rect(color = "grey70", fill = NA, size = 0.5),
    axis.text.x   = element_text(color = "grey20",size = 7),
    axis.text.y   = element_text(color = "grey20",size = 7),
    axis.title.x  = element_text(color = "grey20",face = "bold", size = 8),
    axis.title.y  = element_text(color = "grey20",face = "bold", size = 8),
    title =  element_text(color = "grey10",face = "bold", size = 14),
    legend.title = element_text(size = 8),
    legend.text = element_text(color = "grey20", size = 7),
    panel.grid.minor.x = element_blank()
  ) +
  scale_color_manual(values = c(
    'Log-binomial frequentista' = "#D55E00",
    'Poisson robusto' = "#0072B2",
    'Log-binomial Bayesiano (Quantis)' = "#FF0000",
    'Log-binomial Bayesiano (HPD)' = "#AA4499"
  )) +
  scale_x_discrete(
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  ylim(-0.1, 1.25)



# B3 ----------------------------------------------------------------------

## media -----
gg_b3_media <-
  tabela_1 %>% 
  filter(parametro == "b3") %>% 
  ggplot(aes(x = amostra_categoria, y = media, color = modelo)) +
  geom_line(aes(group = modelo), linewidth = 0.65) +
  geom_point(size = 1.1) +
  geom_hline(yintercept = valor_b3, 
             linetype = "longdash", 
             color = "#333333", 
             linewidth = 0.9) +
  annotate(
    "text",
    x = 1,
    y = valor_b3,
    label = "Valor real",
    hjust = 0,
    vjust = 1.5,
    color = "#333333",
    size = 3
  ) +
  labs(
    title = TeX("$\\beta_{3}$: Estado inicial"),
    x = "Tamanho amostral (n)",
    y = "Valor",
    color = "Modelos"
  ) +
  theme_minimal() +
  theme(
    panel.background =  element_rect(color = "grey70"),
    panel.border = element_rect(color = "grey70", fill = NA, size = 0.5),
    axis.text.x   = element_text(color = "grey20",size = 7),
    axis.text.y   = element_text(color = "grey20",size = 7),
    axis.title.x  = element_text(color = "grey20",face = "bold", size = 8),
    axis.title.y  = element_text(color = "grey20",face = "bold", size = 8),
    title =  element_text(color = "grey10",face = "bold", size = 14),
    legend.title = element_text(size = 8),
    legend.text = element_text(color = "grey20", size = 7),
    panel.grid.minor.x = element_blank()
  ) +
  scale_color_manual(values = c(
    'Log-binomial frequentista' = "#D55E00",
    'Poisson robusto' = "#0072B2",
    'Log-binomial Bayesiano (Média)' = "#e7298a",
    'Log-binomial Bayesiano (Mediana)' = "#009E73"
  )) +
  scale_x_discrete(
    expand = expansion(mult = c(0.02, 0.02))
  )  + 
   ylim(-0.7, 0)

## vies -----
gg_b3_vies <-
  tabela_1 %>% 
  filter(parametro == "b3") %>% 
  ggplot(aes(x = amostra_categoria, y = vies, color = modelo)) +
  geom_line(aes(group = modelo), linewidth = 0.65) +
  geom_point(size = 1.1) +
  geom_hline(yintercept = 0, 
             linetype = "longdash", 
             color = "#333333", 
             linewidth = 0.9) +
  # annotate(
  #   "text",
  #   x = 1,
  #   y = 0,
  #   # label = "Valor real",
  #   hjust = 0,
  #   vjust = 1.5,
  #   color = "#333333",
  #   size = 3
  # ) +
  labs(
    title = TeX("$\\beta_{3}$: Estado inicial"),
    x = "Tamanho amostral (n)",
    y = "Valor",
    color = "Modelos"
  ) +
  theme_minimal() +
  theme(
    panel.background =  element_rect(color = "grey70"),
    panel.border = element_rect(color = "grey70", fill = NA, size = 0.5),
    axis.text.x   = element_text(color = "grey20",size = 7),
    axis.text.y   = element_text(color = "grey20",size = 7),
    axis.title.x  = element_text(color = "grey20",face = "bold", size = 8),
    axis.title.y  = element_text(color = "grey20",face = "bold", size = 8),
    title =  element_text(color = "grey10",face = "bold", size = 14),
    legend.title = element_text(size = 8),
    legend.text = element_text(color = "grey20", size = 7),
    panel.grid.minor.x = element_blank()
  ) +
  scale_color_manual(values = c(
    'Log-binomial frequentista' = "#D55E00",
    'Poisson robusto' = "#0072B2",
    'Log-binomial Bayesiano (Média)' = "#e7298a",
    'Log-binomial Bayesiano (Mediana)' = "#009E73"
  )) +
  scale_x_discrete(
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  ylim(-0.3, 0.1)

## sd -----
gg_b3_sd <-
  tabela_1 %>% 
  filter(parametro == "b3") %>% 
  ggplot(aes(x = amostra_categoria, y = sd, color = modelo)) +
  geom_line(aes(group = modelo), linewidth = 0.65) +
  geom_point(size = 1.1) +
  geom_hline(yintercept = 0, 
             linetype = "longdash", 
             color = "#333333", 
             linewidth = 0.9) +
  # annotate(
  #   "text",
  #   x = 1,
  #   y = 0,
  #   # label = "Valor real",
  #   hjust = 0,
  #   vjust = 1.5,
  #   color = "#333333",
  #   size = 3
  # ) +
  labs(
    title = TeX("$\\beta_{3}$: Estado inicial"),
    x = "Tamanho amostral (n)",
    y = "Valor",
    color = "Modelos"
  ) +
  theme_minimal() +
  theme(
    panel.background =  element_rect(color = "grey70"),
    panel.border = element_rect(color = "grey70", fill = NA, size = 0.5),
    axis.text.x   = element_text(color = "grey20",size = 7),
    axis.text.y   = element_text(color = "grey20",size = 7),
    axis.title.x  = element_text(color = "grey20",face = "bold", size = 8),
    axis.title.y  = element_text(color = "grey20",face = "bold", size = 8),
    title =  element_text(color = "grey10",face = "bold", size = 14),
    legend.title = element_text(size = 8),
    legend.text = element_text(color = "grey20", size = 7),
    panel.grid.minor.x = element_blank()
  ) +
  scale_color_manual(values = c(
    'Log-binomial frequentista' = "#D55E00",
    'Poisson robusto' = "#0072B2",
    'Log-binomial Bayesiano (Média)' = "#e7298a",
    'Log-binomial Bayesiano (Mediana)' = "#009E73"
  )) +
  scale_x_discrete(
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  ylim(0, 0.35)


## reqm -----
gg_b3_reqm <-
  tabela_1 %>% 
  filter(parametro == "b3") %>% 
  ggplot(aes(x = amostra_categoria, y = RMSE, color = modelo)) +
  geom_line(aes(group = modelo), linewidth = 0.65) +
  geom_point(size = 1.1) +
  geom_hline(yintercept = 0, 
             linetype = "longdash", 
             color = "#333333", 
             linewidth = 0.9) +
  # annotate(
  #   "text",
  #   x = 1,
  #   y = 0,
  #   # label = "Valor real",
  #   hjust = 0,
  #   vjust = 1.5,
  #   color = "#333333",
  #   size = 3
  # ) +
  labs(
    title = TeX("$\\beta_{3}$: Estado inicial"),
    x = "Tamanho amostral (n)",
    y = "Valor",
    color = "Modelos"
  ) +
  theme_minimal() +
  theme(
    panel.background =  element_rect(color = "grey70"),
    panel.border = element_rect(color = "grey70", fill = NA, size = 0.5),
    axis.text.x   = element_text(color = "grey20",size = 7),
    axis.text.y   = element_text(color = "grey20",size = 7),
    axis.title.x  = element_text(color = "grey20",face = "bold", size = 8),
    axis.title.y  = element_text(color = "grey20",face = "bold", size = 8),
    title =  element_text(color = "grey10",face = "bold", size = 14),
    legend.title = element_text(size = 8),
    legend.text = element_text(color = "grey20", size = 7),
    panel.grid.minor.x = element_blank()
  ) +
  scale_color_manual(values = c(
    'Log-binomial frequentista' = "#D55E00",
    'Poisson robusto' = "#0072B2",
    'Log-binomial Bayesiano (Média)' = "#e7298a",
    'Log-binomial Bayesiano (Mediana)' = "#009E73"
  )) +
  scale_x_discrete(
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  ylim(0, 0.35)


## prob -----
gg_b3_prob <-
  tabela_2 %>% 
  filter(parametro == "b3") %>% 
  ggplot(aes(x = amostra_categoria, y = prob_c , color = modelo)) +
  geom_line(aes(group = modelo), linewidth = 0.65) +
  geom_point(size = 1.1) +
  geom_hline(yintercept = 1, 
             linetype = "longdash", 
             color = "#333333", 
             linewidth = 0.9) +
  # annotate(
  #   "text",
  #   x = 1,
  #   y = 0,
  #   # label = "Valor real",
  #   hjust = 0,
  #   vjust = 1.5,
  #   color = "#333333",
  #   size = 3
  # ) +
  labs(
    title = TeX("$\\beta_{3}$: Estado inicial"),
    x = "Tamanho amostral (n)",
    y = "Valor",
    color = "Modelos"
  ) +
  theme_minimal() +
  theme(
    panel.background =  element_rect(color = "grey70"),
    panel.border = element_rect(color = "grey70", fill = NA, size = 0.5),
    axis.text.x   = element_text(color = "grey20",size = 7),
    axis.text.y   = element_text(color = "grey20",size = 7),
    axis.title.x  = element_text(color = "grey20",face = "bold", size = 8),
    axis.title.y  = element_text(color = "grey20",face = "bold", size = 8),
    title =  element_text(color = "grey10",face = "bold", size = 14),
    legend.title = element_text(size = 8),
    legend.text = element_text(color = "grey20", size = 7),
    panel.grid.minor.x = element_blank()
  ) +
  scale_color_manual(values = c(
    'Log-binomial frequentista' = "#D55E00",
    'Poisson robusto' = "#0072B2",
    'Log-binomial Bayesiano (Quantis)' = "#FF0000",
    'Log-binomial Bayesiano (HPD)' = "#AA4499" 
  )) +
  scale_x_discrete(
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  ylim(0.80, 1)



## amplitude ---------
gg_b3_ampli <-
  tabela_2 %>% 
  filter(parametro == "b3") %>% 
  ggplot(aes(x = amostra_categoria, y = amplitude , color = modelo)) +
  geom_line(aes(group = modelo), linewidth = 0.65) +
  geom_point(size = 1.1) +
  labs(
    title = TeX("$\\beta_{3}$: Estado inicial"),
    x = "Tamanho amostral (n)",
    y = "Valor",
    color = "Modelos"
  ) +
  theme_minimal() +
  theme(
    panel.background =  element_rect(color = "grey70"),
    panel.border = element_rect(color = "grey70", fill = NA, size = 0.5),
    axis.text.x   = element_text(color = "grey20",size = 7),
    axis.text.y   = element_text(color = "grey20",size = 7),
    axis.title.x  = element_text(color = "grey20",face = "bold", size = 8),
    axis.title.y  = element_text(color = "grey20",face = "bold", size = 8),
    title =  element_text(color = "grey10",face = "bold", size = 14),
    legend.title = element_text(size = 8),
    legend.text = element_text(color = "grey20", size = 7),
    panel.grid.minor.x = element_blank()
  ) +
  scale_color_manual(values = c(
    'Log-binomial frequentista' = "#D55E00",
    'Poisson robusto' = "#0072B2",
    'Log-binomial Bayesiano (Quantis)' = "#FF0000",
    'Log-binomial Bayesiano (HPD)' = "#AA4499"
  )) +
  scale_x_discrete(
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  ylim(-0.1, 1.25)

# gg_b3_media
# gg_b3_sd
# gg_b3_vies
# gg_b3_reqm
# gg_b3_prob
# gg_b3_ampli


# saidas ------------------------------------------------------------------

linha1 <- 
  wrap_plots(
    gg_b0_media, 
    gg_b1_media, 
    gg_b2_media, 
    gg_b3_media,
    nrow = 1,
    guides = "collect"
  )

linha2 <- wrap_plots(
  gg_b0_vies,
  gg_b1_vies,
  gg_b2_vies,
  gg_b3_vies,
  nrow = 1,
  guides = "collect"
)

linha3 <-
  wrap_plots(
    gg_b0_sd,
    gg_b1_sd,
    gg_b2_sd,
    gg_b3_sd,
    nrow = 1,
    guides = "collect"
  )


linha4 <- wrap_plots(
  gg_b0_reqm,
  gg_b1_reqm,
  gg_b2_reqm,
  gg_b3_reqm,
  nrow = 1,
  guides = "collect"
)

linha5 <- wrap_plots(
  gg_b0_prob,
  gg_b1_prob,
  gg_b2_prob,
  gg_b3_prob,
  nrow = 1,
  guides = "collect"
)

linha6 <- wrap_plots(
  gg_b0_ampli,
  gg_b1_ampli,
  gg_b2_ampli,
  gg_b3_ampli,
  nrow = 1,
  guides = "collect"
)

placa <- function(texto) {
  ggplot() +
    annotate(
      "rect",
      xmin = -0.05, xmax = 1.05, # <-- Estica o retângulo para além de 0 e 1
      ymin = 0, ymax = 1,
      fill = "grey30",
      color = "grey20",
      linewidth = 0.5
    ) +
    annotate(
      "text",
      x = 0.5, y = 0.5,
      label = texto,
      color = "white",
      fontface = "bold",
      size = 6
    ) +
    # coord_cartesian com clip = "off" permite que o gráfico vase os limites dos eixos
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), clip = "off") +
    theme_void() +
    # Adicionamos uma margem lateral (esquerda e direita) para garantir 
    # que o retângulo expandido não seja cortado na borda do PDF
    theme(plot.margin = margin(t = 0, r = 15, b = 0, l = 15, unit = "pt"))
}

placa1 <- placa("Média")
placa2 <- placa("Viés")
placa3 <- placa("Desvio-padrão")
placa4 <- placa("REQM")
placa5 <- placa("Probabilidade de cobertura")
placa6 <- placa("Amplitude média")

painel_final <- 
  placa1 /
  linha1 /
  plot_spacer() /
  
  placa2 /
  linha2 /
  plot_spacer() /
  
  placa3 /
  linha3 /
  plot_spacer() /
  
  placa4 /
  linha4 /
  plot_spacer() /
  
  placa5 /
  linha5 /
  plot_spacer() /
  
  placa6 /
  linha6 +
  
  plot_layout(
    heights = c(
      0.25, 1, 0.15,
      0.25, 1, 0.15,
      0.25, 1, 0.15,
      0.25, 1, 0.15,
      0.25, 1, 0.15,
      0.25, 1
    )
  )

painel_final
ggsave(
  "plots/3-plots-simulacao/plot_simulacao_2/c_plot_6.pdf",
  painel_final,
  width = 18,
  height = 24
  )

