# rotina ------------------------------------------------------------------
source("scripts/0-rotina.R", encoding = "UTF-8")


# leitura -----------------------------------------------------------------
coefs <- read.csv("saidas/3-saida-simulacao-completo/C-saidas/C-resultados/C-media_sd.csv")
vies <- read.csv("saidas/3-saida-simulacao-completo/C-saidas/C-resultados/C-vies_reqm.csv")
cob_amp <- read.csv("saidas/3-saida-simulacao-completo/C-saidas/C-resultados/C-pc_am.csv")

# erro monte carlo --------------------------------------------------------

prob_c <- 0.95
M <- 1000

ep_mc <- sqrt(prob_c * (1 - prob_c) / M)

margem_mc <- 1.96 * ep_mc

limite_inferior_ep_mc <- prob_c - margem_mc
limite_superior_ep_mc <- prob_c + margem_mc

c(
  EP_MC = ep_mc,
  Margem = margem_mc,
  Inferior = limite_inferior_ep_mc,
  Superior = limite_superior_ep_mc
)

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
      #title = TeX("$\\beta_{0}$: Intercepto"),
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
    scale_y_continuous(
      limits = c(-0.7, 0),
      breaks = seq(-0.7, 0, by = 0.1),
      labels = scales::label_number(accuracy = 0.1)
    )

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
      #title = TeX("$\\beta_{0}$: Intercepto"),
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
  scale_y_continuous(
    limits = c(-0.3, 0.1),
    breaks = seq(-0.3, 0.1, by = 0.1),
    labels = scales::label_number(accuracy = 0.1)
  )

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
      #title = TeX("$\\beta_{0}$: Intercepto"),
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
  scale_y_continuous(
    limits = c(0, 0.4),
    breaks = seq(0, 0.4, by = 0.1),
    labels = scales::label_number(accuracy = 0.1)
  )


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
    #title = TeX("$\\beta_{0}$: Intercepto"),
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
  scale_y_continuous(
    limits = c(0, 0.4),
    breaks = seq(0, 0.4, by = 0.1),
    labels = scales::label_number(accuracy = 0.1)
  )



## prob -----
gg_b0_prob <-
  tabela_2 %>% 
  filter(parametro == "b0") %>% 
  ggplot(aes(x = amostra_categoria, y = prob_c , color = modelo)) +
  geom_line(aes(group = modelo), linewidth = 0.65) +
  geom_point(size = 1.1) +
  geom_hline(yintercept = 0.95, 
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
    #title = TeX("$\\beta_{0}$: Intercepto"),
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
  scale_y_continuous(
    limits = c(0.80, 1),
    breaks = seq(0.80, 1, by = 0.1),
    labels = scales::label_number(accuracy = 0.1)
  )
  # annotate(
  #   "rect",
  #   xmin = -Inf,
  #   xmax = Inf,
  #   ymin = limite_inferior_ep_mc,
  #   ymax = limite_superior_ep_mc,
  #   fill = "grey60",
  #   alpha = 0.25
  # ) 




## amplitude ---------
gg_b0_ampli <-
  tabela_2 %>% 
  filter(parametro == "b0") %>% 
  ggplot(aes(x = amostra_categoria, y = amplitude , color = modelo)) +
  geom_line(aes(group = modelo), linewidth = 0.65) +
  geom_point(size = 1.1) +
  labs(
    #title = TeX("$\\beta_{0}$: Intercepto"),
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
  scale_y_continuous(
    limits = c(0, 1.25),
    breaks = seq(0, 1.25, by = 0.1),
    labels = scales::label_number(accuracy = 0.1)
  )

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
    #title = TeX("$\\beta_{1}$: Centro"),
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
  scale_y_continuous(
    limits = c(-0.7, 0),
    breaks = seq(-0.7, 0, by = 0.1),
    labels = scales::label_number(accuracy = 0.1)
  )

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
    #title = TeX("$\\beta_{1}$: Centro"),
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
  scale_y_continuous(
    limits = c(-0.3, 0.1),
    breaks = seq(-0.3, 0.1, by = 0.1),
    labels = scales::label_number(accuracy = 0.1)
  )

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
    #title = TeX("$\\beta_{1}$: Centro"),
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
  scale_y_continuous(
    limits = c(0, 0.4),
    breaks = seq(0, 0.4, by = 0.1),
    labels = scales::label_number(accuracy = 0.1)
  )


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
    #title = TeX("$\\beta_{1}$: Centro"),
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
  scale_y_continuous(
    limits = c(0, 0.4),
    breaks = seq(0, 0.4, by = 0.1),
    labels = scales::label_number(accuracy = 0.1)
  )

## prob -----
gg_b1_prob <-
  tabela_2 %>% 
  filter(parametro == "b1") %>% 
  ggplot(aes(x = amostra_categoria, y = prob_c , color = modelo)) +
  geom_line(aes(group = modelo), linewidth = 0.65) +
  geom_point(size = 1.1) +
  geom_hline(yintercept = 0.95, 
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
    #title = TeX("$\\beta_{1}$: Centro"),
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
  scale_y_continuous(
    limits = c(0.80, 1),
    breaks = seq(0.80, 1, by = 0.1),
    labels = scales::label_number(accuracy = 0.1)
  )
  # annotate(
  #   "rect",
  #   xmin = -Inf,
  #   xmax = Inf,
  #   ymin = limite_inferior_ep_mc,
  #   ymax = limite_superior_ep_mc,
  #   fill = "grey60",
  #   alpha = 0.25
  # ) 




## amplitude ---------
gg_b1_ampli <-
  tabela_2 %>% 
  filter(parametro == "b1") %>% 
  ggplot(aes(x = amostra_categoria, y = amplitude , color = modelo)) +
  geom_line(aes(group = modelo), linewidth = 0.65) +
  geom_point(size = 1.1) +
  labs(
    #title = TeX("$\\beta_{1}$: Centro"),
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
  scale_y_continuous(
    limits = c(0, 1.25),
    breaks = seq(0, 1.25, by = 0.1),
    labels = scales::label_number(accuracy = 0.1)
  )


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
    #title = TeX("$\\beta_{2}$: Tratamento"),
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
  scale_y_continuous(
    limits = c(-0.7, 0),
    breaks = seq(-0.7, 0, by = 0.1),
    labels = scales::label_number(accuracy = 0.1)
  )

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
    #title = TeX("$\\beta_{2}$: Tratamento"),
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
  scale_y_continuous(
    limits = c(-0.3, 0.1),
    breaks = seq(-0.3, 0.1, by = 0.1),
    labels = scales::label_number(accuracy = 0.1)
  )


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
    #title = TeX("$\\beta_{2}$: Tratamento"),
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
  scale_y_continuous(
    limits = c(0, 0.4),
    breaks = seq(0, 0.4, by = 0.1),
    labels = scales::label_number(accuracy = 0.1)
  )


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
    #title = TeX("$\\beta_{2}$: Tratamento"),
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
  scale_y_continuous(
    limits = c(0, 0.4),
    breaks = seq(0, 0.4, by = 0.1),
    labels = scales::label_number(accuracy = 0.1)
  )


## prob -----
gg_b2_prob <-
  tabela_2 %>% 
  filter(parametro == "b2") %>% 
  ggplot(aes(x = amostra_categoria, y = prob_c , color = modelo)) +
  geom_line(aes(group = modelo), linewidth = 0.65) +
  geom_point(size = 1.1) +
  geom_hline(yintercept = 0.95, 
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
    #title = TeX("$\\beta_{2}$: Tratamento"),
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
  scale_y_continuous(
    limits = c(0.80, 1),
    breaks = seq(0.80, 1, by = 0.1),
    labels = scales::label_number(accuracy = 0.1)
  )
  # annotate(
  #   "rect",
  #   xmin = -Inf,
  #   xmax = Inf,
  #   ymin = limite_inferior_ep_mc,
  #   ymax = limite_superior_ep_mc,
  #   fill = "grey60",
  #   alpha = 0.25
  # ) 



## amplitude ---------
gg_b2_ampli <-
  tabela_2 %>% 
  filter(parametro == "b2") %>% 
  ggplot(aes(x = amostra_categoria, y = amplitude , color = modelo)) +
  geom_line(aes(group = modelo), linewidth = 0.65) +
  geom_point(size = 1.1) +
  labs(
    #title = TeX("$\\beta_{2}$: Tratamento"),
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
  scale_y_continuous(
    limits = c(0, 1.25),
    breaks = seq(0, 1.25, by = 0.1),
    labels = scales::label_number(accuracy = 0.1)
  )



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
    #title = TeX("$\\beta_{3}$: Estado inicial"),
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
  scale_y_continuous(
    limits = c(-0.7, 0),
    breaks = seq(-0.7, 0, by = 0.1),
    labels = scales::label_number(accuracy = 0.1)
  )

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
    #title = TeX("$\\beta_{3}$: Estado inicial"),
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
  scale_y_continuous(
    limits = c(-0.3, 0.1),
    breaks = seq(-0.3, 0.1, by = 0.1),
    labels = scales::label_number(accuracy = 0.1)
  )


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
    #title = TeX("$\\beta_{3}$: Estado inicial"),
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
  scale_y_continuous(
    limits = c(0, 0.4),
    breaks = seq(0, 0.4, by = 0.1),
    labels = scales::label_number(accuracy = 0.1)
  )


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
    #title = TeX("$\\beta_{3}$: Estado inicial"),
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
  scale_y_continuous(
    limits = c(0, 0.4),
    breaks = seq(0, 0.4, by = 0.1),
    labels = scales::label_number(accuracy = 0.1)
  )




## prob -----
gg_b3_prob <-
  tabela_2 %>% 
  filter(parametro == "b3") %>% 
  ggplot(aes(x = amostra_categoria, y = prob_c , color = modelo)) +
  geom_line(aes(group = modelo), linewidth = 0.65) +
  geom_point(size = 1.1) +
  geom_hline(yintercept = 0.95,  
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
    #title = TeX("$\\beta_{3}$: Estado inicial"),
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
  scale_y_continuous(
    limits = c(0.80, 1),
    breaks = seq(0.80, 1, by = 0.1),
    labels = scales::label_number(accuracy = 0.1)
  )
  # annotate(
  #   "rect",
  #   xmin = -Inf,
  #   xmax = Inf,
  #   ymin = limite_inferior_ep_mc,
  #   ymax = limite_superior_ep_mc,
  #   fill = "grey60",
  #   alpha = 0.25
  # ) 




## amplitude ---------
gg_b3_ampli <-
  tabela_2 %>% 
  filter(parametro == "b3") %>% 
  ggplot(aes(x = amostra_categoria, y = amplitude , color = modelo)) +
  geom_line(aes(group = modelo), linewidth = 0.65) +
  geom_point(size = 1.1) +
  labs(
    #title = TeX("$\\beta_{3}$: Estado inicial"),
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
  scale_y_continuous(
    limits = c(0, 1.25),
    breaks = seq(0, 1.25, by = 0.1),
    labels = scales::label_number(accuracy = 0.1)
  )

# gg_b3_media
# gg_b3_sd
# gg_b3_vies
# gg_b3_reqm
# gg_b3_prob
# gg_b3_ampli


# Gráficos finais ---------------------------------------------------------

library(ggplot2)
library(patchwork)
library(latex2exp)

placa_coluna <- function(texto) {
  ggplot() +
    annotate(
      "rect",
      xmin = -0.05, xmax = 1.05,
      ymin = 0, ymax = 1,
      fill = "#e8e8e8",
      color = "grey80",
      linewidth = 0.5
    ) +
    annotate(
      "text",
      x = 0.5, y = 0.5,
      label = texto,
      color = "grey20",
      fontface = "bold",
      size = 5
    ) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), clip = "off") +
    theme_void() +
    theme(plot.margin = margin(t = 0, r = 10, b = 0, l = 10, unit = "pt"))
}

placa_linha <- function(texto) {
  ggplot() +
    annotate(
      "rect",
      xmin = 0, xmax = 1,
      ymin = -0.05, ymax = 1.05,
      fill = "#e8e8e8",
      color = "grey80",
      linewidth = 0.5
    ) +
    annotate(
      "text",
      x = 0.5, y = 0.5,
      label = texto,
      color = "grey20",
      fontface = "plain",
      size = 4.5,
      angle = -90
    ) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), clip = "off") +
    theme_void() +
    theme(plot.margin = margin(t = 10, r = 2, b = 10, l = 2, unit = "pt"))
}

placa_coluna_tex <- function(texto) {
  ggplot() +
    annotate(
      "rect",
      xmin = -0.05, xmax = 1.05,
      ymin = 0, ymax = 1,
      fill = "#e8e8e8",
      color = "grey80",
      linewidth = 0.5
    ) +
    annotate(
      "text",
      x = 0.5, y = 0.5,
      label = TeX(texto, output = "character"),
      parse = TRUE,
      color = "grey20",
      size = 5
    ) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), clip = "off") +
    theme_void() +
    theme(plot.margin = margin(t = 0, r = 10, b = 0, l = 10, unit = "pt"))
}

tam_fonte <- 11

criar_rodape <- function() {
  ggplot() +
    annotate(
      "text",
      x = 0.5, y = 0.5,
      label = "Tamanho~amostral~(italic(n))",
      parse = TRUE,
      size = tam_fonte / .pt
    ) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
    theme_void()
}

col_b0 <- placa_coluna_tex("$\\beta_{0}$: Intercepto")
col_b1 <- placa_coluna_tex("$\\beta_{1}$: Centro")
col_b2 <- placa_coluna_tex("$\\beta_{2}$: Tratamento")
col_b3 <- placa_coluna_tex("$\\beta_{3}$: Estado inicial")

rodape <- criar_rodape()

## Plot 1: média, viés, desvio-padrão e REQM ----
row_media <- placa_linha("Média")
row_vies  <- placa_linha("Viés")
row_sd    <- placa_linha("Desvio-padrão")
row_reqm  <- placa_linha("REQM")

plots_1 <- list(
  col_b0,      col_b1,      col_b2,      col_b3,      plot_spacer(),
  gg_b0_media, gg_b1_media, gg_b2_media, gg_b3_media, row_media,
  gg_b0_vies,  gg_b1_vies,  gg_b2_vies,  gg_b3_vies,  row_vies,
  gg_b0_sd,    gg_b1_sd,    gg_b2_sd,    gg_b3_sd,    row_sd,
  gg_b0_reqm,  gg_b1_reqm,  gg_b2_reqm,  gg_b3_reqm,  row_reqm
)

matriz_1 <- wrap_plots(
  plots_1,
  ncol    = 5,
  nrow    = 5,
  widths  = c(1, 1, 1, 1, 0.10),
  heights = c(0.10, 1, 1, 1, 1)
) &
  labs(x = NULL, y = NULL)

final_1 <- (matriz_1 / rodape) +
  plot_layout(
    heights = c(1, 0.02),
    guides  = "collect"
  ) &
  theme(
    legend.position = "bottom",
    legend.box = "horizontal"
  )

## Plot 2: probabilidade de cobertura e amplitude média --------

row_probabilidade <- placa_linha("Probabilidade de cobertura")
row_amplitude     <- placa_linha("Amplitude média")

plots_2 <- list(
  col_b0,      col_b1,      col_b2,      col_b3,      plot_spacer(),
  gg_b0_prob,  gg_b1_prob,  gg_b2_prob,  gg_b3_prob,  row_probabilidade,
  gg_b0_ampli, gg_b1_ampli, gg_b2_ampli, gg_b3_ampli, row_amplitude
)

matriz_2 <- wrap_plots(
  plots_2,
  ncol    = 5,
  nrow    = 3,
  widths  = c(1, 1, 1, 1, 0.10),
  heights = c(0.10, 1, 1)
) &
  labs(x = NULL, y = NULL)

final_2 <- (matriz_2 / rodape) +
  plot_layout(
    heights = c(1, 0.04),
    guides  = "collect"
  ) &
  theme(
    legend.position = "bottom",
    legend.box = "horizontal"
  )



# saída -------------------------------------------------------------------

ggsave(
  "plots/3-plots-simulacao/plot_simulacao_2/c_mvdr.pdf",
  final_1,
  width = 20,
  height = 24
)


ggsave(
  "plots/3-plots-simulacao/plot_simulacao_2/c_pa.pdf",
  final_2,
  width  = 18,
  height = 12.5
)
