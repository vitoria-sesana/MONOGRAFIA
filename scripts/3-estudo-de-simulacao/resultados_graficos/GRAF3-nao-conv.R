rm(list = ls())

require(geepack)
require(dplyr)
require(tidyr)
require(ggplot2)
require(latex2exp)
require(ggtext)

# leitura dos resultados --------------------------------------------------

media_sd <- 
  read.csv("saidas/2-saida-simulacao/R4-nao-covergidas/nao_conv_media_sd.csv") %>% 
  select(-X)

vies_reqm <- 
  read.csv("saidas/2-saida-simulacao/R4-nao-covergidas/nao_conv_media_sd.csv") %>% 
  select(-X)

pc_am <- 
  read.csv("saidas/2-saida-simulacao/R4-nao-covergidas/nao_conv_vies_reqm.csv") %>% 
  select(-X)

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


# b0 ----------------------------------------------------------------------

## média -----------

b0_media <-  
  media_sd %>% 
  filter(parametro == "b0") %>% 
  select(amostra_categoria, parametro, media_lgb, media_pois, media_lgb_bayes_media, media_lgb_bayes_mediana) %>% 
  pivot_longer(
    cols = -c(amostra_categoria, parametro),
    names_to = "variavel",
    values_to = "valor"
  ) %>% 
  mutate(
    variavel = stringr::str_replace_all(variavel, "media_", ""),
    variavel = factor(
      variavel,
      levels = c(
        "lgb",
        "pois",
        "lgb_bayes_media",
        "lgb_bayes_mediana"
      )
    )
  )

gg_b0_media <-
  b0_media %>% 
  ggplot(aes(x = amostra_categoria, y = valor, color = variavel)) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 1) +
  labs(
    title = TeX("Média $\\beta_{0}$"),
    x = "Tamanho amostral (n)",
    y = "Valor",
    color = "Modelos"
  ) +
  theme_minimal() +
  theme(
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
  scale_x_continuous(breaks = c(50, 100, 200, 500)) +
  scale_color_manual(values = c(
    lgb = "#1b9e77",
    pois = "#d95f02",
    lgb_bayes_media = "#7570b3",
    lgb_bayes_mediana = "#e7298a"
  ),
  labels = c(
    lgb = "Log-bin",
    pois = "Pois-robusto",
    lgb_bayes_media = "Log-bin Bayesiano (Média)",
    lgb_bayes_mediana = "Log-bin Bayesiano (Mediana)"
  )
  ) 

## viés -----------

b0_vies <-  
  vies_reqm %>% 
  filter(parametro == "b0") %>% 
  select(
    amostra_categoria, 
    parametro,
    vies_lgb,
    vies_pois,
    vies_bayes_media,
    vies_bayes_mediana
  ) %>% 
  pivot_longer(
    cols = -c(amostra_categoria, parametro),
    names_to = "variavel",
    values_to = "valor"
  ) %>% 
  mutate(
    variavel = stringr::str_replace_all(variavel, "vies_", ""),
    variavel = ifelse(variavel == "bayes_media", "lgb_bayes_media", variavel),
    variavel = ifelse(variavel == "bayes_mediana", "lgb_bayes_mediana", variavel),
    variavel = factor(
      variavel,
      levels = c(
        "lgb",
        "pois",
        "lgb_bayes_media",
        "lgb_bayes_mediana"
      )
    )
  )

gg_b0_vies <- 
  b0_vies %>% 
  ggplot(aes(x = amostra_categoria, y = valor, color = variavel)) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 1) +
  labs(
    title = TeX("Viés $\\beta_{0}$"),
    x = "Tamanho amostral (n)",
    y = "Valor",
    color = "Modelos"
  ) +
  theme_minimal() +
  theme(
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
  scale_x_continuous(breaks = c(50, 100, 200, 500)) +
  scale_color_manual(values = c(
    lgb = "#1b9e77",
    pois = "#d95f02",
    lgb_bayes_media = "#7570b3",
    lgb_bayes_mediana = "#e7298a"
  ),
  labels = c(
    lgb = "Log-bin",
    pois = "Pois-robusto",
    lgb_bayes_media = "Log-bin Bayesiano (Média)",
    lgb_bayes_mediana = "Log-bin Bayesiano (Mediana)"
  )
  ) 


## reqm -----------

b0_reqm <-  
  vies_reqm %>% 
  filter(parametro == "b0") %>% 
  select(
    amostra_categoria, 
    parametro,
    RMSE_lgb,
    RMSE_pois,
    RMSE_bayes_media,
    RMSE_bayes_mediana
  ) %>% 
  pivot_longer(
    cols = -c(amostra_categoria, parametro),
    names_to = "variavel",
    values_to = "valor"
  ) %>% 
  mutate(
    variavel = stringr::str_replace_all(variavel, "RMSE_", ""),
    variavel = ifelse(variavel == "bayes_media", "lgb_bayes_media", variavel),
    variavel = ifelse(variavel == "bayes_mediana", "lgb_bayes_mediana", variavel),
    variavel = factor(
      variavel,
      levels = c(
        "lgb",
        "pois",
        "lgb_bayes_media",
        "lgb_bayes_mediana"
      )
    )
  )

gg_b0_reqm <- 
  b0_reqm %>% 
  ggplot(aes(x = amostra_categoria, y = valor, color = variavel)) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 1) +
  labs(
    title = TeX("REQM $\\beta_{0}$"),
    x = "Tamanho amostral (n)",
    y = "Valor",
    color = "Modelos"
  ) +
  theme_minimal() +
  theme(
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
  scale_x_continuous(breaks = c(50, 100, 200, 500)) +
  scale_color_manual(values = c(
    lgb = "#1b9e77",
    pois = "#d95f02",
    lgb_bayes_media = "#7570b3",
    lgb_bayes_mediana = "#e7298a"
  ),
  labels = c(
    lgb = "Log-bin",
    pois = "Pois-robusto",
    lgb_bayes_media = "Log-bin Bayesiano (Média)",
    lgb_bayes_mediana = "Log-bin Bayesiano (Mediana)"
  )
  ) 

## probabilidade cobertura -----------

b0_pc <-  
  pc_am %>% 
  filter(parametro == "b0") %>% 
  rename(
    prob_c_quantilica = probabilidade_cobertura_quantilica,
    prob_c_HPD = probabilidade_cobertura_HPD
  )  %>% 
  select(
    amostra_categoria, 
    parametro,
    prob_c_freq,
    prob_c_poiss,
    prob_c_quantilica,
    prob_c_HPD
  ) %>% 
  pivot_longer(
    cols = -c(amostra_categoria, parametro),
    names_to = "variavel",
    values_to = "valor"
  ) %>%
  mutate(
    variavel = stringr::str_replace_all(variavel, "prob_c_", ""),
    variavel = ifelse(variavel == "poiss", "pois", variavel),
    variavel = ifelse(variavel == "freq", "lgb", variavel),
    variavel = factor(
      variavel,
      levels = c(
        "lgb",
        "pois",
        "quantilica",
        "HPD"
      )
    )
  )


gg_b0_pc <- 
  b0_pc %>% 
  ggplot(aes(x = amostra_categoria, y = valor, color = variavel)) +
  geom_line(linewidth = 0.1) +
  geom_hline(yintercept = 1, 
             linetype = "longdash", 
             color = "grey20", 
             linewidth = 0.4) +
  geom_point(size = 1) +
  labs(
    title = TeX("Probabilidade de cobertura $\\beta_{0}$"),
    x = "Tamanho amostral (n)",
    y = "Valor",
    color = "Modelos"
  ) +
  theme_minimal() +
  theme(
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
  scale_x_continuous(breaks = c(50, 100, 200, 500)) +
  scale_color_manual(values = c(
    lgb = "#1b9e77",
    pois = "#d95f02",
    quantilica = "#7570b3",
    HPD = "#e7298a"
  ),
  labels = c(
    lgb = "Log-bin",
    pois = "Pois-robusto",
    quantilica = "Log-bin Bayesiano (quantílico)",
    HPD = "Log-bin Bayesiano (HPD)"
  )
  ) +
  ylim(0,1)


## amplitude média -----------

b0_am <-  
  pc_am %>% 
  filter(parametro == "b0") %>% 
  mutate(
    amplitude_quantilica = amplitude_bayes_quantilica,
    amplitude_HPD = amplitude_bayes_HPD
  ) %>% 
  select(
    amostra_categoria, 
    parametro,
    amplitude_lgb_freq,
    amplitude_pois,
    amplitude_quantilica,
    amplitude_HPD
  ) %>% 
  pivot_longer(
    cols = -c(amostra_categoria, parametro),
    names_to = "variavel",
    values_to = "valor"
  ) %>% 
  mutate(
    variavel = stringr::str_replace_all(variavel, "amplitude_", ""),
    variavel = ifelse(variavel == "lgb_freq", "lgb", variavel),
    variavel = factor(
      variavel,
      levels = c(
        "lgb",
        "pois",
        "quantilica",
        "HPD"
      )
    )
  )

gg_b0_am <- 
  b0_am %>% 
  ggplot(aes(x = amostra_categoria, y = valor, color = variavel)) +
  geom_line(linewidth = 0.1) +
  geom_point(size = 1) +
  labs(
    title = TeX("Amplitude média $\\beta_{0}$"),
    x = "Tamanho amostral (n)",
    y = "Valor",
    color = "Modelos"
  ) +
  theme_minimal() +
  theme(
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
  scale_x_continuous(breaks = c(50, 100, 200, 500)) +
  scale_color_manual(values = c(
    lgb = "#1b9e77",
    pois = "#d95f02",
    quantilica = "#7570b3",
    HPD = "#e7298a"
  ),
  labels = c(
    lgb = "Log-bin",
    pois = "Pois-robusto",
    quantilica = "Log-bin Bayesiano (quantílico)",
    HPD = "Log-bin Bayesiano (HPD)"
  )
  ) 

## saídas b0 --------------------

linha1_b0 <- (gg_b0_media + gg_b0_vies + gg_b0_reqm) +
  plot_layout(guides = "collect", widths = rep(1, 3))

linha2_b0 <- (gg_b0_pc + gg_b0_am) +
  plot_layout(guides = "collect", widths = rep(1, 2))

painel_final_b0 <- linha1_b0 / 
  plot_spacer() / 
  linha2_b0 +
  plot_layout(heights = c(1, 0.2, 1))

ggsave(
  "saidas/2-saida-simulacao/R4-nao-covergidas/Naoconv-grafico_simulacao_b0.pdf",
  painel_final_b0,
  width = 12, 
  height = 8) 


# b1 ----------------------------------------------------------------------

## média -----------

b1_media <-  
  media_sd %>% 
  filter(parametro == "b1") %>% 
  select(amostra_categoria, parametro, media_lgb, media_pois, media_lgb_bayes_media, media_lgb_bayes_mediana) %>% 
  pivot_longer(
    cols = -c(amostra_categoria, parametro),
    names_to = "variavel",
    values_to = "valor"
  ) %>% 
  mutate(
    variavel = stringr::str_replace_all(variavel, "media_", ""),
    variavel = factor(
      variavel,
      levels = c(
        "lgb",
        "pois",
        "lgb_bayes_media",
        "lgb_bayes_mediana"
      )
    )
  )

gg_b1_media <-
  b1_media %>% 
  ggplot(aes(x = amostra_categoria, y = valor, color = variavel)) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 1) +
  labs(
    title = TeX("Média $\\beta_{1}$"),
    x = "Tamanho amostral (n)",
    y = "Valor",
    color = "Modelos"
  ) +
  theme_minimal() +
  theme(
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
  scale_x_continuous(breaks = c(50, 100, 200, 500)) +
  scale_color_manual(values = c(
    lgb = "#1b9e77",
    pois = "#d95f02",
    lgb_bayes_media = "#7570b3",
    lgb_bayes_mediana = "#e7298a"
  ),
  labels = c(
    lgb = "Log-bin",
    pois = "Pois-robusto",
    lgb_bayes_media = "Log-bin Bayesiano (Média)",
    lgb_bayes_mediana = "Log-bin Bayesiano (Mediana)"
  )
  ) 

## viés -----------

b1_vies <-  
  vies_reqm %>% 
  filter(parametro == "b1") %>% 
  select(
    amostra_categoria, 
    parametro,
    vies_lgb,
    vies_pois,
    vies_bayes_media,
    vies_bayes_mediana
  ) %>% 
  pivot_longer(
    cols = -c(amostra_categoria, parametro),
    names_to = "variavel",
    values_to = "valor"
  ) %>% 
  mutate(
    variavel = stringr::str_replace_all(variavel, "vies_", ""),
    variavel = ifelse(variavel == "bayes_media", "lgb_bayes_media", variavel),
    variavel = ifelse(variavel == "bayes_mediana", "lgb_bayes_mediana", variavel),
    variavel = factor(
      variavel,
      levels = c(
        "lgb",
        "pois",
        "lgb_bayes_media",
        "lgb_bayes_mediana"
      )
    )
  )

gg_b1_vies <- 
  b1_vies %>% 
  ggplot(aes(x = amostra_categoria, y = valor, color = variavel)) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 1) +
  labs(
    title = TeX("Viés $\\beta_{1}$"),
    x = "Tamanho amostral (n)",
    y = "Valor",
    color = "Modelos"
  ) +
  theme_minimal() +
  theme(
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
  scale_x_continuous(breaks = c(50, 100, 200, 500)) +
  scale_color_manual(values = c(
    lgb = "#1b9e77",
    pois = "#d95f02",
    lgb_bayes_media = "#7570b3",
    lgb_bayes_mediana = "#e7298a"
  ),
  labels = c(
    lgb = "Log-bin",
    pois = "Pois-robusto",
    lgb_bayes_media = "Log-bin Bayesiano (Média)",
    lgb_bayes_mediana = "Log-bin Bayesiano (Mediana)"
  )
  ) 


## reqm -----------

b1_reqm <-  
  vies_reqm %>% 
  filter(parametro == "b1") %>% 
  select(
    amostra_categoria, 
    parametro,
    RMSE_lgb,
    RMSE_pois,
    RMSE_bayes_media,
    RMSE_bayes_mediana
  ) %>% 
  pivot_longer(
    cols = -c(amostra_categoria, parametro),
    names_to = "variavel",
    values_to = "valor"
  ) %>% 
  mutate(
    variavel = stringr::str_replace_all(variavel, "RMSE_", ""),
    variavel = ifelse(variavel == "bayes_media", "lgb_bayes_media", variavel),
    variavel = ifelse(variavel == "bayes_mediana", "lgb_bayes_mediana", variavel),
    variavel = factor(
      variavel,
      levels = c(
        "lgb",
        "pois",
        "lgb_bayes_media",
        "lgb_bayes_mediana"
      )
    )
  )

gg_b1_reqm <- 
  b1_reqm %>% 
  ggplot(aes(x = amostra_categoria, y = valor, color = variavel)) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 1) +
  labs(
    title = TeX("REQM $\\beta_{1}$"),
    x = "Tamanho amostral (n)",
    y = "Valor",
    color = "Modelos"
  ) +
  theme_minimal() +
  theme(
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
  scale_x_continuous(breaks = c(50, 100, 200, 500)) +
  scale_color_manual(values = c(
    lgb = "#1b9e77",
    pois = "#d95f02",
    lgb_bayes_media = "#7570b3",
    lgb_bayes_mediana = "#e7298a"
  ),
  labels = c(
    lgb = "Log-bin",
    pois = "Pois-robusto",
    lgb_bayes_media = "Log-bin Bayesiano (Média)",
    lgb_bayes_mediana = "Log-bin Bayesiano (Mediana)"
  )
  ) 

## probabilidade cobertura -----------

b1_pc <-  
  pc_am %>% 
  filter(parametro == "b1") %>% 
  rename(
    prob_c_quantilica = probabilidade_cobertura_quantilica,
    prob_c_HPD = probabilidade_cobertura_HPD
  )  %>% 
  select(
    amostra_categoria, 
    parametro,
    prob_c_freq,
    prob_c_poiss,
    prob_c_quantilica,
    prob_c_HPD
  ) %>% 
  pivot_longer(
    cols = -c(amostra_categoria, parametro),
    names_to = "variavel",
    values_to = "valor"
  ) %>%
  mutate(
    variavel = stringr::str_replace_all(variavel, "prob_c_", ""),
    variavel = ifelse(variavel == "poiss", "pois", variavel),
    variavel = ifelse(variavel == "freq", "lgb", variavel),
    variavel = factor(
      variavel,
      levels = c(
        "lgb",
        "pois",
        "quantilica",
        "HPD"
      )
    )
  )


gg_b1_pc <- 
  b1_pc %>% 
  ggplot(aes(x = amostra_categoria, y = valor, color = variavel)) +
  geom_line(linewidth = 0.1) +
  geom_hline(yintercept = 1, 
             linetype = "longdash", 
             color = "grey20", 
             linewidth = 0.4) +
  geom_point(size = 1) +
  labs(
    title = TeX("Probabilidade de cobertura $\\beta_{1}$"),
    x = "Tamanho amostral (n)",
    y = "Valor",
    color = "Modelos"
  ) +
  theme_minimal() +
  theme(
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
  scale_x_continuous(breaks = c(50, 100, 200, 500)) +
  scale_color_manual(values = c(
    lgb = "#1b9e77",
    pois = "#d95f02",
    quantilica = "#7570b3",
    HPD = "#e7298a"
  ),
  labels = c(
    lgb = "Log-bin",
    pois = "Pois-robusto",
    quantilica = "Log-bin Bayesiano (quantílico)",
    HPD = "Log-bin Bayesiano (HPD)"
  )
  ) +
  ylim(0,1)


## amplitude média -----------

b1_am <-  
  pc_am %>% 
  filter(parametro == "b1") %>% 
  mutate(
    amplitude_quantilica = amplitude_bayes_quantilica,
    amplitude_HPD = amplitude_bayes_HPD
  ) %>% 
  select(
    amostra_categoria, 
    parametro,
    amplitude_lgb_freq,
    amplitude_pois,
    amplitude_quantilica,
    amplitude_HPD
  ) %>% 
  pivot_longer(
    cols = -c(amostra_categoria, parametro),
    names_to = "variavel",
    values_to = "valor"
  ) %>% 
  mutate(
    variavel = stringr::str_replace_all(variavel, "amplitude_", ""),
    variavel = ifelse(variavel == "lgb_freq", "lgb", variavel),
    variavel = factor(
      variavel,
      levels = c(
        "lgb",
        "pois",
        "quantilica",
        "HPD"
      )
    )
  )

gg_b1_am <- 
  b1_am %>% 
  ggplot(aes(x = amostra_categoria, y = valor, color = variavel)) +
  geom_line(linewidth = 0.1) +
  geom_point(size = 1) +
  labs(
    title = TeX("Amplitude média $\\beta_{1}$"),
    x = "Tamanho amostral (n)",
    y = "Valor",
    color = "Modelos"
  ) +
  theme_minimal() +
  theme(
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
  scale_x_continuous(breaks = c(50, 100, 200, 500)) +
  scale_color_manual(values = c(
    lgb = "#1b9e77",
    pois = "#d95f02",
    quantilica = "#7570b3",
    HPD = "#e7298a"
  ),
  labels = c(
    lgb = "Log-bin",
    pois = "Pois-robusto",
    quantilica = "Log-bin Bayesiano (quantílico)",
    HPD = "Log-bin Bayesiano (HPD)"
  )
  ) 

## saídas b1 --------------------

linha1_b1 <- (gg_b1_media + gg_b1_vies + gg_b1_reqm) +
  plot_layout(guides = "collect", widths = rep(1, 3))

linha2_b1 <- (gg_b1_pc + gg_b1_am) +
  plot_layout(guides = "collect", widths = rep(1, 2))

painel_final_b1 <- linha1_b1 / 
  plot_spacer() / 
  linha2_b1 +
  plot_layout(heights = c(1, 0.2, 1))

ggsave(
  "saidas/2-saida-simulacao/R4-nao-covergidas/Naoconv-grafico_simulacao_b1.pdf",
  painel_final_b1,
  width = 12, 
  height = 8) 


# b2 ----------------------------------------------------------------------

## média -----------

b2_media <-  
  media_sd %>% 
  filter(parametro == "b2") %>% 
  select(amostra_categoria, parametro, media_lgb, media_pois, media_lgb_bayes_media, media_lgb_bayes_mediana) %>% 
  pivot_longer(
    cols = -c(amostra_categoria, parametro),
    names_to = "variavel",
    values_to = "valor"
  ) %>% 
  mutate(
    variavel = stringr::str_replace_all(variavel, "media_", ""),
    variavel = factor(
      variavel,
      levels = c(
        "lgb",
        "pois",
        "lgb_bayes_media",
        "lgb_bayes_mediana"
      )
    )
  )

gg_b2_media <-
  b2_media %>% 
  ggplot(aes(x = amostra_categoria, y = valor, color = variavel)) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 1) +
  labs(
    title = TeX("Média $\\beta_{2}$"),
    x = "Tamanho amostral (n)",
    y = "Valor",
    color = "Modelos"
  ) +
  theme_minimal() +
  theme(
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
  scale_x_continuous(breaks = c(50, 100, 200, 500)) +
  scale_color_manual(values = c(
    lgb = "#1b9e77",
    pois = "#d95f02",
    lgb_bayes_media = "#7570b3",
    lgb_bayes_mediana = "#e7298a"
  ),
  labels = c(
    lgb = "Log-bin",
    pois = "Pois-robusto",
    lgb_bayes_media = "Log-bin Bayesiano (Média)",
    lgb_bayes_mediana = "Log-bin Bayesiano (Mediana)"
  )
  ) 

## viés -----------

b2_vies <-  
  vies_reqm %>% 
  filter(parametro == "b2") %>% 
  select(
    amostra_categoria, 
    parametro,
    vies_lgb,
    vies_pois,
    vies_bayes_media,
    vies_bayes_mediana
  ) %>% 
  pivot_longer(
    cols = -c(amostra_categoria, parametro),
    names_to = "variavel",
    values_to = "valor"
  ) %>% 
  mutate(
    variavel = stringr::str_replace_all(variavel, "vies_", ""),
    variavel = ifelse(variavel == "bayes_media", "lgb_bayes_media", variavel),
    variavel = ifelse(variavel == "bayes_mediana", "lgb_bayes_mediana", variavel),
    variavel = factor(
      variavel,
      levels = c(
        "lgb",
        "pois",
        "lgb_bayes_media",
        "lgb_bayes_mediana"
      )
    )
  )

gg_b2_vies <- 
  b2_vies %>% 
  ggplot(aes(x = amostra_categoria, y = valor, color = variavel)) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 1) +
  labs(
    title = TeX("Viés $\\beta_{2}$"),
    x = "Tamanho amostral (n)",
    y = "Valor",
    color = "Modelos"
  ) +
  theme_minimal() +
  theme(
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
  scale_x_continuous(breaks = c(50, 100, 200, 500)) +
  scale_color_manual(values = c(
    lgb = "#1b9e77",
    pois = "#d95f02",
    lgb_bayes_media = "#7570b3",
    lgb_bayes_mediana = "#e7298a"
  ),
  labels = c(
    lgb = "Log-bin",
    pois = "Pois-robusto",
    lgb_bayes_media = "Log-bin Bayesiano (Média)",
    lgb_bayes_mediana = "Log-bin Bayesiano (Mediana)"
  )
  ) 


## reqm -----------

b2_reqm <-  
  vies_reqm %>% 
  filter(parametro == "b2") %>% 
  select(
    amostra_categoria, 
    parametro,
    RMSE_lgb,
    RMSE_pois,
    RMSE_bayes_media,
    RMSE_bayes_mediana
  ) %>% 
  pivot_longer(
    cols = -c(amostra_categoria, parametro),
    names_to = "variavel",
    values_to = "valor"
  ) %>% 
  mutate(
    variavel = stringr::str_replace_all(variavel, "RMSE_", ""),
    variavel = ifelse(variavel == "bayes_media", "lgb_bayes_media", variavel),
    variavel = ifelse(variavel == "bayes_mediana", "lgb_bayes_mediana", variavel),
    variavel = factor(
      variavel,
      levels = c(
        "lgb",
        "pois",
        "lgb_bayes_media",
        "lgb_bayes_mediana"
      )
    )
  )

gg_b2_reqm <- 
  b2_reqm %>% 
  ggplot(aes(x = amostra_categoria, y = valor, color = variavel)) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 1) +
  labs(
    title = TeX("REQM $\\beta_{2}$"),
    x = "Tamanho amostral (n)",
    y = "Valor",
    color = "Modelos"
  ) +
  theme_minimal() +
  theme(
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
  scale_x_continuous(breaks = c(50, 100, 200, 500)) +
  scale_color_manual(values = c(
    lgb = "#1b9e77",
    pois = "#d95f02",
    lgb_bayes_media = "#7570b3",
    lgb_bayes_mediana = "#e7298a"
  ),
  labels = c(
    lgb = "Log-bin",
    pois = "Pois-robusto",
    lgb_bayes_media = "Log-bin Bayesiano (Média)",
    lgb_bayes_mediana = "Log-bin Bayesiano (Mediana)"
  )
  ) 

## probabilidade cobertura -----------

b2_pc <-  
  pc_am %>% 
  filter(parametro == "b2") %>% 
  rename(
    prob_c_quantilica = probabilidade_cobertura_quantilica,
    prob_c_HPD = probabilidade_cobertura_HPD
  )  %>% 
  select(
    amostra_categoria, 
    parametro,
    prob_c_freq,
    prob_c_poiss,
    prob_c_quantilica,
    prob_c_HPD
  ) %>% 
  pivot_longer(
    cols = -c(amostra_categoria, parametro),
    names_to = "variavel",
    values_to = "valor"
  ) %>%
  mutate(
    variavel = stringr::str_replace_all(variavel, "prob_c_", ""),
    variavel = ifelse(variavel == "poiss", "pois", variavel),
    variavel = ifelse(variavel == "freq", "lgb", variavel),
    variavel = factor(
      variavel,
      levels = c(
        "lgb",
        "pois",
        "quantilica",
        "HPD"
      )
    )
  )


gg_b2_pc <- 
  b2_pc %>% 
  ggplot(aes(x = amostra_categoria, y = valor, color = variavel)) +
  geom_line(linewidth = 0.1) +
  geom_hline(yintercept = 1, 
             linetype = "longdash", 
             color = "grey20", 
             linewidth = 0.4) +
  geom_point(size = 1) +
  labs(
    title = TeX("Probabilidade de cobertura $\\beta_{2}$"),
    x = "Tamanho amostral (n)",
    y = "Valor",
    color = "Modelos"
  ) +
  theme_minimal() +
  theme(
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
  scale_x_continuous(breaks = c(50, 100, 200, 500)) +
  scale_color_manual(values = c(
    lgb = "#1b9e77",
    pois = "#d95f02",
    quantilica = "#7570b3",
    HPD = "#e7298a"
  ),
  labels = c(
    lgb = "Log-bin",
    pois = "Pois-robusto",
    quantilica = "Log-bin Bayesiano (quantílico)",
    HPD = "Log-bin Bayesiano (HPD)"
  )
  ) +
  ylim(0,1)


## amplitude média -----------

b2_am <-  
  pc_am %>% 
  filter(parametro == "b2") %>% 
  mutate(
    amplitude_quantilica = amplitude_bayes_quantilica,
    amplitude_HPD = amplitude_bayes_HPD
  ) %>% 
  select(
    amostra_categoria, 
    parametro,
    amplitude_lgb_freq,
    amplitude_pois,
    amplitude_quantilica,
    amplitude_HPD
  ) %>% 
  pivot_longer(
    cols = -c(amostra_categoria, parametro),
    names_to = "variavel",
    values_to = "valor"
  ) %>% 
  mutate(
    variavel = stringr::str_replace_all(variavel, "amplitude_", ""),
    variavel = ifelse(variavel == "lgb_freq", "lgb", variavel),
    variavel = factor(
      variavel,
      levels = c(
        "lgb",
        "pois",
        "quantilica",
        "HPD"
      )
    )
  )

gg_b2_am <- 
  b2_am %>% 
  ggplot(aes(x = amostra_categoria, y = valor, color = variavel)) +
  geom_line(linewidth = 0.1) +
  geom_point(size = 1) +
  labs(
    title = TeX("Amplitude média $\\beta_{2}$"),
    x = "Tamanho amostral (n)",
    y = "Valor",
    color = "Modelos"
  ) +
  theme_minimal() +
  theme(
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
  scale_x_continuous(breaks = c(50, 100, 200, 500)) +
  scale_color_manual(values = c(
    lgb = "#1b9e77",
    pois = "#d95f02",
    quantilica = "#7570b3",
    HPD = "#e7298a"
  ),
  labels = c(
    lgb = "Log-bin",
    pois = "Pois-robusto",
    quantilica = "Log-bin Bayesiano (quantílico)",
    HPD = "Log-bin Bayesiano (HPD)"
  )
  ) 

## saídas b2 --------------------

linha1_b2 <- (gg_b2_media + gg_b2_vies + gg_b2_reqm) +
  plot_layout(guides = "collect", widths = rep(1, 3))

linha2_b2 <- (gg_b2_pc + gg_b2_am) +
  plot_layout(guides = "collect", widths = rep(1, 2))

painel_final_b2 <- linha1_b2 / 
  plot_spacer() / 
  linha2_b2 +
  plot_layout(heights = c(1, 0.2, 1))

ggsave(
  "saidas/2-saida-simulacao/R4-nao-covergidas/Naoconv-grafico_simulacao_b2.pdf",
  painel_final_b2,
  width = 12, 
  height = 8) 



# b3 ----------------------------------------------------------------------

## média -----------

b3_media <-  
  media_sd %>% 
  filter(parametro == "b3") %>% 
  select(amostra_categoria, parametro, media_lgb, media_pois, media_lgb_bayes_media, media_lgb_bayes_mediana) %>% 
  pivot_longer(
    cols = -c(amostra_categoria, parametro),
    names_to = "variavel",
    values_to = "valor"
  ) %>% 
  mutate(
    variavel = stringr::str_replace_all(variavel, "media_", ""),
    variavel = factor(
      variavel,
      levels = c(
        "lgb",
        "pois",
        "lgb_bayes_media",
        "lgb_bayes_mediana"
      )
    )
  )

gg_b3_media <-
  b3_media %>% 
  ggplot(aes(x = amostra_categoria, y = valor, color = variavel)) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 1) +
  labs(
    title = TeX("Média $\\beta_{3}$"),
    x = "Tamanho amostral (n)",
    y = "Valor",
    color = "Modelos"
  ) +
  theme_minimal() +
  theme(
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
  scale_x_continuous(breaks = c(50, 100, 200, 500)) +
  scale_color_manual(values = c(
    lgb = "#1b9e77",
    pois = "#d95f02",
    lgb_bayes_media = "#7570b3",
    lgb_bayes_mediana = "#e7298a"
  ),
  labels = c(
    lgb = "Log-bin",
    pois = "Pois-robusto",
    lgb_bayes_media = "Log-bin Bayesiano (Média)",
    lgb_bayes_mediana = "Log-bin Bayesiano (Mediana)"
  )
  ) 

## viés -----------

b3_vies <-  
  vies_reqm %>% 
  filter(parametro == "b3") %>% 
  select(
    amostra_categoria, 
    parametro,
    vies_lgb,
    vies_pois,
    vies_bayes_media,
    vies_bayes_mediana
  ) %>% 
  pivot_longer(
    cols = -c(amostra_categoria, parametro),
    names_to = "variavel",
    values_to = "valor"
  ) %>% 
  mutate(
    variavel = stringr::str_replace_all(variavel, "vies_", ""),
    variavel = ifelse(variavel == "bayes_media", "lgb_bayes_media", variavel),
    variavel = ifelse(variavel == "bayes_mediana", "lgb_bayes_mediana", variavel),
    variavel = factor(
      variavel,
      levels = c(
        "lgb",
        "pois",
        "lgb_bayes_media",
        "lgb_bayes_mediana"
      )
    )
  )

gg_b3_vies <- 
  b3_vies %>% 
  ggplot(aes(x = amostra_categoria, y = valor, color = variavel)) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 1) +
  labs(
    title = TeX("Viés $\\beta_{3}$"),
    x = "Tamanho amostral (n)",
    y = "Valor",
    color = "Modelos"
  ) +
  theme_minimal() +
  theme(
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
  scale_x_continuous(breaks = c(50, 100, 200, 500)) +
  scale_color_manual(values = c(
    lgb = "#1b9e77",
    pois = "#d95f02",
    lgb_bayes_media = "#7570b3",
    lgb_bayes_mediana = "#e7298a"
  ),
  labels = c(
    lgb = "Log-bin",
    pois = "Pois-robusto",
    lgb_bayes_media = "Log-bin Bayesiano (Média)",
    lgb_bayes_mediana = "Log-bin Bayesiano (Mediana)"
  )
  ) 


## reqm -----------

b3_reqm <-  
  vies_reqm %>% 
  filter(parametro == "b3") %>% 
  select(
    amostra_categoria, 
    parametro,
    RMSE_lgb,
    RMSE_pois,
    RMSE_bayes_media,
    RMSE_bayes_mediana
  ) %>% 
  pivot_longer(
    cols = -c(amostra_categoria, parametro),
    names_to = "variavel",
    values_to = "valor"
  ) %>% 
  mutate(
    variavel = stringr::str_replace_all(variavel, "RMSE_", ""),
    variavel = ifelse(variavel == "bayes_media", "lgb_bayes_media", variavel),
    variavel = ifelse(variavel == "bayes_mediana", "lgb_bayes_mediana", variavel),
    variavel = factor(
      variavel,
      levels = c(
        "lgb",
        "pois",
        "lgb_bayes_media",
        "lgb_bayes_mediana"
      )
    )
  )

gg_b3_reqm <- 
  b3_reqm %>% 
  ggplot(aes(x = amostra_categoria, y = valor, color = variavel)) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 1) +
  labs(
    title = TeX("REQM $\\beta_{3}$"),
    x = "Tamanho amostral (n)",
    y = "Valor",
    color = "Modelos"
  ) +
  theme_minimal() +
  theme(
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
  scale_x_continuous(breaks = c(50, 100, 200, 500)) +
  scale_color_manual(values = c(
    lgb = "#1b9e77",
    pois = "#d95f02",
    lgb_bayes_media = "#7570b3",
    lgb_bayes_mediana = "#e7298a"
  ),
  labels = c(
    lgb = "Log-bin",
    pois = "Pois-robusto",
    lgb_bayes_media = "Log-bin Bayesiano (Média)",
    lgb_bayes_mediana = "Log-bin Bayesiano (Mediana)"
  )
  ) 

## probabilidade cobertura -----------

b3_pc <-  
  pc_am %>% 
  filter(parametro == "b3") %>% 
  rename(
    prob_c_quantilica = probabilidade_cobertura_quantilica,
    prob_c_HPD = probabilidade_cobertura_HPD
  )  %>% 
  select(
    amostra_categoria, 
    parametro,
    prob_c_freq,
    prob_c_poiss,
    prob_c_quantilica,
    prob_c_HPD
  ) %>% 
  pivot_longer(
    cols = -c(amostra_categoria, parametro),
    names_to = "variavel",
    values_to = "valor"
  ) %>%
  mutate(
    variavel = stringr::str_replace_all(variavel, "prob_c_", ""),
    variavel = ifelse(variavel == "poiss", "pois", variavel),
    variavel = ifelse(variavel == "freq", "lgb", variavel),
    variavel = factor(
      variavel,
      levels = c(
        "lgb",
        "pois",
        "quantilica",
        "HPD"
      )
    )
  )


gg_b3_pc <- 
  b3_pc %>% 
  ggplot(aes(x = amostra_categoria, y = valor, color = variavel)) +
  geom_line(linewidth = 0.1) +
  geom_hline(yintercept = 1, 
             linetype = "longdash", 
             color = "grey20", 
             linewidth = 0.4) +
  geom_point(size = 1) +
  labs(
    title = TeX("Probabilidade de cobertura $\\beta_{3}$"),
    x = "Tamanho amostral (n)",
    y = "Valor",
    color = "Modelos"
  ) +
  theme_minimal() +
  theme(
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
  scale_x_continuous(breaks = c(50, 100, 200, 500)) +
  scale_color_manual(values = c(
    lgb = "#1b9e77",
    pois = "#d95f02",
    quantilica = "#7570b3",
    HPD = "#e7298a"
  ),
  labels = c(
    lgb = "Log-bin",
    pois = "Pois-robusto",
    quantilica = "Log-bin Bayesiano (quantílico)",
    HPD = "Log-bin Bayesiano (HPD)"
  )
  ) +
  ylim(0,1)


## amplitude média -----------

b3_am <-  
  pc_am %>% 
  filter(parametro == "b3") %>% 
  mutate(
    amplitude_quantilica = amplitude_bayes_quantilica,
    amplitude_HPD = amplitude_bayes_HPD
  ) %>% 
  select(
    amostra_categoria, 
    parametro,
    amplitude_lgb_freq,
    amplitude_pois,
    amplitude_quantilica,
    amplitude_HPD
  ) %>% 
  pivot_longer(
    cols = -c(amostra_categoria, parametro),
    names_to = "variavel",
    values_to = "valor"
  ) %>% 
  mutate(
    variavel = stringr::str_replace_all(variavel, "amplitude_", ""),
    variavel = ifelse(variavel == "lgb_freq", "lgb", variavel),
    variavel = factor(
      variavel,
      levels = c(
        "lgb",
        "pois",
        "quantilica",
        "HPD"
      )
    )
  )

gg_b3_am <- 
  b3_am %>% 
  ggplot(aes(x = amostra_categoria, y = valor, color = variavel)) +
  geom_line(linewidth = 0.1) +
  geom_point(size = 1) +
  labs(
    title = TeX("Amplitude média $\\beta_{3}$"),
    x = "Tamanho amostral (n)",
    y = "Valor",
    color = "Modelos"
  ) +
  theme_minimal() +
  theme(
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
  scale_x_continuous(breaks = c(50, 100, 200, 500)) +
  scale_color_manual(values = c(
    lgb = "#1b9e77",
    pois = "#d95f02",
    quantilica = "#7570b3",
    HPD = "#e7298a"
  ),
  labels = c(
    lgb = "Log-bin",
    pois = "Pois-robusto",
    quantilica = "Log-bin Bayesiano (quantílico)",
    HPD = "Log-bin Bayesiano (HPD)"
  )
  ) 

## saídas b3 --------------------

linha1_b3 <- (gg_b3_media + gg_b3_vies + gg_b3_reqm) +
  plot_layout(guides = "collect", widths = rep(1, 3))

linha2_b3 <- (gg_b3_pc + gg_b3_am) +
  plot_layout(guides = "collect", widths = rep(1, 2))

painel_final_b3 <- linha1_b3 / 
  plot_spacer() / 
  linha2_b3 +
  plot_layout(heights = c(1, 0.2, 1))

ggsave(
  "saidas/2-saida-simulacao/R4-nao-covergidas/Naoconv-grafico_simulacao_b3.pdf",
  painel_final_b3,
  width = 12, 
  height = 8) 




# rascunho: graficos de mesmo tamanho -----------------------------------------------------------
# 
# painel_final <- (gg_b2_media + gg_b2_vies + gg_b2_reqm + gg_b2_pc + gg_b2_am) +
#   plot_layout(
#     design = "
#     AAA
#     BBB
#     "
#   )
# 
# todos_graficos <- list(gg_b2_media, gg_b2_vies, gg_b2_reqm, gg_b2_pc, gg_b2_am)
# 
# painel_final <- patchwork::wrap_plots(todos_graficos, ncol = 3, nrow = 2) +
#   plot_layout(guides = "collect")
# painel_final
