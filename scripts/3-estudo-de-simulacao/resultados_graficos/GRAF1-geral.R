rm(list = ls())

# leitura dos resultados --------------------------------------------------

media_sd <- 
  read.csv("saidas/2-saida-simulacao/R2-geral/geral_media_sd.csv") %>% 
  select(-X)

vies_reqm <- 
  read.csv("saidas/2-saida-simulacao/R2-geral/geral_vies_reqm.csv") %>% 
  select(-X)

pc_am <- 
  read.csv("saidas/2-saida-simulacao/R2-geral/geral_pc_am.csv") %>% 
  select(-X)


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
    title = "Média B0",
    x = "Tamanho amostral (n)",
    y = "Valor",
    color = "Modelos"
  ) +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "grey70", fill = NA, size = 0.5),
    axis.text.x   = element_text(color = "grey20",size = 7),
    axis.text.y   = element_text(color = "grey20",size = 7),
    axis.title.x  = element_text(color = "grey20",face = "bold"),
    axis.title.y  = element_text(color = "grey20",face = "bold"),
    panel.grid.minor.x = element_blank()
  ) +
  scale_x_continuous(breaks = c(50, 100, 200, 500)) +
  scale_color_manual(values = c(
    lgb = "#1b9e77",
    pois = "#d95f02",
    lgb_bayes_media = "#7570b3",
    lgb_bayes_mediana = "#e7298a"
  )) 

gg_b0_media

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
    title = "Vies B0",
    x = "Tamanho amostral (n)",
    y = "Valor",
    color = "Modelos"
  ) +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "grey70", fill = NA, size = 0.5),
    axis.text.x   = element_text(color = "grey20",size = 7),
    axis.text.y   = element_text(color = "grey20",size = 7),
    axis.title.x  = element_text(color = "grey20",face = "bold"),
    axis.title.y  = element_text(color = "grey20",face = "bold"),
    panel.grid.minor.x = element_blank()
  ) +
  scale_x_continuous(breaks = c(50, 100, 200, 500)) +
  scale_color_manual(values = c(
    lgb = "#1b9e77",
    pois = "#d95f02",
    lgb_bayes_media = "#7570b3",
    lgb_bayes_mediana = "#e7298a"
  )) 
  
gg_b0_vies


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
    title = "REQM B0",
    x = "Tamanho amostral (n)",
    y = "Valor",
    color = "Modelos"
  ) +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "grey70", fill = NA, size = 0.5),
    axis.text.x   = element_text(color = "grey20",size = 7),
    axis.text.y   = element_text(color = "grey20",size = 7),
    axis.title.x  = element_text(color = "grey20",face = "bold"),
    axis.title.y  = element_text(color = "grey20",face = "bold"),
    panel.grid.minor.x = element_blank()
  ) +
  scale_x_continuous(breaks = c(50, 100, 200, 500)) +
  scale_color_manual(values = c(
    lgb = "#1b9e77",
    pois = "#d95f02",
    lgb_bayes_media = "#7570b3",
    lgb_bayes_mediana = "#e7298a"
  )) 

gg_b0_reqm

## probabilidade cobertura -----------

b0_pc <-  
  pc_am %>% 
  filter(parametro == "b0") %>% 
  rename(
    prob_c_quantilica = probabilidade_cobertura_quantilica,
    prob_c_HPD = probabilidade_cobertura_HPD
  ) %>% 
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
  geom_line(linewidth = 0.5) +
  geom_point(size = 1) +
  labs(
    title = "PC B0",
    x = "Tamanho amostral (n)",
    y = "Valor",
    color = "Modelos"
  ) +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "grey70", fill = NA, size = 0.5),
    axis.text.x   = element_text(color = "grey20",size = 7),
    axis.text.y   = element_text(color = "grey20",size = 7),
    axis.title.x  = element_text(color = "grey20",face = "bold"),
    axis.title.y  = element_text(color = "grey20",face = "bold"),
    panel.grid.minor.x = element_blank()
  ) +
  scale_x_continuous(breaks = c(50, 100, 200, 500)) +
  scale_color_manual(values = c(
    lgb = "#1b9e77",
    pois = "#d95f02",
    quantilica = "#7570b3",
    HPD = "#e7298a"
  )) 

gg_b0_pc


## amplitude média -----------

b0_am <-  
  pc_am %>% 
  filter(parametro == "(Intercept)") %>% 
  select(
    amostra_categoria, 
    parametro,
    amplitude_lgb_freq,
    amplitude_pois,
    amplitude_media
  ) %>% 
  pivot_longer(
    cols = -c(amostra_categoria, parametro),
    names_to = "variavel",
    values_to = "valor"
  ) %>% 
  mutate(
    variavel = stringr::str_replace_all(variavel, "amplitude_", ""),
    variavel = ifelse(variavel == "media", "bayes", variavel),
    variavel = ifelse(variavel == "lgb_freq", "lgb", variavel),
    variavel = factor(
      variavel,
      levels = c(
        "lgb",
        "pois",
        "bayes"
      )
    )
  )

gg_b0_am <- 
  b0_am %>% 
  ggplot(aes(x = amostra_categoria, y = valor, color = variavel)) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 1) +
  labs(
    title = "AM B0",
    x = "Tamanho amostral (n)",
    y = "Valor",
    color = "Modelos"
  ) +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "grey70", fill = NA, size = 0.5),
    axis.text.x   = element_text(color = "grey20",size = 7),
    axis.text.y   = element_text(color = "grey20",size = 7),
    axis.title.x  = element_text(color = "grey20",face = "bold"),
    axis.title.y  = element_text(color = "grey20",face = "bold"),
    panel.grid.minor.x = element_blank()
  ) +
  scale_x_continuous(breaks = c(50, 100, 200, 500)) +
  scale_color_manual(values = c(
    lgb = "#1b9e77",
    pois = "#d95f02",
    quantilica = "#7570b3",
    HPD = "#e7298a"
  )) 

gg_b0_am

# saídas -------------------------------------------------------------------
library(patchwork)

grafico_b0_1 <- (gg_b0_media / gg_b0_vies / gg_b0_reqm) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

grafico_b0_1

grafico_b0_2 <- (gg_b0_pc / gg_b0_am) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

grafico_b0_2

ggsave(
  "saidas/2-saida-simulacao/R2-geral/graficos_empilhados.pdf",
  grafico_b0_1,
  width = 8,
  height = 12,
  dpi = 300
)

ggsave(
  "saidas/2-saida-simulacao/R2-geral/graficos_empilhados2.pdf",
  grafico_b0_2,
  width = 8,
  height = 12,
  dpi = 300
)
