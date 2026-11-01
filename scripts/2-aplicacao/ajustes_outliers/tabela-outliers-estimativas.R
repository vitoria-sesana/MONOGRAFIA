# tabela resultados das estimativas dos modelos sob inclusão e exclusão de outliers

# rotina ------------------------------------------------------------------
source("scripts/0-rotina.R", encoding = "UTF-8")

# Leitura -----------------------------------------------------------------

outliers_logbin_freq <- 
  "saidas/1-saida-aplicacao/outliers_log_binomial_frequentista_estimativas.rds" %>% 
  readRDS() 

outliers_pois_robusto <- 
  "saidas/1-saida-aplicacao/outliers_poisson_robusto_estimativas.rds" %>% 
  readRDS() 

outliers_logbin_bayes <- 
  "saidas/1-saida-aplicacao/outliers_log_binomial_bayesiano_estimativas.rds" %>% 
  readRDS() %>% 
  rename(modelo = estatistica) %>% 
  mutate(
    modelo = 
      case_when(
        modelo == "Media" ~ "Log-binomial bayesiano (média)",
        modelo == "Mediana" ~ "Log-binomial bayesiano (mediana)",
        TRUE ~ modelo
      )
  ) %>% 
  mutate(
    across(
      c(b0, b0_exp, b1, b1_exp, b2, b2_exp, b3, b3_exp),
      ~ round(as.numeric(.x), 4)
    )
  )


# tratamento  -------------------------------------------------------------

outliers_logbin_freq_tratado <- 
  outliers_logbin_freq %>% 
  select(modelo, Cenario, Coeficiente, Estimativa) %>% 
  mutate(
    Coeficiente = case_when(
      Coeficiente == "(Intercept)" ~ "b0",
      Coeficiente == "center1"     ~ "b1",
      Coeficiente == "treatP"      ~ "b2",
      Coeficiente == "baseline0"   ~ "b3",
      TRUE ~ Coeficiente
    ),
    Estimativa_exp = exp(Estimativa)
  ) %>% 
  pivot_wider(
    id_cols = c(Cenario, modelo),
    names_from = Coeficiente,
    values_from = c(Estimativa, Estimativa_exp),
    names_glue = "{Coeficiente}_{.value}"
  ) %>%
  rename_with(
    ~ gsub("_Estimativa", "", .x)
  ) %>%
  rename_with(
    ~ gsub("_Estimativa_exp", "_exp", .x)
  ) %>%
  select(
    cenario = Cenario,
    modelo,
    b0, b0_exp,
    b1, b1_exp,
    b2, b2_exp,
    b3, b3_exp
  ) %>% 
  mutate(
    across(
      c(b0, b0_exp, b1, b1_exp, b2, b2_exp, b3, b3_exp),
      ~ round(as.numeric(.x), 4)
    )
  ) 


outliers_pois_robusto_tratado <- 
  outliers_pois_robusto %>% 
  select(modelo, Cenario, Coeficiente, Estimativa) %>% 
  mutate(
    Coeficiente = case_when(
      Coeficiente == "(Intercept)" ~ "b0",
      Coeficiente == "center1"     ~ "b1",
      Coeficiente == "treatP"      ~ "b2",
      Coeficiente == "baseline0"   ~ "b3",
      TRUE ~ Coeficiente
    ),
    Estimativa_exp = exp(Estimativa)
  ) %>% 
  pivot_wider(
    id_cols = c(Cenario, modelo),
    names_from = Coeficiente,
    values_from = c(Estimativa, Estimativa_exp),
    names_glue = "{Coeficiente}_{.value}"
  ) %>%
  rename_with(
    ~ gsub("_Estimativa", "", .x)
  ) %>%
  rename_with(
    ~ gsub("_Estimativa_exp", "_exp", .x)
  ) %>%
  select(
    cenario = Cenario,
    modelo,
    b0, b0_exp,
    b1, b1_exp,
    b2, b2_exp,
    b3, b3_exp
  ) %>% 
  mutate(
    across(
      c(b0, b0_exp, b1, b1_exp, b2, b2_exp, b3, b3_exp),
      ~ round(as.numeric(.x), 4)
    )
  ) 

# tabela com todas as estimativas -----------------------------------------

tabela_consolidada_outliers <- 
  bind_rows(
    outliers_logbin_freq_tratado,
    outliers_pois_robusto_tratado,
    outliers_logbin_bayes
  ) %>% 
  mutate(
    cenario = factor(
      cenario,
      levels = c(
        "Dados completos",
        "Sem 81",
        "Sem 107",
        "Sem 81 e 107"
      )
    ),
    modelo = factor(
      modelo,
      levels = c(
        "Log-binomial frequentista",
        "Poisson robusto",
        "Log-binomial bayesiano (média)",
        "Log-binomial bayesiano (mediana)"
      )
    )
  ) %>% 
  arrange(cenario, modelo) 
  


# gráfico -----------------------------------------------------------------

## tabela grafico 

tabela_consolidada_outliers_long <- 
  tabela_consolidada_outliers %>%
  pivot_longer(
    cols = c(b0, b1, b2, b3),
    names_to = "parametro",
    values_to = "coeficiente"
  ) %>%
  pivot_longer(
    cols = c(b0_exp, b1_exp, b2_exp, b3_exp),
    names_to = "parametro_exp",
    values_to = "exp_coef"
  ) %>%
  filter(stringr::str_sub(parametro, 2) == stringr::str_sub(parametro_exp, 2, -5)) %>%
  select(cenario, modelo, parametro, coeficiente, exp_coef) 



## intercepto 
gg_intercepto <- 
  tabela_consolidada_outliers_long %>% 
  filter(parametro == "b0") %>%
  ggplot(aes(x = cenario, y = coeficiente, color = modelo)) +
  geom_line(aes(group = modelo), linewidth = 0.65) +
  geom_point(size = 1.1) +
  labs(
    title = TeX("$\\beta_{0}$: Intercepto"),
    x = "Cenário",
    y = "Valor",
    color = "Modelos"
  ) +
  theme_minimal() +
  
  theme(
    panel.background = element_rect(color = "grey70"),
    panel.border = element_rect(color = "grey70", fill = NA, linewidth = 0.5),
    axis.text.x = element_text(angle = 45, vjust = +0.5, color = "grey20", size = 11),
    axis.text.y = element_text(color = "grey20", size = 11),
    axis.title.x = element_text(
      color = "grey20",
      face = "bold",
      size = 13
    ),
    axis.title.y = element_text(color = "grey20", face = "bold", size = 13),
    legend.title = element_text(size = 11),
    legend.text = element_text(color = "grey20", size = 11),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  scale_color_manual(values = c(
    "Log-binomial frequentista"        = "#D55E00",
    "Poisson robusto"                  = "#0072B2",
    "Log-binomial bayesiano (média)"   = "#e7298a",
    "Log-binomial bayesiano (mediana)" = "#009E73"
  ))+ 
  scale_y_continuous(
    limits = c(-0.2, 1.4),
    breaks = seq(-0.2, 1.4, by = 0.1),
    labels = scales::label_number(accuracy = 0.1)
  ) 
  




## intercepto 
gg_centro <- 
  tabela_consolidada_outliers_long %>% 
  filter(parametro == "b1") %>%
  ggplot(aes(x = cenario, y = coeficiente, color = modelo)) +
  geom_line(aes(group = modelo), linewidth = 0.65) +
  geom_point(size = 1.1) +
  labs(
    title = TeX("$\\beta_{1}$: Centro"),
    x = "Cenário",
    y = "Valor",
    color = "Modelos"
  ) +
  theme_minimal() +
  
  theme(
    panel.background = element_rect(color = "grey70"),
    panel.border = element_rect(color = "grey70", fill = NA, linewidth = 0.5),
    axis.text.x = element_text(angle = 45, vjust = +0.5, color = "grey20", size = 11),
    axis.text.y = element_text(color = "grey20", size = 11),
    axis.title.x = element_text(
      color = "grey20",
      face = "bold",
      size = 13
    ),
    axis.title.y = element_text(color = "grey20", face = "bold", size = 13),
    legend.title = element_text(size = 11),
    legend.text = element_text(color = "grey20", size = 11),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  scale_color_manual(values = c(
    "Log-binomial frequentista"        = "#D55E00",
    "Poisson robusto"                  = "#0072B2",
    "Log-binomial bayesiano (média)"   = "#e7298a",
    "Log-binomial bayesiano (mediana)" = "#009E73"
  ))+ 
  scale_y_continuous(
    limits = c(-0.5, -0.1),
    breaks = seq(-0.5, -0.1, by = 0.1),
    labels = scales::label_number(accuracy = 0.1)
  ) 






## intercepto 
gg_tratamento <- 
  tabela_consolidada_outliers_long %>% 
  filter(parametro == "b2") %>%
  ggplot(aes(x = cenario, y = coeficiente, color = modelo)) +
  geom_line(aes(group = modelo), linewidth = 0.65) +
  geom_point(size = 1.1) +
  labs(
    title = TeX("$\\beta_{2}$: Tratamento"),
    x = "Cenário",
    y = "Valor",
    color = "Modelos"
  ) +
  theme_minimal() +
  
  theme(
    panel.background = element_rect(color = "grey70"),
    panel.border = element_rect(color = "grey70", fill = NA, linewidth = 0.5),
    axis.text.x = element_text(angle = 45, vjust = +0.5, color = "grey20", size = 11),
    axis.text.y = element_text(color = "grey20", size = 11),
    axis.title.x = element_text(
      color = "grey20",
      face = "bold",
      size = 13
    ),
    axis.title.y = element_text(color = "grey20", face = "bold", size = 13),
    legend.title = element_text(size = 11),
    legend.text = element_text(color = "grey20", size = 11),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  scale_color_manual(values = c(
    "Log-binomial frequentista"        = "#D55E00",
    "Poisson robusto"                  = "#0072B2",
    "Log-binomial bayesiano (média)"   = "#e7298a",
    "Log-binomial bayesiano (mediana)" = "#009E73"
  ))+ 
  scale_y_continuous(
    limits = c(-0.5, -0.1),
    breaks = seq(-0.5, -0.1, by = 0.1),
    labels = scales::label_number(accuracy = 0.1)
  ) 






## estado inicial 
gg_estado_inicial <- 
  tabela_consolidada_outliers_long %>% 
  filter(parametro == "b3") %>%
  ggplot(aes(x = cenario, y = coeficiente, color = modelo)) +
  geom_line(aes(group = modelo), linewidth = 0.65) +
  geom_point(size = 1.1) +
  labs(
    title = TeX("$\\beta_{3}$: Estado inicial"),
    x = "Cenário",
    y = "Valor",
    color = "Modelos"
  ) +
  theme_minimal() +
  
  theme(
    panel.background = element_rect(color = "grey70"),
    panel.border = element_rect(color = "grey70", fill = NA, linewidth = 0.5),
    axis.text.x = element_text(angle = 45, vjust = +0.5, color = "grey20", size = 11),
    axis.text.y = element_text(color = "grey20", size = 11),
    axis.title.x = element_text(
      color = "grey20",
      face = "bold",
      size = 13
    ),
    axis.title.y = element_text(color = "grey20", face = "bold", size = 13),
    legend.title = element_text(size = 11),
    legend.text = element_text(color = "grey20", size = 11),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  scale_color_manual(values = c(
    "Log-binomial frequentista"        = "#D55E00",
    "Poisson robusto"                  = "#0072B2",
    "Log-binomial bayesiano (média)"   = "#e7298a",
    "Log-binomial bayesiano (mediana)" = "#009E73"
  ))+ 
  scale_y_continuous(
    limits = c(-0.8, -0.5),
    breaks = seq(-0.8, -0.5, by = 0.1),
    labels = scales::label_number(accuracy = 0.1)
  ) 


gg_intercepto
gg_centro
gg_tratamento
gg_estado_inicial

# x -----------------------------------------------------------------------

tam_fonte <- 13

criar_rodape <- function() {
  ggplot() +
    annotate(
      "text",
      x = 0.5, y = 0.5,
      label = "Cenário",
      parse = TRUE,
      size = tam_fonte / .pt
    ) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
    theme_void()
}

rodape <- criar_rodape()

plots <- list(
  gg_intercepto, gg_centro,
  gg_tratamento, gg_estado_inicial
)

matriz <- wrap_plots(
  plots,
  ncol    = 2,
  nrow    = 2,
  widths  = c(1, 1),
  heights = c(1, 1)
) &
  labs(x = NULL)

final <- (matriz / rodape) +
  plot_layout(
    heights = c(1, 0.04),
    guides  = "collect"
  ) &
  theme(
    legend.position = "bottom",
    legend.box = "horizontal"
  )

final


# Apêndice ----------------------------------------------------------------

tabela_apendice_freqs <- 
  bind_rows(
    outliers_logbin_freq,
    outliers_pois_robusto
  )  %>%
  select(Cenario, modelo, Coeficiente, everything()) %>% 
  arrange(Cenario, modelo) %>% 
  mutate(
    across(everything(), ~ ifelse(is.na(.x), "-", as.character(.x)))
  ) 


# saída -------------------------------------------------------------------

kbl(
  tabela_consolidada_outliers,
  format = "latex",
  booktabs = TRUE,
  caption = "Tabela de resultados",
  label = "tab:resultados",
  align = "lcccccccc"
)


kbl(
  tabela_apendice_freqs,
  format = "latex",
  booktabs = TRUE,
  caption = "Tabela de resultados",
  label = "tab:resultados",
  align = "lcccccccc"
)

ggsave(
  "plots/2-plots-aplicacao/outliers_coeficientes.pdf",
  final,
  width = 16,
  height = 10
)



# k -----------------------------------------------------------------------


outliers_logbin_bayes_posterior <- 
  "saidas/1-saida-aplicacao/outliers_log_binomial_bayesiano_posterior.rds" %>% 
  readRDS()


outliers_logbin_bayes_posterior
