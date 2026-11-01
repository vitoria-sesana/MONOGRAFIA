# tabela resultados das estimativas dos modelos sob diferentes chutes iniciais

# rotina ------------------------------------------------------------------
source("scripts/0-rotina.R", encoding = "UTF-8")

# Leitura -----------------------------------------------------------------

chutes_logbin_freq <- 
  "saidas/1-saida-aplicacao/chutes_iniciais_log_binomial_frequentista_estimativas.rds" %>% 
  readRDS() %>% 
  mutate(
    across(where(is.numeric), ~ round(.x, 4))
  ) %>% 
  mutate(
    across(
      where(is.numeric),
      ~ sprintf("%.4f", .x)
    )
  ) %>% 
  mutate(
    modelo = "Log-binomial frequentista" 
  )  %>% 
  select(
    modelo,
    Chute_inicial,
    `(Intercept)`,
    center1,
    treatP,
    baseline0
  ) %>% 
  janitor::clean_names()

chutes_pois_robusto <- 
  "saidas/1-saida-aplicacao/chutes_iniciais_poisson_robusto_estimativas.rds" %>% 
  readRDS() %>%  
  mutate(
    across(where(is.numeric), ~ round(.x, 4))
  ) %>% 
  bind_rows(
    tibble::tibble(
      Chute_inicial = "chute10",
      !!!setNames(
        as.list(rep(NA_real_, ncol(.) - 1)),
        setdiff(names(.), "Chute_inicial")
      )
    )
  ) %>% 
  mutate(
    across(
      where(is.numeric),
      ~ sprintf("%.4f", .x)
    )
  ) %>% 
  mutate(
    modelo = "Poisson robusto"
  ) %>% 
  select(
    modelo,
    Chute_inicial,
    `(Intercept)`,
    center1,
    treatP,
    baseline0
  ) %>% 
  janitor::clean_names()

chutes_logbin_bayes <- 
  "saidas/1-saida-aplicacao/chutes_iniciais_log_binomial_bayesiano_estimativas.rds" %>% 
  readRDS() %>% 
  mutate(
    Chute_inicial = paste0("chute", chute),
    modelo = 
      case_when(
        estatistica == "Media" ~ "Log-binomial bayesiano (média)",
        estatistica == "Mediana" ~ "Log-binomial bayesiano (mediana)",
        TRUE ~ estatistica
        )
    ) %>% 
  mutate(
    across(where(is.numeric), ~ round(.x, 4))
  ) %>% 
  mutate(
    across(
      where(is.numeric),
      ~ sprintf("%.4f", .x)
    )
  ) %>% 
  rename(
    `(Intercept)` = b0,
    center1 = b1,
    treatP = b2,
    baseline0 = b3
  ) %>% 
  select(
    modelo,
    Chute_inicial,
    `(Intercept)`,
    center1,
    treatP,
    baseline0
  ) %>% 
  janitor::clean_names()


# tabela com todas as estimativas -----------------------------------------

tabela_consolidada_chutes_iniciais <-
  bind_rows(
    chutes_logbin_freq,
    chutes_pois_robusto,
    chutes_logbin_bayes
  ) %>% 
  pivot_longer(
    cols = c(intercept, center1, treat_p, baseline0),
    names_to = "coeficiente",
    values_to = "valor"
  ) %>%
  pivot_wider(
    names_from = chute_inicial,
    values_from = valor
  ) 

# Saída -------------------------------------------------------------------

kbl(
  tabela_consolidada_chutes_iniciais,
  format = "latex",
  booktabs = TRUE,
  caption = "Tabela de resultados",
  label = "tab:resultados",
  align = "lcccccccc"
)


# Gráficos ----------------------------------------------------------------

tabela_grafico <- 
  tabela_consolidada_chutes_iniciais %>% 
  pivot_longer(
    cols = starts_with("chute"),
    names_to = "chute",
    values_to = "valor"
  ) %>%
  mutate(
    valor  = readr::parse_number(as.character(valor), locale = readr::locale(decimal_mark = ".")),
    modelo = factor(modelo, levels = c(
      "Log-binomial frequentista",
      "Poisson robusto",
      "Log-binomial bayesiano (média)",
      "Log-binomial bayesiano (mediana)"
    )),
    chute = factor(chute, levels = paste0("chute", 1:10))
  ) %>%
  drop_na(valor) %>%
  filter(!(modelo == "Poisson robusto" & chute == "chute10")) %>% 
  mutate(chute = stringr::str_replace(chute, "chute(\\d+)", "Chute \\1")) %>% 
  mutate(chute = factor(chute, levels = paste0("Chute ", 1:10)))



## intercepto -----------
gg_intercepto <- 
  tabela_grafico %>% 
  filter(coeficiente == "intercept") %>%
  ggplot(aes(x = chute, y = valor, color = modelo)) +
  geom_line(aes(group = modelo), linewidth = 0.65) +
  geom_point(size = 1.1) +
  labs(
    title = TeX("$\\beta_{0}$: Intercepto"),
    x = "Chutes iniciais",
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
    limits = c(-2.8, 1.1),
    breaks = seq(-2.8, 1.1, by = 0.5),
    labels = scales::label_number(accuracy = 0.1)
  )





## centro -----------
gg_centro <- 
  tabela_grafico %>% 
  filter(coeficiente == "center1") %>%
  ggplot(aes(x = chute, y = valor, color = modelo)) +
  geom_line(aes(group = modelo), linewidth = 0.65) +
  geom_point(size = 1.1) +
  labs(
    title = TeX("$\\beta_{1}$: Centro"),
    x = "Chutes iniciais",
    y = "Valor",
    color = "Modelos"
  ) +
  theme_minimal() +
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
    limits = c(-0.5, 4),
    breaks = seq(-0.5, 4, by = 0.5),
    labels = scales::label_number(accuracy = 0.1)
  )



## tratamento -----------
gg_tratamento <- 
  tabela_grafico %>% 
  filter(coeficiente == "treat_p") %>%
  ggplot(aes(x = chute, y = valor, color = modelo)) +
  geom_line(aes(group = modelo), linewidth = 0.65) +
  geom_point(size = 1.1) +
  labs(
    title = TeX("$\\beta_{2}$: Tratamento"),
    x = "Chutes iniciais",
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
    limits = c(-0.4, 5.7),
    breaks = seq(-0.4, 5.7, by = 1),
    labels = scales::label_number(accuracy = 0.1)
  )

## estado inicial -----------
gg_estado_inicial <- 
  tabela_grafico %>% 
  filter(coeficiente == "baseline0") %>%
  ggplot(aes(x = chute, y = valor, color = modelo)) +
  geom_line(aes(group = modelo), linewidth = 0.65) +
  geom_point(size = 1.1) +
  labs(
    title = TeX("$\\beta_{3}$: Estado inicial"),
    x = "Chutes iniciais",
    y = "Valor",
    color = "Modelos"
  ) +
  theme_minimal() +
  theme_minimal() +
  
  theme(
    panel.background = element_rect(color = "grey70"),
    panel.border = element_rect(color = "grey70", fill = NA, linewidth = 0.5),
    axis.text.x = element_text( angle = 45, vjust = +0.5, color = "grey20", size = 11),
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
    limits = c(-4.8, -0.4),
    breaks = seq(-4.8, -0.4, by = 1),
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
      label = "Chutes~iniciais",
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


# saída -------------------------------------------------------------------

ggsave(
  "plots/2-plots-aplicacao/chutes_iniciais_coeficientes.pdf",
  final,
  width = 16,
  height = 10
)
