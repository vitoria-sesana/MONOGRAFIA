# rotina ------------------------------------------------------------------
source("scripts/0-rotina.R", encoding = "UTF-8")


# função auxiliar ---------------------------------------------------------

formatar_tempo_S <- function(x) {
  trunc(as.numeric(x))
}

formatar_tempo_HMS <- function(x) {
  segundos <- as.numeric(x)
  
  sprintf(
    "%02d:%02d:%02d",
    floor(segundos / 3600),
    floor((segundos %% 3600) / 60),
    floor(segundos %% 60)
  )
}


# Cenário C ---------------------------------------------------------------

## leitura -----------------------------------------------------------------
C_tempo_logbin_freq <- 
  "saidas/3-saida-simulacao-completo/C-saidas/C-M1-logbin-frequentista/C-tempo_execucao_logbin_frequentista_tamanho_amostral.rds" %>% 
  readRDS() %>% 
  mutate(
    modelo = "Log-binomial frequentista"
  ) %>% 
  select(modelo, amostra, tempo_total)


C_tempo_poisson_robusto <- 
  "saidas/3-saida-simulacao-completo/C-saidas/C-M2-poisson-robusto/C-tempo_execucao_ajustes_poisson_robusto_tamanho_amostral.rds" %>% 
  readRDS()  %>% 
  mutate(
    modelo = "Poisson robusto",
    tempo_total = soma_tempo_total
  ) %>% 
  select(modelo, amostra, tempo_total)


C_tempo_logbin_bayes <- 
  "saidas/3-saida-simulacao-completo/C-saidas/C-M3-logbin-bayesiano/C-tempo_execucao_ajustes_logbin_bayesiano_tamanho_amostral.rds" %>% 
  readRDS()  %>% 
  mutate(
    modelo = "Log-binomial bayesiano"
  ) %>% 
  select(modelo, amostra, tempo_total)

## tabela ------------------------------------------------------------------

C_tabela_tempo <- 
  rbind(
    C_tempo_logbin_freq,
    C_tempo_poisson_robusto,
    C_tempo_logbin_bayes
  ) %>% 
  mutate(
    cenario = "Cenário 1 - Converge",
    valor_S = formatar_tempo_S(tempo_total),
    valor_HMS = formatar_tempo_HMS(valor_S)
  )

C_tabela_tempo

# Cenário NC --------------------------------------------------------------

## leitura -----------------------------------------------------------------
# NC_tempo_logbin_freq <- 
#   "saidas/3-saida-simulacao-completo/NC-saidas/NC-M1-logbin-frequentista/NC-tempo_execucao_logbin_frequentista_tamanho_amostral.rds" %>% 
#   readRDS() %>% 
#   mutate(
#     modelo = "Log-binomial frequentista"
#   ) %>% 
#   select(modelo, amostra, tempo_total)



NC_tempo_poisson_robusto <- 
  "saidas/3-saida-simulacao-completo/NC-saidas/NC-M2-poisson-robusto/NC-tempo_execucao_ajustes_poisson_robusto_tamanho_amostral.rds" %>% 
  readRDS() %>% 
  mutate(
    modelo = "Poisson robusto",
    tempo_total = soma_tempo_total
  ) %>% 
  select(modelo, amostra, tempo_total)



NC_tempo_logbin_bayes <- 
  "saidas/3-saida-simulacao-completo/NC-saidas/NC-M3-logbin-bayesiano/NC-tempo_execucao_ajustes_logbin_bayesiano_tamanho_amostral.rds" %>% 
  readRDS() %>% 
  mutate(
    modelo = "Log-binomial bayesiano"
  ) %>% 
  select(modelo, amostra, tempo_total)



## tabela ------------------------------------------------------------------

NC_tabela_tempo <- 
  rbind(
    # NC_tempo_logbin_freq,
    NC_tempo_poisson_robusto,
    NC_tempo_logbin_bayes
  ) %>% 
  mutate(
    cenario = "Cenário 2 - Não converge",
    valor_S = formatar_tempo_S(tempo_total),
    valor_HMS = formatar_tempo_HMS(valor_S)
  )

C_tabela_tempo
NC_tabela_tempo

# Tabela final ------------------------------------------------------------

tabela_execucao <- 
  bind_rows(
    C_tabela_tempo, 
    NC_tabela_tempo
  ) %>% 
  mutate(
    cenario = factor(
      cenario,
      levels = c(
        #"Geral", 
        "Cenário 1 - Converge", 
        "Cenário 2 - Não converge"
      )
    ),
    amostra = factor(
      amostra, 
      levels = c(
        50, 
        100,
        200
      )
    ),
    modelo = factor(
      modelo,
      levels = c(
        "Log-binomial frequentista",
        "Poisson robusto",
        "Log-binomial bayesiano"
      )
    )
  )  %>% 
  select(
    modelo, cenario, amostra, valor_S, valor_HMS
  ) %>% 
  arrange(modelo, cenario, amostra) %>% 
  mutate(
    cenario = factor(
      cenario,
      levels = c(
        #"Geral", 
        "Cenário 1 - Converge", 
        "Cenário 2 - Não converge"
      )
    ),
    amostra = factor(
      amostra, 
      levels = c(
        50, 
        100,
        200
      )
    ),
    modelo = factor(
      modelo,
      levels = c(
        "Log-binomial frequentista",
        "Poisson robusto",
        "Log-binomial bayesiano"
      )
    )
  )

tabela_execucao


# Gráfico -----------------------------------------------------------------

## Cenário 1 ------------------
grafico_tempo_execucao_1 <-
  tabela_execucao %>% 
  filter(cenario == "Cenário 1 - Converge") %>%
  ggplot(aes(
    x = amostra,
    y = valor_S,
    color = modelo,
    group = modelo
  )) +
  
  geom_line(linewidth = 0.9) +
  
  geom_point(size = 3) +
  
  geom_text(
    aes(
      label = paste0(valor_HMS, " (", valor_S, "s)"),
      vjust = case_when(
        modelo == "Log-binomial frequentista" ~ -2,
        modelo == "Poisson robusto" ~ -0.9,
        modelo == "Log-binomial bayesiano" ~ -1.2,
        TRUE ~ -0.9
      ),
      hjust = case_when(
        modelo == "Log-binomial frequentista" ~ -0.3,
        modelo == "Poisson robusto" ~ -0.3,
        modelo == "Log-binomial bayesiano" ~ 0.9,
        TRUE ~ -0.3
      )
    ),
    size = 4,
    show.legend = FALSE
  ) +
  
  labs(
    x = expression(bold("Tamanho amostral (") * bolditalic(n) * bold(")")),
    y = "Tempo (segundos)",
    color = "Modelo"
  ) +
  
  theme_minimal() +
  
  theme(
    panel.background = element_rect(color = "grey70"),
    panel.border = element_rect(color = "grey70", fill = NA, linewidth = 0.5),
    axis.text.x = element_text(color = "grey20", size = 9),
    axis.text.y = element_text(color = "grey20", size = 9),
    axis.title.x = element_text(
      color = "grey20",
      face = "bold",
      size = 11
    ),
    axis.title.y = element_text(color = "grey20", face = "bold", size = 11),
    legend.title = element_text(size = 11),
    legend.text = element_text(color = "grey20", size = 9),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  
  scale_y_continuous(
    limits = c(0, 6000),
    breaks = scales::pretty_breaks(n = 16),
    expand = expansion(mult = c(0, 0.15))
  ) +
  
  scale_color_manual(
    values = c(
      "Log-binomial frequentista" = "#D55E00",
      "Poisson robusto" = "#0072B2",
      "Log-binomial bayesiano" = "#009E73"
    )
  )


grafico_tempo_execucao_1


## Cenário 2 ------------------
grafico_tempo_execucao_2 <-
  tabela_execucao %>% 
  filter(cenario == "Cenário 2 - Não converge") %>%
  ggplot(aes(
    x = amostra,
    y = valor_S,
    color = modelo,
    group = modelo
  )) +
  
  geom_line(linewidth = 0.9) +
  
  geom_point(size = 3) +
  
  geom_text(
    aes(
      label = paste0(valor_HMS, " (", valor_S, "s)"),
      vjust = case_when(
        modelo == "Log-binomial frequentista" ~ -2,
        modelo == "Poisson robusto" ~ -0.9,
        modelo == "Log-binomial bayesiano" ~ -1.2,
        TRUE ~ -0.9
      ),
      hjust = case_when(
        modelo == "Log-binomial frequentista" ~ -0.3,
        modelo == "Poisson robusto" ~ -0.3,
        modelo == "Log-binomial bayesiano" ~ 0.9,
        TRUE ~ -0.3
      )
    ),
    size = 4,
    show.legend = FALSE
  ) +
  
  labs(
    x = expression(bold("Tamanho amostral (") * bolditalic(n) * bold(")")),
    y = "Tempo (segundos)",
    color = "Modelo"
  ) +
  
  theme_minimal() +
  
  theme(
    panel.background = element_rect(color = "grey70"),
    panel.border = element_rect(color = "grey70", fill = NA, linewidth = 0.5),
    axis.text.x = element_text(color = "grey20", size = 9),
    axis.text.y = element_text(color = "grey20", size = 9),
    axis.title.x = element_text(
      color = "grey20",
      face = "bold",
      size = 11
    ),
    axis.title.y = element_text(color = "grey20", face = "bold", size = 11),
    legend.title = element_text(size = 11),
    legend.text = element_text(color = "grey20", size = 9),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  
  scale_y_continuous(
    limits = c(0, 6000),
    breaks = scales::pretty_breaks(n = 16),
    expand = expansion(mult = c(0, 0.15))
  ) +
  
  scale_color_manual(
    values = c(
      "Log-binomial frequentista" = "#D55E00",
      "Poisson robusto" = "#0072B2",
      "Log-binomial bayesiano" = "#009E73"
    )
  )

# grafico_tempo_execucao_1
grafico_tempo_execucao_2

# saída latex -------------------------------------------------------------

kbl(
  tabela_execucao,
  format = "latex",
  booktabs = TRUE,
  align = "rrll",
  caption = "Tabela tempo de execução dos modelos em cada cenário."
) %>%
  kable_styling(
    latex_options = c("hold_position"),
    stripe_color = "gray!15"
  ) %>%
  row_spec(1:3, background = "gray!15") %>%
  row_spec(4:6, background = "white") %>%
  row_spec(7:9, background = "gray!15")

# Saída gráfico -----------------------------------------------------------

ggsave(
  filename = "plots/3-plots-simulacao/tempos/tempo_execucao_modelos_cenario1.pdf",
  plot = grafico_tempo_execucao_1,
  device = cairo_pdf,
  width = 28,
  height = 14,
  units = "cm"
)


ggsave(
  filename = "plots/3-plots-simulacao/tempos/tempo_execucao_modelos_cenario2.pdf",
  plot = grafico_tempo_execucao_2,
  device = cairo_pdf,
  width = 28,
  height = 14,
  units = "cm"
)
