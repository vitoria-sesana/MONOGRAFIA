# rotina ------------------------------------------------------------------
source("scripts/0-rotina.R", encoding = "UTF-8")


# função auxiliar ---------------------------------------------------------

formatar_tempo_S <- function(x) {
  segundos <- as.numeric(x, units = "secs")
  
  round(segundos, 0)
}

formatar_tempo_HMS <- function(x) {
  segundos <- as.numeric(x, units = "secs")
  
  sprintf(
    "%02d:%02d:%02d",
    floor(segundos / 3600),
    floor((segundos %% 3600) / 60),
    floor(segundos %% 60)
  )
}


# Cenário Geral -----------------------------------------------------------

## leitura -----------------------------------------------------------------
g_tempo_logbin_freq <- 
  "saidas/2-saida-simulacao/M1-logbin-frequentista/tempo_execucao_logbin_frequentista.rds" %>% 
  readRDS() 


g_tempo_poisson_robusto <- 
  "saidas/2-saida-simulacao/M2-poisson-robusto/tempo_execucao_ajustes_poisson_robusto.rds" %>% 
  readRDS() 


g_tempo_logbin_bayes <- 
  "saidas/2-saida-simulacao/M3-logbin-bayesiano/tempo_execucao_ajustes_logbin_bayesiano.rds" %>% 
  readRDS()


## tabela ------------------------------------------------------------------

g_tabela_tempo <- data.frame(
  cenario = rep("Geral", 3),
  modelo = c(
    "Log-binomial frequentista",
    "Poisson robusto",
    "Log-binomial bayesiano"
  ),
  valor_HMS = c(
    formatar_tempo_HMS(g_tempo_logbin_freq),
    formatar_tempo_HMS(g_tempo_poisson_robusto),
    formatar_tempo_HMS(g_tempo_logbin_bayes)
  ),
  valor_S = c(
    formatar_tempo_S(g_tempo_logbin_freq),
    formatar_tempo_S(g_tempo_poisson_robusto),
    formatar_tempo_S(g_tempo_logbin_bayes)
  )
)

g_tabela_tempo


# Cenário C ---------------------------------------------------------------

## leitura -----------------------------------------------------------------
C_tempo_logbin_freq <- 
  "saidas/3-saida-simulacao-completo/C-saidas/C-M1-logbin-frequentista/C-tempo_execucao_logbin_frequentista.rds" %>% 
  readRDS()   
  


C_tempo_poisson_robusto <- 
  "saidas/3-saida-simulacao-completo/C-saidas/C-M2-poisson-robusto/C-tempo_execucao_ajustes_poisson_robusto.rds" %>% 
  readRDS()  
  


C_tempo_logbin_bayes <- 
  "saidas/3-saida-simulacao-completo/C-saidas/C-M3-logbin-bayesiano/C-tempo_execucao_ajustes_logbin_bayesiano.rds" %>% 
  readRDS()  
  


## tabela ------------------------------------------------------------------

C_tabela_tempo <- data.frame(
  cenario = rep("Convergido", 3),
  modelo = c(
    "Log-binomial frequentista",
    "Poisson robusto",
    "Log-binomial bayesiano"
  ),
  valor_HMS = c(
    formatar_tempo_HMS(C_tempo_logbin_freq),
    formatar_tempo_HMS(C_tempo_poisson_robusto),
    formatar_tempo_HMS(C_tempo_logbin_bayes)
  ),
  valor_S = c(
    formatar_tempo_S(C_tempo_logbin_freq),
    formatar_tempo_S(C_tempo_poisson_robusto),
    formatar_tempo_S(C_tempo_logbin_bayes)
  )
)

C_tabela_tempo


# Cenário NC --------------------------------------------------------------

## leitura -----------------------------------------------------------------
NC_tempo_logbin_freq <- 
  "saidas/3-saida-simulacao-completo/NC-saidas/NC-M1-logbin-frequentista/NC-tempo_execucao_logbin_frequentista.rds" %>% 
  readRDS()  
  


NC_tempo_poisson_robusto <- 
  "saidas/3-saida-simulacao-completo/NC-saidas/NC-M2-poisson-robusto/NC-tempo_execucao_ajustes_poisson_robusto.rds" %>% 
  readRDS()  
  


NC_tempo_logbin_bayes <- 
  "saidas/3-saida-simulacao-completo/NC-saidas/NC-M3-logbin-bayesiano/NC-tempo_execucao_ajustes_logbin_bayesiano.rds" %>% 
  readRDS()  
  


## tabela ------------------------------------------------------------------

NC_tabela_tempo <- data.frame(
  cenario = rep("Não Convergido", 3),
  modelo = c(
    "Log-binomial frequentista",
    "Poisson robusto",
    "Log-binomial bayesiano"
  ),
  valor_HMS = c(
    formatar_tempo_HMS(NC_tempo_logbin_freq),
    formatar_tempo_HMS(NC_tempo_poisson_robusto),
    formatar_tempo_HMS(NC_tempo_logbin_bayes)
  ),
  valor_S = c(
    formatar_tempo_S(NC_tempo_logbin_freq),
    formatar_tempo_S(NC_tempo_poisson_robusto),
    formatar_tempo_S(NC_tempo_logbin_bayes)
  )
)

NC_tabela_tempo


# tabela final ------------------------------------------------------------

tabela_execucao <- bind_rows(g_tabela_tempo, C_tabela_tempo, NC_tabela_tempo)
tabela_execucao

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

# gráfico -----------------------------------------------------------------

tabela_execucao <- tabela_execucao %>%
  mutate(
    cenario = factor(
      cenario,
      levels = c("Geral", "Convergido", "Não Convergido")
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

# 3 cenários

grafico_tempo_execucao_3 <-
  tabela_execucao %>% 
  # filter(cenario != "Geral") %>%
  ggplot(aes(x = cenario, y = valor_S , fill = modelo)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(
    aes(label = paste0(valor_HMS, "\n ", "(", valor_S,"s)")),
    position = position_dodge(width = 0.8),
    vjust = -0.5,
    size = 3.5
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) + # Espaço extra no topo para o texto
  labs(
    # title = "Comparação de Tempo de Execução",
    x = "\n Cenário",
    y = "Tempo (segundos)",
    fill = "Modelo"
  ) +
  theme_minimal() +
  theme(
    panel.background =  element_rect(color = "grey70"),
    panel.border = element_rect(color = "grey70", fill = NA, size = 0.5),
    axis.text.x   = element_text(color = "grey20",size = 7),
    axis.text.y   = element_text(color = "grey20",size = 7),
    axis.title.x  = element_text(color = "grey20",face = "bold", size = 8),
    axis.title.y  = element_text(color = "grey20",face = "bold", size = 8),
    # title =  element_text(color = "grey10",face = "bold", size = 14),
    legend.title = element_text(size = 8),
    legend.text = element_text(color = "grey20", size = 7),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  scale_y_continuous(
    breaks = scales::pretty_breaks(n = 16),
    expand = expansion(mult = c(0, 0.1)),
    limits = c(0,26000)
    # limits = c(0,12000)
  ) + 
  
  scale_fill_manual(
    values = c(
      "Log-binomial frequentista" = "#8B2E2E",  # azul escuro                   # vermelho escuro
      "Poisson robusto"  = "#1F4E79", 
      "Log-binomial bayesiano"= "#2E5D34"                   # verde escuro
    )
  )

grafico_tempo_execucao_3

# 2 cenários 
grafico_tempo_execucao_2 <-
  tabela_execucao %>% 
  filter(cenario != "Geral") %>%
  ggplot(aes(x = cenario, y = valor_S , fill = modelo)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    geom_text(
      aes(label = paste0(valor_HMS, "\n ", "(", valor_S,"s)")),
      position = position_dodge(width = 0.8),
      vjust = -0.5,
      size = 3.5
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) + # Espaço extra no topo para o texto
    labs(
      # title = "Comparação de Tempo de Execução",
      x = "\n Cenário",
      y = "Tempo (segundos)",
      fill = "Modelo"
    ) +
    theme_minimal() +
    theme(
      panel.background =  element_rect(color = "grey70"),
      panel.border = element_rect(color = "grey70", fill = NA, size = 0.5),
      axis.text.x   = element_text(color = "grey20",size = 7),
      axis.text.y   = element_text(color = "grey20",size = 7),
      axis.title.x  = element_text(color = "grey20",face = "bold", size = 8),
      axis.title.y  = element_text(color = "grey20",face = "bold", size = 8),
      # title =  element_text(color = "grey10",face = "bold", size = 14),
      legend.title = element_text(size = 8),
      legend.text = element_text(color = "grey20", size = 7),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_blank()
    ) +
    scale_y_continuous(
      breaks = scales::pretty_breaks(n = 16),
      expand = expansion(mult = c(0, 0.1)),
      # limits = c(0,26000)
      limits = c(0,12000)
    ) + 
    
    scale_fill_manual(
      values = c(
        "Log-binomial frequentista" = "#8B2E2E",  # azul escuro                   # vermelho escuro
        "Poisson robusto"  = "#1F4E79", 
        "Log-binomial bayesiano"= "#2E5D34"                   # verde escuro
      )
    )

grafico_tempo_execucao_2

## exportação gráfico ------------------------

ggsave(
  filename = "plots/3-plots-simulacao/tempos/tempo_execucao_modelos_3_cenarios.pdf",
  plot = grafico_tempo_execucao_3,
  device = cairo_pdf,
  width = 28,
  height = 14,
  units = "cm"
)


ggsave(
  filename = "plots/3-plots-simulacao/tempos/tempo_execucao_modelos_2_cenarios.pdf",
  plot = grafico_tempo_execucao_2,
  device = cairo_pdf,
  width = 28,
  height = 14,
  units = "cm"
)
