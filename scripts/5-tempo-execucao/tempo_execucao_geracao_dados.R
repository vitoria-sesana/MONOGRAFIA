# rotina ------------------------------------------------------------------
source("scripts/0-rotina.R", encoding = "UTF-8")


# função auxiliar ---------------------------------------------------------

formatar_tempo_S <- function(x) {
  segundos <- as.numeric(x, units = "secs")
  
  trunc(segundos)
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



# Cenários -----------------------------------------------------------------

## leitura -----------------------------------------------------------------
G_tempo_execucao_dados <- 
  "saidas/2-saida-simulacao/tempo_execucao.rds" %>% 
  readRDS()   

C_tempo_execucao_dados <- 
  "saidas/3-saida-simulacao-completo/C-tempo_execucao.rds" %>% 
  readRDS()   

NC_tempo_execucao_dados <- 
  "saidas/3-saida-simulacao-completo/NC-tempo_execucao.rds" %>% 
  readRDS()  



## tabela ------------------------------------------------------------------

tabela_tempo_execucao_dados <- data.frame(
  Bases = c(
    "Geral",
    "Cenário 1 - Converge",
    "Cenário 2 - Não converge"
  ),
  valor_HMS = c(
    formatar_tempo_HMS(G_tempo_execucao_dados),
    formatar_tempo_HMS(C_tempo_execucao_dados),
    formatar_tempo_HMS(NC_tempo_execucao_dados)
  ),
  valor_S = c(
    formatar_tempo_S(G_tempo_execucao_dados),
    formatar_tempo_S(C_tempo_execucao_dados),
    formatar_tempo_S(NC_tempo_execucao_dados)
  )
)

tabela_tempo_execucao_dados


# saída latex -------------------------------------------------------------
kbl(
  tabela_tempo_execucao_dados,
  format = "latex",
  booktabs = TRUE,
  align = "rrl",
  caption = "Tabela tempo de execução das bases geradas em cada cenário."
) %>%
  kable_styling(
    latex_options = c("hold_position"),
    stripe_color = "gray!15"
  ) 

# gráfico -----------------------------------------------------------------

tabela_tempo_execucao_dados <- 
  tabela_tempo_execucao_dados %>%
  mutate(
    Bases = factor(
      Bases,
      levels = c("Geral", "Cenário 1 - Converge", "Cenário 2 - Não converge")
    )
  )

grafico_tempo_execucao_dados_1 <-
  tabela_tempo_execucao_dados %>% 
  # filter(cenario != "Geral") %>%
  ggplot(aes(x = Bases, y = valor_S)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7,    fill = "#D4A017") +
  geom_text(
    aes(label = paste0(valor_HMS, "\n ", "(", valor_S,"s)")),
    position = position_dodge(width = 0.8),
    vjust = -0.5,
    size = 3.5
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) + # Espaço extra no topo para o texto
  labs(
    # title = "Comparação de Tempo de Execução",
    x = "\n Base de dados",
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
  ) 
# scale_y_continuous(
#   breaks = scales::pretty_breaks(n = 16),
#   expand = expansion(mult = c(0, 0.1)),
#   limits = c(0,26000)
#   # limits = c(0,12000)
# ) 

grafico_tempo_execucao_dados_2 <-
  tabela_tempo_execucao_dados %>% 
  filter(Bases != "Geral") %>%
  ggplot(aes(x = Bases, y = valor_S)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7,    fill = "#D4A017") +
  geom_text(
    aes(label = paste0(valor_HMS, "\n ", "(", valor_S,"s)")),
    position = position_dodge(width = 0.8),
    vjust = -0.5,
    size = 3.5
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) + # Espaço extra no topo para o texto
  labs(
    # title = "Comparação de Tempo de Execução",
    x = "\n Base de dados",
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
  ) 
# scale_y_continuous(
#   breaks = scales::pretty_breaks(n = 16),
#   expand = expansion(mult = c(0, 0.1)),
#   limits = c(0,26000)
#   # limits = c(0,12000)
# ) 

grafico_tempo_execucao_dados_1
grafico_tempo_execucao_dados_2

## exportação gráfico ------------------------

ggsave(
  filename = "plots/3-plots-simulacao/tempos/tempo_execucao_geracao_dados_3_cenarios.pdf",
  plot = grafico_tempo_execucao_dados_1,
  device = cairo_pdf,
  width = 28,
  height = 14,
  units = "cm"
)

ggsave(
  filename = "plots/3-plots-simulacao/tempos/tempo_execucao_geracao_dados_2_cenarios.pdf",
  plot = grafico_tempo_execucao_dados_2,
  device = cairo_pdf,
  width = 28,
  height = 14,
  units = "cm"
)

