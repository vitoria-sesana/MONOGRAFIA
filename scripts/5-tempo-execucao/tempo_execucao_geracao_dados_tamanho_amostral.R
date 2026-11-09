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
C_replicas <- 
  "saidas/3-saida-simulacao-completo/tempo_execucao_bases/C-tempo_execucao_tentativa.rds" %>% 
  readRDS() %>% 
  mutate(
    amostra = as.integer(stringr::str_extract(base, "(?<=amostra_)\\d+")),
    replica = as.integer(stringr::str_extract(base, "(?<=replica_)\\d+")),
    bases = "Bases convergidas",
    status = ifelse(convergiu == "TRUE", "Válidas", "Descartadas")
  ) 

NC_replicas <- 
  "saidas/3-saida-simulacao-completo/tempo_execucao_bases/NC-tempo_execucao_tentativa.rds" %>% 
  readRDS() %>% 
  mutate(
    amostra = as.integer(stringr::str_extract(base, "(?<=amostra_)\\d+")),
    replica = as.integer(stringr::str_extract(base, "(?<=replica_)\\d+")),
    bases = "Bases não convergidas",
    status = ifelse(convergiu == "TRUE", "Descartadas", "Válidas")
  ) 

## tabela ------------------------------------------------------------------

tabela_tempo <- 
  rbind(
    C_replicas,
    NC_replicas
  ) %>% 
  select(bases, status, amostra, everything()) 

tabela_tempo_agregada <- 
  tabela_tempo %>% 
  group_by(bases, status, amostra) %>% 
  summarise(
    tempo = sum(tempo_seg)
  )
  
tabela_total <-   
  tabela_tempo %>% 
  group_by(bases, amostra) %>% 
  summarise(
    tempo = sum(tempo_seg)
  ) %>% 
  mutate(status = "Total")



  
  # tabela grafico ----------------------------------------------------------


tabela_grafico <- 
  rbind(
    tabela_tempo_agregada,
    tabela_total
  ) %>% 
  mutate( 
    valor_S = formatar_tempo_S(tempo),
    valor_HMS = formatar_tempo_HMS(valor_S)
  ) %>% 
  mutate(
    bases = factor(
      bases,
      levels = c(
        "Bases convergidas",
        "Bases não convergidas"
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
    status = factor(
      status,
      levels = c(
        "Válidas",
        "Descartadas",
        "Total"
      )
    )
  )  %>% 
  arrange(bases, status, amostra)


# saída latex -------------------------------------------------------------
kbl(
  tabela_grafico,
  format = "latex",
  booktabs = TRUE,
  align = "rrl",
  caption = "Tabela tempo de execução das bases geradas em cada cenário."
)

# gráfico -----------------------------------------------------------------


## convergidas -----
grafico_tempo_bases_convergidas <-
  tabela_grafico %>% 
  filter(bases == "Bases convergidas") %>%
  ggplot(aes(
    x = amostra,
    y = valor_S,
    color = status,
    group = status
  )) +
  
  geom_line(linewidth = 0.9) +
  
  geom_point(size = 3) +
  
  geom_text(
    aes(
      label = paste0(valor_HMS, " (", valor_S, "s)"),
      vjust = case_when(
        status == "Total" ~ -2.5,
        status == "Descartadas" ~ -0.8,
        status == "Válidas" ~ +1.5,
        TRUE ~ -0.9
      ),
      hjust = case_when(
        status == "Total" ~ 0,
        status == "Descartadas" ~ -0.4,
        status == "Válidas" ~ +1.2,
        TRUE ~ -0.3
      )
    ),
    size = 4,
    show.legend = FALSE
  ) +
  
  labs(
    x = expression(bold("Tamanho amostral (") * bolditalic(n) * bold(")")),
    y = "Tempo (segundos)",
    color = "Status"
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
    limits = c(0, 1700),
    breaks = scales::pretty_breaks(n = 16),
    expand = expansion(mult = c(0, 0.15))
  ) +
  
  scale_color_manual(
    values = c(
      "Total"   = "#333333",  # Cinza bem escuro (quase preto)
      "Descartadas"   = "#D95F02",  # Laranja queimado
      "Válidas" = "#1B9E77"   # Verde-azulado (Teal)
    )
  )

grafico_tempo_bases_convergidas

## não convergidas -----
grafico_tempo_bases_nao_convergidas <-
  tabela_grafico %>% 
  filter(bases == "Bases não convergidas") %>%
  ggplot(aes(
    x = amostra,
    y = valor_S,
    color = status,
    group = status
  )) +
  
  geom_line(linewidth = 0.9) +
  
  geom_point(size = 3) +
  
  geom_text(
    aes(
      label = paste0(valor_HMS, " (", valor_S, "s)"),
      vjust = case_when(
        status == "Total" ~ -2.5,
        status == "Descartadas" ~ -1.2,
        status == "Válidas" ~ -1.5,
        TRUE ~ -0.9
      ),
      hjust = case_when(
        status == "Total" ~ +0.5,
        status == "Descartadas" ~ +0.9,
        status == "Válidas" ~ +0.4,
        TRUE ~ -0.3
      )
    ),
    size = 4,
    show.legend = FALSE
  ) +
  
  labs(
    x = expression(bold("Tamanho amostral (") * bolditalic(n) * bold(")")),
    y = "Tempo (segundos)",
    color = "Status"
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
    limits = c(0, 1700),
    breaks = scales::pretty_breaks(n = 16),
    expand = expansion(mult = c(0, 0.15))
  ) +
  
  scale_color_manual(
    values = c(
      "Total"   = "#333333",  # Cinza bem escuro (quase preto)
      "Descartadas"   = "#D95F02",  # Laranja queimado
      "Válidas" = "#1B9E77"   # Verde-azulado (Teal)
    )
  )

grafico_tempo_bases_nao_convergidas

#grafico_tempo_bases_convergidas

# Tabela quantidade -------------------------------------------------------

library(dplyr)
library(tidyr)



contagens_brutas <- tabela_tempo %>%
  count(bases, amostra, status, name = "n") %>%
  pivot_wider(
    names_from = status,
    values_from = n,
    values_fill = 0
  )


if(!("Descartadas" %in% names(contagens_brutas))) contagens_brutas$Descartadas <- 0
if(!("Válidas" %in% names(contagens_brutas))) contagens_brutas$Válidas <- 0



dados_base <- contagens_brutas %>%
  mutate(amostra = as.character(amostra))


tabela_com_totais <- dados_base %>%
  mutate(
    Total = Descartadas + Válidas,
    Descartadas_pct = (Descartadas / Total) * 100,
    Válidas_pct = (Válidas / Total) * 100
  )


subtotais_bases <- tabela_com_totais %>%
  group_by(bases) %>%
  summarise(
    amostra = "Subtotal",
    Descartadas = sum(Descartadas),
    Válidas = sum(Válidas),
    Total = sum(Total),
    Descartadas_pct = (Descartadas / Total) * 100,
    Válidas_pct = (Válidas / Total) * 100,
    .groups = "drop"
  )


total_geral <- tabela_com_totais %>%
  summarise(
    bases = "Total Geral",
    amostra = "Todos",
    Descartadas = sum(Descartadas),
    Válidas = sum(Válidas),
    Total = sum(Total),
    Descartadas_pct = (Descartadas / Total) * 100,
    Válidas_pct = (Válidas / Total) * 100
  )

lista_bases <- unique(tabela_com_totais$bases)

tabela_com_subtotais_ordenada <- bind_rows(
  lapply(lista_bases, function(b) {
    df_b <- filter(tabela_com_totais, bases == b)
    sub_b <- filter(subtotais_bases, bases == b)
    bind_rows(df_b, sub_b)
  })
)

tabela_final <- bind_rows(tabela_com_subtotais_ordenada, total_geral) %>%
  mutate(
    Descartadas   = paste0(Descartadas, " (", sprintf("%.1f", Descartadas_pct), "%)"),
    Válidas = paste0(Válidas, " (", sprintf("%.1f", Válidas_pct), "%)"),
    Total   = as.character(Total)
  ) %>%
  select(bases, amostra, Descartadas, Válidas, Total)

tabela_final

# saída latex -------------------------------------------------------------
kbl(
  tabela_final,
  format = "latex",
  booktabs = TRUE,
  align = "ccccccccccccccccccccccccc",
  caption = "Resumo dos tempo de execução por modelo, cenário e tamanho amostral."
) 

# saída gráfico -----------------------------------------------------------

ggsave(
  filename = "plots/3-plots-simulacao/tempos/C-tempo_geracao_dados.pdf",
  plot = grafico_tempo_bases_convergidas,
  device = cairo_pdf,
  width = 28,
  height = 14,
  units = "cm"
)


ggsave(
  filename = "plots/3-plots-simulacao/tempos/NC-tempo_geracao_dados.pdf",
  plot = grafico_tempo_bases_nao_convergidas,
  device = cairo_pdf,
  width = 28,
  height = 14,
  units = "cm"
)







