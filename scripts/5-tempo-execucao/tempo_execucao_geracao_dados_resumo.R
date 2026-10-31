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
    status = ifelse(convergiu == "TRUE", "Sucesso", "Falha")
  ) 

NC_replicas <- 
  "saidas/3-saida-simulacao-completo/tempo_execucao_bases/NC-tempo_execucao_tentativa.rds" %>% 
  readRDS() %>% 
  mutate(
    amostra = as.integer(stringr::str_extract(base, "(?<=amostra_)\\d+")),
    replica = as.integer(stringr::str_extract(base, "(?<=replica_)\\d+")),
    bases = "Bases não convergidas",
    status = ifelse(convergiu == "TRUE", "Falha", "Sucesso")
  ) 


## tabela ------------------------------------------------------------------

tabela_tempo <- 
  rbind(
    C_replicas,
    NC_replicas
  ) %>% 
  rename("tempo" = "tempo_seg") %>% 
  select(bases, status, amostra, everything()) 

tabela_tempo_agregada <- 
  tabela_tempo %>% 
  group_by(bases, status, amostra) %>% 
  summarise(
    n_bases         = dplyr::n(),
    media_seg       = mean(tempo),
    dp_seg          = sd(tempo),
    cv              = sd(tempo) / mean(tempo),  
    min_seg         = min(tempo),
    q1_seg          = quantile(tempo, 0.25),
    mediana_seg     = median(tempo),
    q3_seg          = quantile(tempo, 0.75),
    max_seg         = max(tempo),
    total_seg       = sum(tempo),
    total_seg       = sum(tempo),
    .groups = "drop"
  ) %>% 
  mutate(
    across(where(is.numeric) & !matches("n_bases"), ~ round(., 4)),
    total_HMS = formatar_tempo_HMS(total_seg)
  ) %>%
  dplyr::arrange(bases, status, amostra)


tabela_tempo_agregada_total <- 
  tabela_tempo %>% 
  group_by(bases, amostra) %>% 
  summarise(
    n_bases         = dplyr::n(),
    media_seg       = mean(tempo),
    dp_seg          = sd(tempo),
    cv              = sd(tempo) / mean(tempo),  
    min_seg         = min(tempo),
    q1_seg          = quantile(tempo, 0.25),
    mediana_seg     = median(tempo),
    q3_seg          = quantile(tempo, 0.75),
    max_seg         = max(tempo),
    total_seg       = sum(tempo),
    total_seg       = sum(tempo),
    .groups = "drop"
  ) %>% 
  mutate(
    across(where(is.numeric) & !matches("n_bases"), ~ round(., 4)),
    total_HMS = formatar_tempo_HMS(total_seg)
  ) %>%
  dplyr::arrange(bases, amostra) %>% 
  mutate(status = "Total")
  
tabela_resumo <- 
  bind_rows(
    tabela_tempo_agregada,
    tabela_tempo_agregada_total
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
        "Sucesso",
        "Falha",
        "Total"
      )
    )
  )  %>% 
  arrange(bases, status, amostra)



# Saída latex -------------------------------------------------------------
kbl(
  tabela_resumo,
  format = "latex",
  booktabs = TRUE,
  align = "ccccccccccccccccccccccccc",
  caption = "Resumo dos tempo de execução por base, status e tamanho amostral."
) 
