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
  "saidas/3-saida-simulacao-completo/C-saidas/C-M1-logbin-frequentista/C-tempo_execucao_logbin_frequentista_replica.rds" %>% 
  readRDS() %>% 
  tibble::enframe(name = "base", value = "tempo") %>%
  mutate(
    amostra = as.integer(stringr::str_extract(base, "(?<=amostra_)\\d+")),
    replica = as.integer(stringr::str_extract(base, "(?<=replica_)\\d+")),
    modelo = "Log-binomial frequentista"
  ) %>% 
  select(modelo, base, amostra, replica, tempo) 



C_tempo_poisson_robusto <- 
  "saidas/3-saida-simulacao-completo/C-saidas/C-M2-poisson-robusto/C-tempo_execucao_ajustes_poisson_robusto_replica.rds" %>% 
  readRDS() %>% 
  mutate(
    amostra = as.integer(stringr::str_extract(base, "(?<=amostra_)\\d+")),
    replica = as.integer(stringr::str_extract(base, "(?<=replica_)\\d+")),
    modelo = "Poisson robusto",
    tempo = tempo_total
  ) %>% 
    select(modelo, base, amostra, replica, tempo) 
  


C_tempo_logbin_bayes <- 
  "saidas/3-saida-simulacao-completo/C-saidas/C-M3-logbin-bayesiano/C-tempo_execucao_ajustes_logbin_bayesiano_replica.rds" %>% 
  readRDS()  %>% 
  mutate(
    modelo = "Log-binomial bayesiano",
    base = nome_dado,
    tempo = tempo_segundos, 
    amostra = as.integer(stringr::str_extract(base, "(?<=amostra_)\\d+")),
    replica = as.integer(stringr::str_extract(base, "(?<=replica_)\\d+")),
  ) %>% 
  select(modelo, base, amostra, replica, tempo) 

  
  
## tabela ------------------------------------------------------------------

C_tabela_tempo <- 
  rbind(
    C_tempo_logbin_freq,
    C_tempo_poisson_robusto,
    C_tempo_logbin_bayes
  ) %>% 
  mutate(
    cenario = "Cenário 1 - Converge",
    valor_S = formatar_tempo_S(tempo),
    valor_HMS = formatar_tempo_HMS(valor_S)
  )

C_tabela_tempo

# Cenário NC --------------------------------------------------------------

## leitura -----------------------------------------------------------------
# NC_tempo_logbin_freq <- 
#   "saidas/3-saida-simulacao-completo/NC-saidas/NC-M1-logbin-frequentista/NC-tempo_execucao_logbin_frequentista_replica.rds" %>% 
#   readRDS() %>% 
#   tibble::enframe(name = "base", value = "tempo") %>%
#   mutate(
#     amostra = as.integer(stringr::str_extract(base, "(?<=amostra_)\\d+")),
#     replica = as.integer(stringr::str_extract(base, "(?<=replica_)\\d+")),
#     modelo = "Log-binomial frequentista"
#   ) %>% 
#   select(modelo, base, amostra, replica, tempo) 



NC_tempo_poisson_robusto <- 
  "saidas/3-saida-simulacao-completo/NC-saidas/NC-M2-poisson-robusto/NC-tempo_execucao_ajustes_poisson_robusto_replica.rds" %>% 
  readRDS() %>% 
  mutate(
    amostra = as.integer(stringr::str_extract(base, "(?<=amostra_)\\d+")),
    replica = as.integer(stringr::str_extract(base, "(?<=replica_)\\d+")),
    modelo = "Poisson robusto",
    tempo = tempo_total
  ) %>% 
  select(modelo, base, amostra, replica, tempo) 



NC_tempo_logbin_bayes <- 
  "saidas/3-saida-simulacao-completo/NC-saidas/NC-M3-logbin-bayesiano/NC-tempo_execucao_ajustes_logbin_bayesiano_replica.rds" %>% 
  readRDS()  %>% 
  mutate(
    modelo = "Log-binomial bayesiano",
    base = nome_dado,
    tempo = tempo_segundos, 
    amostra = as.integer(stringr::str_extract(base, "(?<=amostra_)\\d+")),
    replica = as.integer(stringr::str_extract(base, "(?<=replica_)\\d+")),
  ) %>% 
  select(modelo, base, amostra, replica, tempo) 



## tabelas -----------------------------------------------------------------

NC_tabela_tempo <- 
  rbind(
    # NC_tempo_logbin_freq,
    NC_tempo_poisson_robusto,
    NC_tempo_logbin_bayes
  ) %>% 
  mutate(
    cenario = "Cenário 2 - Não converge",
    valor_S = formatar_tempo_S(tempo),
    valor_HMS = formatar_tempo_HMS(valor_S)
  )

C_tabela_tempo
NC_tabela_tempo

# tabela final ------------------------------------------------------------

tabela_tempo <- 
  bind_rows(
    C_tabela_tempo, 
    NC_tabela_tempo
  )

tabela_tempo <- 
  tabela_tempo %>%
  mutate(
    cenario = factor(
      cenario,
      levels = c(
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

tabela_tempo



# resumo  -----------------------------------------------------------------

tabela_resumo <- 
  tabela_tempo %>% 
  group_by(modelo, cenario, amostra) %>% 
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
  dplyr::arrange(modelo, cenario, amostra)

tabela_resumo



# Saída latex -------------------------------------------------------------
kbl(
  tabela_resumo,
  format = "latex",
  booktabs = TRUE,
  align = "ccccccccccccccccccccccccc",
  caption = "Resumo dos tempo de execução por modelo, cenário e tamanho amostral."
) 


