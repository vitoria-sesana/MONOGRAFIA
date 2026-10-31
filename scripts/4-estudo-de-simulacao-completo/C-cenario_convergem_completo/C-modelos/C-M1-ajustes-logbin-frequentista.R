rm(list = ls())

# leitura -----------------------------------------------------------------
dados_gerados <- readRDS("saidas/3-saida-simulacao-completo/C-dados_convergidos_gerados.rds")

# fórmula logbin ----------------------------------------------------------
formula_logbin <- as.formula("y ~ center + treat + baseline")

# ajuste logbin frequentista ---------------------------------------------
tempos_logbin <- numeric(length(dados_gerados))
names(tempos_logbin) <- names(dados_gerados)

modelos_logbin <- lapply(names(dados_gerados), function(nome) {
  
  tempo_inicial <- Sys.time()
  
  modelo <- logbin::logbin(
    formula = formula_logbin,
    data = dados_gerados[[nome]],
    start =  c(-1, -0.5, -0.5, -0.5)
  )
  
  tempo_final <- Sys.time()
  
  tempos_logbin[nome] <<- as.numeric(
    difftime(tempo_final, tempo_inicial, units = "secs")
  )
  
  modelo
})

names(modelos_logbin) <- names(dados_gerados)


# tempo de execução -------------------------------------------------------
resumo_tempos_logbin_freq <- tibble(
  base = names(tempos_logbin),
  tempo_segundos = as.numeric(tempos_logbin)
) %>%
  mutate(
    amostra = as.integer(
      stringr::str_extract(base, "(?<=amostra_)\\d+")
    )
  ) %>%
  group_by(amostra) %>%
  summarise(
    n_replicas = n(),
    tempo_total = sum(tempo_segundos),
    tempo_medio = mean(tempo_segundos),
    desvio_padrao = sd(tempo_segundos),
    mediana = median(tempo_segundos),
    tempo_maximo = max(tempo_segundos),
    tempo_minimo = min(tempo_segundos),
    .groups = "drop"
  )

resumo_tempos_logbin_freq

# saida -------------------------------------------------------------------
saveRDS(
  modelos_logbin, # modelo logbinomial frequentista
  file = "saidas/3-saida-simulacao-completo/C-saidas/C-M1-logbin-frequentista/C-ajustes_logbin_frequentista.rds"
  )

saveRDS(
  tempos_logbin, # tempo de execução modelo logbinomial frequentista por replica
  file = "saidas/3-saida-simulacao-completo/C-saidas/C-M1-logbin-frequentista/C-tempo_execucao_logbin_frequentista_replica.rds"
)

saveRDS(
  resumo_tempos_logbin_freq, # tempo de execução modelo logbinomial frequentista por tamanho amostral
  file = "saidas/3-saida-simulacao-completo/C-saidas/C-M1-logbin-frequentista/C-tempo_execucao_logbin_frequentista_tamanho_amostral.rds"
)
