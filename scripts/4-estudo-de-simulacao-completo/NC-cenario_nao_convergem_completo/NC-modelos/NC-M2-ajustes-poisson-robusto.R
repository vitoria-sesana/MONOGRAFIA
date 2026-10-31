rm(list = ls())

# leitura -----------------------------------------------------------------
dados_gerados <- readRDS("saidas/3-saida-simulacao-completo/NC-dados_nao_convergidos_gerados.rds")

# ajuste sandwich ---------------------------------------------------------
tempos_poisson <- tibble(
  base = names(dados_gerados),
  tempo_glm = NA_real_,
  tempo_robusto = NA_real_,
  tempo_total = NA_real_
)

modelos_poisson_classico <- purrr::map(
  seq_along(dados_gerados),
  function(i) {
    
    tempo_inicial <- Sys.time()
    
    modelo <- glm(
      y ~ center + treat + baseline,
      family = poisson(link = "log"),
      data = dados_gerados[[i]], 
      start =  c(-1, -0.5, -0.5, -0.5)
    )
    
    tempo_glm <- as.numeric(
      difftime(Sys.time(), tempo_inicial, units = "secs")
    )
    
    tempo_inicial_robusto <- Sys.time()
    
    lmtest::coeftest(
      modelo,
      vcov = sandwich::sandwich
    )
    
    tempo_robusto <- as.numeric(
      difftime(Sys.time(), tempo_inicial_robusto, units = "secs")
    )
    
    tempos_poisson$tempo_glm[i] <<- tempo_glm
    tempos_poisson$tempo_robusto[i] <<- tempo_robusto
    tempos_poisson$tempo_total[i] <<- tempo_glm + tempo_robusto
    
    modelo
  }
)

names(modelos_poisson_classico) <- names(dados_gerados)

# modelos
modelos_poisson_robusto <- purrr::map(
  modelos_poisson_classico,
  ~ lmtest::coeftest(.x, vcov = sandwich::sandwich)
)


# tempos de execução por tamanho amostral
tempos_poisson <- tempos_poisson %>%
  mutate(
    amostra = as.integer(
      stringr::str_extract(base, "(?<=amostra_)\\d+")
    )
  )

resumo_tempos_poisson <- tempos_poisson %>%
  mutate(
    amostra = as.integer(stringr::str_extract(base, "(?<=amostra_)\\d+"))
  ) %>%
  group_by(amostra) %>%
  summarise(
    n_replicas = n(),
    tempo_glm_total = sum(tempo_glm),
    tempo_robusto_total = sum(tempo_robusto),
    soma_tempo_total = sum(tempo_total),
    tempo_medio = mean(tempo_total),
    desvio_padrao = sd(.data$tempo_total), 
    mediana = median(tempo_total),
    tempo_maximo = max(tempo_total),
    tempo_minimo = min(tempo_total),
    .groups = "drop"
  )

resumo_tempos_poisson

# saida -------------------------------------------------------------------
saveRDS(
  modelos_poisson_classico, # modelo Poisson clássico
  file = "saidas/3-saida-simulacao-completo/NC-saidas/NC-M2-poisson-robusto/NC-ajustes_poisson_classico.rds"
)

saveRDS(
  modelos_poisson_robusto, # modelo Poisson robusto
  file = "saidas/3-saida-simulacao-completo/NC-saidas/NC-M2-poisson-robusto/NC-ajustes_poisson_robusto.rds"
)

saveRDS(
  tempos_poisson, # tempo de execução modelo Poisson robusto por replica
  file = "saidas/3-saida-simulacao-completo/NC-saidas/NC-M2-poisson-robusto/NC-tempo_execucao_ajustes_poisson_robusto_replica.rds"
)

saveRDS(
  resumo_tempos_poisson, # tempo de execução modelo Poisson robusto por tamanho amostral
  file = "saidas/3-saida-simulacao-completo/NC-saidas/NC-M2-poisson-robusto/NC-tempo_execucao_ajustes_poisson_robusto_tamanho_amostral.rds"
)

