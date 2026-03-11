rm(list = ls())

# leitura -----------------------------------------------------------------
dados_gerados <- readRDS("saidas/3-saida-simulacao-completo/NC-dados_nao_convergidos_gerados.rds")

# fórmula logbin ----------------------------------------------------------
formula_logbin <- as.formula("y ~ center + treat + baseline")

# ajuste logbin frequentista ---------------------------------------------
tempo_inicial <- Sys.time()
modelos_logbin <-
  lapply(
    dados_gerados,
    function(bases)
      logbin::logbin(
        formula = formula_logbin,
        data = bases
      )
  )
tempo_final <- Sys.time()
tempo_execucao <- tempo_final - tempo_inicial
tempo_execucao

lapply(modelos_logbin, coefficients)
lapply(modelos_logbin, vcov)

# saida -------------------------------------------------------------------
saveRDS(
  modelos_logbin,
  file = "saidas/3-saida-simulacao-completo/NC-saidas/NC-M1-logbin-frequentista/NC-ajustes_logbin_frequentista.rds"
  )

saveRDS(
  tempo_execucao,
  file = "saidas/3-saida-simulacao-completo/NC-saidas/NC-M1-logbin-frequentista/NC-tempo_execucao_logbin_frequentista.rds"
)
