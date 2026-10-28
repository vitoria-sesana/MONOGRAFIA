rm(list = ls())

# leitura -----------------------------------------------------------------
dados_gerados <- readRDS("saidas/3-saida-simulacao-completo/C-dados_convergidos_gerados.rds")

# ajuste logbin bayesiano -----------------------------------------------
require(rjags)

# modelo ------------------------------------------------------------------
modelo <- "
model{
  # --- Likelihood -----------------------------------------------------
  for (i in 1:N) {
    pp[i] <- exp(b0 + b1*X1[i] + b2*X2[i] + b3*X3[i])
    Y[i] ~ dbern(pp[i])
  }
  
  # --- Priors ---------------------------------------------------------
  b0 ~ dnorm(0, 0.01)
  b1 ~ dnorm(0, 0.01)
  b2 ~ dnorm(0, 0.01)
  b3 ~ dnorm(0, 0.01)
  
  # --- Constraints for each observation ------------------------------
  for (i in 1:N) {
    ones[i] ~ dbern(C1[i])     # use ones[i] supplied from R
    C1[i] <- step(1 - pp[i])   # ensures pp[i] ≤ 1
  }
  
  # --- Constraints for the 8 possible combinations of (X1,X2,X3) -----
  ones.e ~ dbern(C1.e)         # use ones.e supplied from R
  C1.e <-
    step(1 - exp(b0)) *
    step(1 - exp(b0 + b1)) *
    step(1 - exp(b0 + b2)) *
    step(1 - exp(b0 + b3)) *
    step(1 - exp(b0 + b1 + b2)) *
    step(1 - exp(b0 + b1 + b3)) *
    step(1 - exp(b0 + b2 + b3)) *
    step(1 - exp(b0 + b1 + b2 + b3))
}
"

# inits -------------------------------------------------------------------
chutes_iniciais <- 
  list(
    b0 = -1,
    b1 = -0.5,
    b2 = -0.5,
    b3 = -0.5,
    .RNG.name = "base::Mersenne-Twister", # gerador de semente
    .RNG.seed = 34 # semente pra manter os mesmos resultados
  )

# funções -----------------------------------------------------------------
funcao_jags <- function(model_string, inits_list, dado, nome_dado) {
  
  print(paste("Simulação:", nome_dado))
  
  # dados
  dataList <- 
    list(
      N = length(dado$y), 
      Y = dado$y,
      X1 = dado$center,
      X2 = dado$treat,
      X3 = dado$baseline
    )
  
  # ajuste jags
  model <- 
    rjags::jags.model(
      file = textConnection(model_string),
      data = dataList,
      inits = inits_list,
      n.chains = 1,
      n.adapt = 0
    )
  
  # atualização
  update(model, n.iter = 1000)
  
  # amostras das posteriores
  posterior <- 
    rjags::coda.samples(
      model,
      variable.names = c("b0", "b1", "b2", "b3"),
      n.iter = 10000,
      thin = 40
    )
  
  return(posterior)
}

# ajuste -----------------------------------------------------------
tempo_inicial_jags <- Sys.time()

tempos_jags <- purrr::imap(dados_gerados, function(dado, nome) {
  
  tempo_inicio <- Sys.time()
  
  resultado <- funcao_jags(
    model_string = modelo,
    inits_list   = chutes_iniciais,
    dado         = dado,
    nome_dado    = nome
  )
  
  tempo_fim <- Sys.time()
  
  list(
    modelo = resultado,
    tempo_segundos = as.numeric(
      difftime(tempo_fim, tempo_inicio, units = "secs")
    ),
    nome_dado = nome
  )
})

tempo_final_jags <- Sys.time()

tempo_execucao_jags <- difftime(
  tempo_final_jags,
  tempo_inicial_jags,
  units = "secs"
)

modelos_logbin_jags <- purrr::map(tempos_jags, "modelo")


# tempo de execução -------------------------------------------------------
resumo_tempos_jags_base <- 
  purrr::imap_dfr(tempos_jags, function(x, nome) {
    tibble::tibble(
      nome_dado = nome,
      tempo_segundos = x$tempo_segundos
    )
  })

resumo_tempos_jags <- 
  resumo_tempos_jags_base %>%
  dplyr::mutate(
    amostra = as.integer(stringr::str_extract(nome_dado, "\\d+"))
  ) %>%
  dplyr::group_by(amostra) %>%
  dplyr::summarise(
    n_replicas = dplyr::n(),
    tempo_total = sum(tempo_segundos),
    tempo_medio = mean(tempo_segundos),
    desvio_padrao = sd(tempo_segundos),
    mediana = median(tempo_segundos),
    tempo_maximo = max(tempo_segundos),
    tempo_minimo = min(tempo_segundos),
    .groups = "drop"
  )

##
# modelos_logbin_jags
# tempo_execucao_jags
# resumo_tempos_jags_base
# resumo_tempos_jags

# saida -------------------------------------------------------------------
saveRDS(
  modelos_logbin_jags, # modelo logbinomial bayesiano
  file = "saidas/3-saida-simulacao-completo/C-saidas/C-M3-logbin-bayesiano/C-ajustes_logbin_bayesiano.rds"
)

saveRDS(
  resumo_tempos_jags_base, # tempo de execução modelo logbinomial bayesiano por replica
  file = "saidas/3-saida-simulacao-completo/C-saidas/C-M3-logbin-bayesiano/C-tempo_execucao_ajustes_logbin_bayesiano_replica.rds"
)

saveRDS(
  resumo_tempos_jags, # tempo de execução modelo logbinomial bayesiano por tamanho amostral
  file = "saidas/3-saida-simulacao-completo/C-saidas/C-M3-logbin-bayesiano/C-tempo_execucao_ajustes_logbin_bayesiano_tamanho_amostral.rds"
)
