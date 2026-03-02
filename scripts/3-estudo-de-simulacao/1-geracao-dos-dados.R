rm(list = ls())

# função para geração dos dados -----------------------------------------------
gerando_dados_respiratory <- function(replicas, amostras, n_clusters = NULL) {
  # replicas: número de réplicas por tamanho
  # amostras: vetor com tamanhos amostrais
  # n_clusters: número de clusters (se NULL, assume cada linha como 1 cluster)
  suppressMessages(require(dplyr))
  bases_amostras <- list()
  
  for (i in 1:length(amostras)) {
    n_obs <- amostras[i]
    bases_replicas <- list()
    
    for (j in 1:replicas) {
      center <- rbinom(n_obs, 1, 0.5)
      treat <- rbinom(n_obs, 1, 0.51)
      baseline <- rbinom(n_obs, 1, 0.53)
      
      ## betas ----- 
      b0 =  -0.1089
      b_center = -0.3414
      b_treat = -0.2544
      b_baseline = -0.5994
      
      p_vetor <- 
        exp(b0 
            + (b_center * center)
            + (b_treat * treat)
            + (b_baseline * baseline)
        )
      
      y_vetor <- rbinom(n_obs, 1, p_vetor)
      
      dado_simulado_n <- data.frame(
        y = y_vetor,
        treat,
        baseline, 
        center,
        p = p_vetor
      )
      
      # Define id
      if (!is.null(n_clusters)) {
        if (n_obs %% n_clusters != 0) {
          stop("n_obs deve ser divisível por n_clusters")
        }
        dado_simulado_n <- dado_simulado_n |>
          mutate(id = rep(1:n_clusters, each = n_obs / n_clusters))
      } else {
        dado_simulado_n <- dado_simulado_n |>
          mutate(id = 1:n_obs)
      }
      
      index_rep <- paste0("amostra_", amostras[i], "_replica_", j)
      bases_replicas[[index_rep]] <- dado_simulado_n
    }
    
    bases_amostras <- c(bases_amostras, bases_replicas)
  }
  
  return(bases_amostras)
}

# geranndo base de dados simulada------------------------------------------
set.seed(34)
dados_gerados_respiratory <- 
  gerando_dados_respiratory(
    replicas = 1000, 
    amostras = c(50, 100, 200, 500), 
    n_clusters = NULL
  )

# saida ----------------------------------------------------------------
saveRDS(
  dados_gerados_respiratory,
  "saidas/2-saida-simulacao/dados_gerados.rds"
)
