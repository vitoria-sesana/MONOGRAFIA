rm(list = ls())

# função gerar 1000 bases convergidas --------------------------------------

func_gerar_mil_bases_conv <- function(replicas, amostras, n_clusters = NULL) {
  # replicas: número de réplicas por tamanho
  # amostras: vetor com tamanhos amostrais
  # n_clusters: número de clusters (se NULL, assume cada linha como 1 cluster)
  suppressMessages(require(dplyr))
  bases_conv <- list()
  modelo_lgbin_conv <- list()
  
  for (i in 1:length(amostras)) {
    n_obs <- amostras[i]
    bases_replicas_conv <- list()
    modelos_replicas_conv <- list()
    contagem_conv <- 1
    
    while (contagem_conv <= replicas) {
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
      
      # Define id para ajustar modelos Poisson 
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
      
      index_rep <- paste0("amostra_", amostras[i], "_replica_", contagem_conv)
      
      ## ajuste do modelo 
      
      modelo_logbin_frequentista <- 
        logbin::logbin(
          y ~ center  + treat + baseline, 
          data = dado_simulado_n
        ) 
      
      qntd_conv = sum(is.na(vcov(modelo_logbin_frequentista)))
      
      if (qntd_conv == 0) {
        bases_replicas_conv[[index_rep]] <- dado_simulado_n
        modelos_replicas_conv[[index_rep]] <- modelo_logbin_frequentista
        contagem_conv <- contagem_conv + 1
      } 
      
      cat(
        "n = ", n_obs, "\n",
        "Quantidade bases não convergidas", contagem_conv - 1, "\n"
      )
    }
    
    bases_conv <- c(bases_conv, bases_replicas_conv)
    modelo_lgbin_conv <- c(modelo_lgbin_conv, modelos_replicas_conv)
  }
  
  lista_unica <- list(
    bases_conv = bases_conv,
    modelo_lgbin_conv = modelo_lgbin_conv
  )
  
  return(lista_unica)
}


# geranndo base de dados simulada------------------------------------------
set.seed(34)
dados_e_modelos_gerados_conv <- 
  func_gerar_mil_bases_conv(
    replicas = 1000, 
    amostras = c(50, 100), 
    n_clusters = NULL
  )

dados_gerados_convergidos <- dados_e_modelos_gerados_conv$bases_conv
modelos_logbin_freq_convergidos <- dados_e_modelos_gerados_conv$modelo_lgbin_conv

# validação visual -------------------------------------------------------

dados_gerados_convergidos
dados_gerados_convergidos$amostra_50_replica_1

modelos_logbin_freq_convergidos
lapply(modelos_logbin_freq_convergidos, vcov)

# saída ------------------------------------------------------------------

saveRDS(
  dados_gerados_convergidos,
  file = "saidas/3-saida-simulacao-completo/C-dados_convergidos_gerados.rds"
)
