rm(list = ls())

# função gerar bases não convergidas ---------------------------------------

func_gerar_mil_bases_nao_conv <- function(replicas, amostras, n_clusters = NULL) {
  # replicas: número de réplicas por tamanho
  # amostras: vetor com tamanhos amostrais
  # n_clusters: número de clusters (se NULL, assume cada linha como 1 cluster)
  suppressMessages(require(dplyr))
  
  bases_nao_conv        <- list()
  modelo_lgbin_nao_conv <- list()
  lista_tentativas      <- list()   # uma linha por TENTATIVA (convergida ou não)
  
  for (i in seq_along(amostras)) {
    n_obs <- amostras[i]
    bases_replicas_nao_conv   <- list()
    modelos_replicas_nao_conv <- list()
    contagem_nao_conv  <- 1
    tentativas_replica <- 0
    
    while (contagem_nao_conv <= replicas) {
      t_ini <- Sys.time()
      tentativas_replica <- tentativas_replica + 1
      
      center   <- rbinom(n_obs, 1, 0.5)
      treat    <- rbinom(n_obs, 1, 0.51)
      baseline <- rbinom(n_obs, 1, 0.53)
      
      ## betas -----
      b0         <- -0.1089
      b_center   <- -0.3414
      b_treat    <- -0.2544
      b_baseline <- -0.5994
      
      p_vetor <- exp(b0 + b_center * center + b_treat * treat + b_baseline * baseline)
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
      
      index_rep <- paste0("amostra_", n_obs, "_replica_", contagem_nao_conv)
      
      ## ajuste do modelo
      modelo_logbin_frequentista <- 
        logbin::logbin(
          y ~ center + treat + baseline,
          data = dado_simulado_n
        )
      
      nao_convergiu <- sum(is.na(vcov(modelo_logbin_frequentista))) > 0
      
      # tempo desta tentativa (geração + ajuste + checagem)
      tempo_tentativa <- as.numeric(difftime(Sys.time(), t_ini, units = "secs"))
      
      # registra TODA tentativa, convergindo ou não
      lista_tentativas[[length(lista_tentativas) + 1]] <- data.frame(
        base       = index_rep,
        n_amostral = n_obs,
        replica    = contagem_nao_conv,
        tentativa  = tentativas_replica,
        convergiu  = !nao_convergiu,
        tempo_seg  = tempo_tentativa
      )
      
      if (nao_convergiu) {
        bases_replicas_nao_conv[[index_rep]]   <- dado_simulado_n
        modelos_replicas_nao_conv[[index_rep]] <- modelo_logbin_frequentista
        
        contagem_nao_conv  <- contagem_nao_conv + 1
        tentativas_replica <- 0
        
        cat("n =", n_obs, "| bases não convergidas:", contagem_nao_conv - 1, "\n")
      }
    }
    
    bases_nao_conv        <- c(bases_nao_conv, bases_replicas_nao_conv)
    modelo_lgbin_nao_conv <- c(modelo_lgbin_nao_conv, modelos_replicas_nao_conv)
  }
  
  # 1) tabela por tentativa ------------------------------------------------
  tabela_tempo_tentativa <- dplyr::bind_rows(lista_tentativas) |>
    dplyr::mutate(
      status = ifelse(convergiu, "convergiu", "não convergiu")
    )
  rownames(tabela_tempo_tentativa) <- NULL
  
  # 2) tabela por réplica --------------------------------------------------
  # aqui a tentativa-alvo é a NÃO convergida; as convergidas são descartadas
  tabela_tempo_base <- tabela_tempo_tentativa |>
    dplyr::group_by(base, n_amostral, replica) |>
    dplyr::summarise(
      n_tentativas                  = dplyr::n(),
      n_convergidas_descartadas     = sum(convergiu),
      tempo_tentativa_nao_convergida = sum(tempo_seg[!convergiu]),  # tempo da réplica que foi aceita
      tempo_convergidas_descartadas  = sum(tempo_seg[convergiu]),   # tempo gasto nas descartadas
      tempo_ate_nao_convergir        = sum(tempo_seg),              # tempo total até achar uma não convergida
      .groups = "drop"
    )
  
  # 3) tabela por tamanho amostral ----------------------------------------
  tabela_tempo_total <- tabela_tempo_tentativa |>
    dplyr::group_by(n_amostral) |>
    dplyr::summarise(
      n_tentativas                  = dplyr::n(),
      n_convergidas                 = sum(convergiu),
      n_nao_convergidas             = sum(!convergiu),
      taxa_nao_convergencia         = mean(!convergiu),
      tempo_convergidas_seg         = sum(tempo_seg[convergiu]),
      tempo_nao_convergidas_seg     = sum(tempo_seg[!convergiu]),
      tempo_total_seg               = sum(tempo_seg),
      tempo_total_min               = sum(tempo_seg) / 60,
      tempo_medio_tentativa_seg     = mean(tempo_seg),
      tempo_medio_replica_seg       = sum(tempo_seg) / sum(!convergiu),  # custo médio por réplica não convergida
      .groups = "drop"
    )
  
  list(
    bases_nao_conv         = bases_nao_conv,
    modelo_lgbin_nao_conv  = modelo_lgbin_nao_conv,
    tabela_tempo_tentativa = tabela_tempo_tentativa,
    tabela_tempo_base      = tabela_tempo_base,
    tabela_tempo_total     = tabela_tempo_total
  )
}


# gerando base de dados simulada ------------------------------------------
set.seed(34)
tempo_inicial_nc <- Sys.time()

dados_e_modelos_gerados_nao_conv <- 
  func_gerar_mil_bases_nao_conv(
    replicas   = 1000, 
    amostras   = c(50, 100, 200), 
    n_clusters = NULL
  )

tempo_final_nc    <- Sys.time()
tempo_execucao_nc <- as.numeric(difftime(tempo_final_nc, tempo_inicial_nc, units = "secs"))
tempo_execucao_nc   

# extraindo os resultados --------------------------------------------------
dados_gerados_nao_convergidos       <- dados_e_modelos_gerados_nao_conv$bases_nao_conv
modelos_logbin_freq_nao_convergidos <- dados_e_modelos_gerados_nao_conv$modelo_lgbin_nao_conv
tabela_tempo_tentativa_nc           <- dados_e_modelos_gerados_nao_conv$tabela_tempo_tentativa
tabela_tempo_replica_nc             <- dados_e_modelos_gerados_nao_conv$tabela_tempo_base
tabela_tempo_tamanho_amostral_nc    <- dados_e_modelos_gerados_nao_conv$tabela_tempo_total


# validação ----------------------------------------------------------------
length(dados_gerados_nao_convergidos)          
table(tabela_tempo_replica_nc$n_amostral)          
table(tabela_tempo_tentativa_nc$n_amostral, tabela_tempo_tentativa_nc$status)
sum(tabela_tempo_tamanho_amostral_nc$tempo_total_seg)      

dados_gerados_nao_convergidos$amostra_50_replica_1
lapply(modelos_logbin_freq_nao_convergidos, vcov)

# saída ------------------------------------------------------------------

saveRDS(
  dados_gerados_nao_convergidos,
  file = "saidas/3-saida-simulacao-completo/NC-dados_nao_convergidos_gerados.rds"
)

saveRDS(
  tabela_tempo_tentativa_nc,
  file = "saidas/3-saida-simulacao-completo/tempo_execucao_bases/NC-tempo_execucao_tentativa.rds"
)

saveRDS(
  tabela_tempo_replica_nc,
  file = "saidas/3-saida-simulacao-completo/tempo_execucao_bases/NC-tempo_execucao_replica.rds"
)

saveRDS(
  tabela_tempo_tamanho_amostral_nc,
  file = "saidas/3-saida-simulacao-completo/tempo_execucao_bases/NC-tempo_execucao_tamanho_amostral.rds"
)