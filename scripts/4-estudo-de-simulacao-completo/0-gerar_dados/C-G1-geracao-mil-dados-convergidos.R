rm(list = ls())

# função gerar bases convergidas -------------------------------------------

func_gerar_mil_bases_conv <- function(replicas, amostras, n_clusters = NULL) {
  # replicas: número de réplicas por tamanho
  # amostras: vetor com tamanhos amostrais
  # n_clusters: número de clusters (se NULL, assume cada linha como 1 cluster)
  suppressMessages(require(dplyr))
  
  bases_conv        <- list()
  modelo_lgbin_conv <- list()
  lista_tentativas  <- list()   # uma linha por TENTATIVA (convergida ou não)
  
  for (i in seq_along(amostras)) {
    n_obs <- amostras[i]
    bases_replicas_conv   <- list()
    modelos_replicas_conv <- list()
    contagem_conv      <- 1
    tentativas_replica <- 0
    
    while (contagem_conv <= replicas) {
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
      
      index_rep <- paste0("amostra_", n_obs, "_replica_", contagem_conv)
      
      ## ajuste do modelo
      modelo_logbin_frequentista <- 
        logbin::logbin(
          y ~ center + treat + baseline,
          data = dado_simulado_n
        )
      
      convergiu <- sum(is.na(vcov(modelo_logbin_frequentista))) == 0
      
      # tempo desta tentativa (geração + ajuste + checagem)
      tempo_tentativa <- as.numeric(difftime(Sys.time(), t_ini, units = "secs"))
      
      # registra TODA tentativa, convergindo ou não
      lista_tentativas[[length(lista_tentativas) + 1]] <- data.frame(
        base       = index_rep,
        n_amostral = n_obs,
        replica    = contagem_conv,
        tentativa  = tentativas_replica,
        convergiu  = convergiu,
        tempo_seg  = tempo_tentativa
      )
      
      if (convergiu) {
        bases_replicas_conv[[index_rep]]   <- dado_simulado_n
        modelos_replicas_conv[[index_rep]] <- modelo_logbin_frequentista
        
        contagem_conv      <- contagem_conv + 1
        tentativas_replica <- 0
        
        cat("n =", n_obs, "| bases convergidas:", contagem_conv - 1, "\n")
      }
    }
    
    bases_conv        <- c(bases_conv, bases_replicas_conv)
    modelo_lgbin_conv <- c(modelo_lgbin_conv, modelos_replicas_conv)
  }
  
  # 1) tabela por tentativa ------------------------------------------------
  tabela_tempo_tentativa <- dplyr::bind_rows(lista_tentativas) |>
    dplyr::mutate(
      status = ifelse(convergiu, "convergiu", "não convergiu")
    )
  rownames(tabela_tempo_tentativa) <- NULL
  
  # 2) tabela por réplica --------------------------------------------------
  tabela_tempo_base <- tabela_tempo_tentativa |>
    dplyr::group_by(base, n_amostral, replica) |>
    dplyr::summarise(
      n_tentativas               = dplyr::n(),
      n_nao_convergidas          = sum(!convergiu),
      tempo_tentativa_convergida = sum(tempo_seg[convergiu]),   # tempo da réplica que funcionou
      tempo_nao_convergidas      = sum(tempo_seg[!convergiu]),  # tempo gasto nas falhas
      tempo_ate_convergir        = sum(tempo_seg),              # tempo total até achar uma que funcione
      .groups = "drop"
    )
  
  # 3) tabela por tamanho amostral ----------------------------------------
  tabela_tempo_total <- tabela_tempo_tentativa |>
    dplyr::group_by(n_amostral) |>
    dplyr::summarise(
      n_tentativas              = dplyr::n(),
      n_convergidas             = sum(convergiu),
      n_nao_convergidas         = sum(!convergiu),
      taxa_convergencia         = mean(convergiu),
      tempo_convergidas_seg     = sum(tempo_seg[convergiu]),
      tempo_nao_convergidas_seg = sum(tempo_seg[!convergiu]),
      tempo_total_seg           = sum(tempo_seg),
      tempo_total_min           = sum(tempo_seg) / 60,
      tempo_medio_tentativa_seg = mean(tempo_seg),
      tempo_medio_replica_seg   = sum(tempo_seg) / sum(convergiu),  # custo médio por réplica válida
      .groups = "drop"
    )
  
  list(
    bases_conv             = bases_conv,
    modelo_lgbin_conv      = modelo_lgbin_conv,
    tabela_tempo_tentativa = tabela_tempo_tentativa,
    tabela_tempo_base      = tabela_tempo_base,
    tabela_tempo_total     = tabela_tempo_total
  )
}


# gerando base de dados simulada ------------------------------------------
set.seed(34)
tempo_inicial_c <- Sys.time()

dados_e_modelos_gerados_conv <- 
  func_gerar_mil_bases_conv(
    replicas   = 1000, 
    amostras   = c(50, 100, 200), 
    n_clusters = NULL
  )

tempo_final_c    <- Sys.time()
tempo_execucao_c <- as.numeric(difftime(tempo_final_c, tempo_inicial_c, units = "secs"))
tempo_execucao_c   


# resultados --------------------------------------------------------------
dados_gerados_convergidos       <- dados_e_modelos_gerados_conv$bases_conv
modelos_logbin_freq_convergidos <- dados_e_modelos_gerados_conv$modelo_lgbin_conv
tabela_tempo_tentativa          <- dados_e_modelos_gerados_conv$tabela_tempo_tentativa
tabela_tempo_replica            <- dados_e_modelos_gerados_conv$tabela_tempo_base
tabela_tempo_tamanho_amostral   <- dados_e_modelos_gerados_conv$tabela_tempo_total


# validação ----------------------------------------------------------------
length(dados_gerados_convergidos)             
table(tabela_tempo_replica$n_amostral)           
table(tabela_tempo_tentativa$n_amostral, tabela_tempo_tentativa$status)
sum(tabela_tempo_tamanho_amostral$tempo_total_seg)     

dados_gerados_convergidos$amostra_50_replica_1
lapply(modelos_logbin_freq_convergidos, vcov)


# saída ------------------------------------------------------------------

saveRDS(
  dados_gerados_convergidos,
  file = "saidas/3-saida-simulacao-completo/C-dados_convergidos_gerados.rds"
)

saveRDS(
  tabela_tempo_tentativa,
  file = "saidas/3-saida-simulacao-completo/tempo_execucao_bases/C-tempo_execucao_tentativa.rds"
)

saveRDS(
  tabela_tempo_replica,
  file = "saidas/3-saida-simulacao-completo/tempo_execucao_bases/C-tempo_execucao_replica.rds"
)

saveRDS(
  tabela_tempo_tamanho_amostral,
  file = "saidas/3-saida-simulacao-completo/tempo_execucao_bases/C-tempo_execucao_tamanho_amostral.rds"
)