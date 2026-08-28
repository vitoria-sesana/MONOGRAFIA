rm(list = ls())

# leitura -----------------------------------------------------------------
dados_gerados <- readRDS("saidas/2-saida-simulacao/dados_gerados.rds")

# ajuste sandwich ---------------------------------------------------------
tempo_inicial_poisson <- Sys.time()

modelos_poisson_classico <- 
  purrr::map(
    dados_gerados, 
    ~ glm(
      y ~ center + treat + baseline,
      family = poisson(link=log), 
      data = .x
    )
  )

modelos_poisson_robusto <- 
  purrr::map(
    modelos_poisson_classico, 
    ~ lmtest::coeftest(.x, vcov = sandwich::sandwich)
  )

tempo_final_poisson  <- Sys.time()
tempo_execucao_poisson <- tempo_final_poisson  - tempo_inicial_poisson 
tempo_execucao_poisson 


## saida -------------------------------------------------------------------
# saveRDS(
#   modelos_poisson_classico, # modelo Poisson clássico
#   file = "saidas/2-saida-simulacao/M2-poisson-robusto/ajustes_poisson_classico.rds"
# )
# 
# saveRDS(
#   modelos_poisson_robusto, # modelo Poisson robusto
#   file = "saidas/2-saida-simulacao/M2-poisson-robusto/ajustes_poisson_robusto.rds"
# )
# 
# saveRDS(
#   tempo_execucao_poisson, # tempode de execução modelo Poisson robusto
#   file = "saidas/2-saida-simulacao/M2-poisson-robusto/tempo_execucao_ajustes_poisson_robusto.rds"
# )