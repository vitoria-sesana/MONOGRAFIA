# rotina ------------------------------------------------------------------
source("scripts/0-rotina.R", encoding = "UTF-8")


# função auxiliar ---------------------------------------------------------
formatar_tempo <- function(x) {
  segundos <- as.numeric(x, units = "secs")
  
  sprintf(
    "%02d:%02d:%02d",
    floor(segundos / 3600),
    floor((segundos %% 3600) / 60),
    floor(segundos %% 60)
  )
}

# leitura -----------------------------------------------------------------
NC_tempo_logbin_freq <- 
  "saidas/3-saida-simulacao-completo/NC-saidas/NC-M1-logbin-frequentista/NC-tempo_execucao_logbin_frequentista.rds" %>% 
  readRDS() %>% 
  formatar_tempo()


NC_tempo_poisson_robusto <- 
  "saidas/3-saida-simulacao-completo/NC-saidas/NC-M2-poisson-robusto/NC-tempo_execucao_ajustes_poisson_robusto.rds" %>% 
  readRDS() %>% 
  formatar_tempo()


NC_tempo_logbin_bayes <- 
  "saidas/3-saida-simulacao-completo/NC-saidas/NC-M3-logbin-bayesiano/NC-tempo_execucao_ajustes_logbin_bayesiano.rds" %>% 
  readRDS() %>% 
  formatar_tempo()


# tabela ------------------------------------------------------------------

NC_tabela_tempo <- data.frame(
  modelo = c(
    "Log-binomial frequentista",
    "Poisson robusto",
    "Log-binomial bayesiano"
  ),
  valor = c(
    NC_tempo_logbin_freq,
    NC_tempo_poisson_robusto,
    NC_tempo_logbin_bayes
  )
)

NC_tabela_tempo
