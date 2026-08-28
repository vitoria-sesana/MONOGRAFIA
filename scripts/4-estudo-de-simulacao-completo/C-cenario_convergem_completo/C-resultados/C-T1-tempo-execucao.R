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
C_tempo_logbin_freq <- 
  "saidas/3-saida-simulacao-completo/C-saidas/C-M1-logbin-frequentista/C-tempo_execucao_logbin_frequentista.rds" %>% 
  readRDS() %>% 
  formatar_tempo()


C_tempo_poisson_robusto <- 
  "saidas/3-saida-simulacao-completo/C-saidas/C-M2-poisson-robusto/C-tempo_execucao_ajustes_poisson_robusto.rds" %>% 
  readRDS() %>% 
  formatar_tempo()


C_tempo_logbin_bayes <- 
  "saidas/3-saida-simulacao-completo/C-saidas/C-M3-logbin-bayesiano/C-tempo_execucao_ajustes_logbin_bayesiano.rds" %>% 
  readRDS() %>% 
  formatar_tempo()


# tabela ------------------------------------------------------------------

C_tabela_tempo <- data.frame(
  modelo = c(
    "Log-binomial frequentista",
    "Poisson robusto",
    "Log-binomial bayesiano"
  ),
  valor = c(
    C_tempo_logbin_freq,
    C_tempo_poisson_robusto,
    C_tempo_logbin_bayes
  )
)

C_tabela_tempo
