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
tempo_logbin_freq <- 
  "saidas/2-saida-simulacao/M1-logbin-frequentista/tempo_execucao_logbin_frequentista.rds" %>% 
  readRDS() %>% 
  formatar_tempo()


tempo_poisson_robusto <- 
  "saidas/2-saida-simulacao/M2-poisson-robusto/tempo_execucao_ajustes_poisson_robusto.rds" %>% 
  readRDS() %>% 
  formatar_tempo()


tempo_logbin_bayes <- 
  "saidas/2-saida-simulacao/M3-logbin-bayesiano/tempo_execucao_ajustes_logbin_bayesiano.rds" %>% 
  readRDS() %>% 
  formatar_tempo()


# tabela ------------------------------------------------------------------

tabela_tempo <- data.frame(
  modelo = c(
    "Log-binomial frequentista",
    "Poisson robusto",
    "Log-binomial bayesiano"
  ),
  valor = c(
    tempo_logbin_freq,
    tempo_poisson_robusto,
    tempo_logbin_bayes
  )
)

tabela_tempo

# saída latex -------------------------------------------------------------
kbl(tabela_tempo, format = "latex", booktabs = TRUE, align = "crr",
    caption = "Tabela tempo de execução dos ajustes de cada modelo.") %>%
  kable_styling(latex_options = c("hold_position", "striped"))
