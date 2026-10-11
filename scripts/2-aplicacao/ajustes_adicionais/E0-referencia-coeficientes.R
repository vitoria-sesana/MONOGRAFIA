# rotina ------------------------------------------------------------------
source("scripts/0-rotina.R", encoding = "UTF-8")

# ajuste logbin -----------------------------------------------------------
modelo_logbin_frequentista <-
  readRDS("saidas/1-saida-aplicacao/ajuste_logbin_frequentista.rds")

modelo_logbin_frequentista <- modelo_logbin_frequentista$modelo_logbin_frequentista 

# ajuste poisson ----------------------------------------------------------
modelo_poisson_sandwich <- 
  readRDS("saidas/1-saida-aplicacao/ajuste_poisson_robusto.rds")

modelo_poisson_sandwich <- modelo_poisson_sandwich$modelo_poisson

# ajuste logbinomial bayesiano -----------------------------------------
modelo_logbin_bayesiano <- 
  readRDS("saidas/1-saida-aplicacao/ajuste_logbin_bayesiano.rds")

modelo_logbin_bayesiano <- modelo_logbin_bayesiano$resultado_bayesiano


# avaliando referencias ---------------------------------------------------


modelo_logbin_frequentista
modelo_poisson_sandwich

modelo_poisson_sandwich$xlevels


modelo_logbin_bayesiano
