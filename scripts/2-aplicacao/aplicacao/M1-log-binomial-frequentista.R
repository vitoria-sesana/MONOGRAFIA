# Ajuste do modelo log-binomial frequentista

# rotina ------------------------------------------------------------------
source("scripts/0-rotina.R", encoding = "UTF-8")

# ajuste logbin -----------------------------------------------------------
modelo_logbin_frequentista <- 
  logbin::logbin(
    outcome ~ center  + treat + baseline, 
    data=respiratory4
  ) 

summary(modelo_logbin_frequentista)
respiratory4 %>% lapply(class)
model.matrix(modelo_logbin_frequentista) 

# Referências do modelo:
# center(1 OU 2): 2 é referência, 2 = 0
# treat(A ou P): A é referência, A = 0
# baseline(0 ou 1): 1 é referência, 1 = 0

# outcome(o ou 1): 1 é referencia
# Então o modelo está estimando o RISCO (probabilidade) de um desfecho BOM.

# saídas ------------------------------------------------------------------
rm(respiratory4)
resultados_logbin_frequentista <- as.list(environment()) 
saveRDS(resultados_logbin_frequentista, "saidas/1-saida-ajustes/ajuste_logbin_frequentista.rds")




# verificando os chutes iniciais ------------------------------------------

modelo_logbin_frequentista1 <- 
  logbin::logbin(
    outcome ~ center  + treat + baseline, 
    data=respiratory4, start = list(b0=-1, b1=-0.5, b2=-0.5, b3=-0.5)
  ) 

summary(modelo_logbin_frequentista1)$coefficients


modelo_logbin_frequentista2 <- 
  logbin::logbin(
    outcome ~ center  + treat + baseline, 
    data=respiratory4, start = list(b0=-0.8, b1=-0.3, b2=-0.4, b3=-0.2)
  ) 

summary(modelo_logbin_frequentista2)$coefficients


modelo_logbin_frequentista3 <- 
  logbin::logbin(
    outcome ~ center  + treat + baseline, 
    data=respiratory4, start = list(b0=-1.2, b1=-0.6, b2=-0.3, b3=-0.7)
  ) 

summary(modelo_logbin_frequentista3)$coefficients



modelo_logbin_frequentista4 <- 
  logbin::logbin(
    outcome ~ center  + treat + baseline, 
    data=respiratory4, start = list(b0=-0.1, b1=-0.1, b2=-0.1, b3=-0.1)
  ) 

summary(modelo_logbin_frequentista4)$coefficients
