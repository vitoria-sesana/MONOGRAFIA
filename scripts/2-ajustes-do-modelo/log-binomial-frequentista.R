# Ajuste do modelo log-binomial frequentista

# rotina ------------------------------------------------------------------
source("scripts/0-rotina.R", encoding = "UTF-8")

# ajuste logbin -----------------------------------------------------------
logbin4 <- 
  logbin::logbin(
    outcome ~ center  + treat + baseline, 
    data=respiratory4
  ) 

summary(logbin4)
respiratory4 %>% lapply(class)
model.matrix(logbin4) 

# Referências do modelo:
# center(1 OU 2): 2 é referência, 2 = 0
# treat(A ou P): A é referência, A = 0
# baseline(0 ou 1): 1 é referência, 1 = 0

# outcome(o ou 1): 1 é referencia
# Então o modelo está estimando o RISCO (probabilidade) de um desfecho BOM.

# saídas ------------------------------------------------------------------


