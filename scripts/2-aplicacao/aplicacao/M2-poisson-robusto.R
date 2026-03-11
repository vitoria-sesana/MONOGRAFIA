# Ajuste do modelo poisson com variância robusta

# rotina ------------------------------------------------------------------
source("scripts/0-rotina.R", encoding = "UTF-8")

# ajuste poisson com variância robusta ------------------------------------
modelo_poisson <- 
  glm(
    outcome ~ center  + treat + baseline,
    family = poisson(link=log), 
    data = respiratory4
  )

modelo_sandwich <- 
  lmtest::coeftest(modelo_poisson, vcov = sandwich::sandwich)

matriz_covariancia_variancia_sandwich <- 
  sandwich::sandwich(modelo_poisson)

# analises ----------------------------------------------------------------

# variaveis significantes pro modelo
summary(modelo_poisson)
modelo_sandwich

# matriz de variancia e covariancia
vcov(modelo_poisson)
matriz_covariancia_variancia_sandwich

# saídas -------------------------------------------------------------------
rm(respiratory4)
resultados_poisson_robusto <- as.list(environment()) 
saveRDS(resultados_poisson_robusto, "saidas/1-saida-ajustes/ajuste_poisson_robusto.rds")
