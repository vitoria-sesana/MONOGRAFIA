# avaliando resultados das estimativas do modelo poisson robusto sob diferentes chutes iniciais

# rotina ------------------------------------------------------------------
source("scripts/0-rotina.R", encoding = "UTF-8")

# Chutes iniciais ---------------------------------------------------------
chutes_iniciais <- readRDS("saidas/1-saida-aplicacao/chutes_iniciais_lista.rds")

# Ajustes -----------------------------------------------------------------


# Ajuste Poisson com variância robusta ------------------------------------

# Ajuste 1
modelo_poisson1 <-
  glm(
    outcome ~ center + treat + baseline,
    family = poisson(link = "log"),
    data = respiratory4,
    start = unlist(chutes_iniciais$chute1)
  )

modelo_sandwich1 <-
  lmtest::coeftest(modelo_poisson1, vcov = sandwich::sandwich)

matriz_covariancia_variancia_sandwich1 <-
  sandwich::sandwich(modelo_poisson1)


# Ajuste 2
modelo_poisson2 <-
  glm(
    outcome ~ center + treat + baseline,
    family = poisson(link = "log"),
    data = respiratory4,
    start = unlist(chutes_iniciais$chute2)
  )

modelo_sandwich2 <-
  lmtest::coeftest(modelo_poisson2, vcov = sandwich::sandwich)

matriz_covariancia_variancia_sandwich2 <-
  sandwich::sandwich(modelo_poisson2)


# Ajuste 3
modelo_poisson3 <-
  glm(
    outcome ~ center + treat + baseline,
    family = poisson(link = "log"),
    data = respiratory4,
    start = unlist(chutes_iniciais$chute3)
  )

modelo_sandwich3 <-
  lmtest::coeftest(modelo_poisson3, vcov = sandwich::sandwich)

matriz_covariancia_variancia_sandwich3 <-
  sandwich::sandwich(modelo_poisson3)


# Ajuste 4
modelo_poisson4 <-
  glm(
    outcome ~ center + treat + baseline,
    family = poisson(link = "log"),
    data = respiratory4,
    start = unlist(chutes_iniciais$chute4)
  )

modelo_sandwich4 <-
  lmtest::coeftest(modelo_poisson4, vcov = sandwich::sandwich)

matriz_covariancia_variancia_sandwich4 <-
  sandwich::sandwich(modelo_poisson4)


# Ajuste 5
modelo_poisson5 <-
  glm(
    outcome ~ center + treat + baseline,
    family = poisson(link = "log"),
    data = respiratory4,
    start = unlist(chutes_iniciais$chute5)
  )

modelo_sandwich5 <-
  lmtest::coeftest(modelo_poisson5, vcov = sandwich::sandwich)

matriz_covariancia_variancia_sandwich5 <-
  sandwich::sandwich(modelo_poisson5)


# Ajuste 6
modelo_poisson6 <-
  glm(
    outcome ~ center + treat + baseline,
    family = poisson(link = "log"),
    data = respiratory4,
    start = unlist(chutes_iniciais$chute6)
  )

modelo_sandwich6 <-
  lmtest::coeftest(modelo_poisson6, vcov = sandwich::sandwich)

matriz_covariancia_variancia_sandwich6 <-
  sandwich::sandwich(modelo_poisson6)


# Ajuste 7
modelo_poisson7 <-
  glm(
    outcome ~ center + treat + baseline,
    family = poisson(link = "log"),
    data = respiratory4,
    start = unlist(chutes_iniciais$chute7)
  )

modelo_sandwich7 <-
  lmtest::coeftest(modelo_poisson7, vcov = sandwich::sandwich)

matriz_covariancia_variancia_sandwich7 <-
  sandwich::sandwich(modelo_poisson7)


# Ajuste 8
modelo_poisson8 <-
  glm(
    outcome ~ center + treat + baseline,
    family = poisson(link = "log"),
    data = respiratory4,
    start = unlist(chutes_iniciais$chute8)
  )

modelo_sandwich8 <-
  lmtest::coeftest(modelo_poisson8, vcov = sandwich::sandwich)

matriz_covariancia_variancia_sandwich8 <-
  sandwich::sandwich(modelo_poisson8)


# Ajuste 9
modelo_poisson9 <-
  glm(
    outcome ~ center + treat + baseline,
    family = poisson(link = "log"),
    data = respiratory4,
    start = unlist(chutes_iniciais$chute9)
  )

modelo_sandwich9 <-
  lmtest::coeftest(modelo_poisson9, vcov = sandwich::sandwich)

matriz_covariancia_variancia_sandwich9 <-
  sandwich::sandwich(modelo_poisson9)


# Ajuste 10
modelo_poisson10 <-
  glm(
    outcome ~ center + treat + baseline,
    family = poisson(link = "log"),
    data = respiratory4,
    start = unlist(chutes_iniciais$chute10)
  )

modelo_sandwich10 <-
  lmtest::coeftest(modelo_poisson10, vcov = sandwich::sandwich)

matriz_covariancia_variancia_sandwich10 <-
  sandwich::sandwich(modelo_poisson10)


# Resultados --------------------------------------------------------------

modelos_sandwich <- list(
  chute1 = modelo_sandwich1,
  chute2 = modelo_sandwich2,
  chute3 = modelo_sandwich3,
  chute4 = modelo_sandwich4,
  chute5 = modelo_sandwich5,
  chute6 = modelo_sandwich6,
  chute7 = modelo_sandwich7,
  chute8 = modelo_sandwich8,
  chute9 = modelo_sandwich9 #, chute10 = modelo_sandwich10
  )


coeficientes <- lapply(modelos_sandwich, coef)

tabela_resultados_estimativas <- data.frame(
  Chute_inicial = names(coeficientes),
  do.call(rbind, coeficientes),
  row.names = NULL,
  check.names = FALSE
)

tabela_resultados_estimativas[-1] <- lapply(
  tabela_resultados_estimativas[-1],
  round,
  digits = 6
)

print(tabela_resultados_estimativas, row.names = FALSE)



# Saídas ------------------------------------------------------------------

saveRDS(tabela_resultados_estimativas, "saidas/1-saida-aplicacao/chutes_iniciais_poisson_robusto_estimativas.rds")

