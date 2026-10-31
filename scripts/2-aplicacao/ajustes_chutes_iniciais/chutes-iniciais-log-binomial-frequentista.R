# avaliando resultados das estimativas do modelo logbin frequentista sob diferentes chutes iniciais

# rotina ------------------------------------------------------------------
source("scripts/0-rotina.R", encoding = "UTF-8")

# Chutes iniciais ---------------------------------------------------------
chutes_iniciais <- readRDS("saidas/1-saida-aplicacao/chutes_iniciais_lista.rds")


# Ajustes -----------------------------------------------------------------


# Ajuste 1
modelo_logbin_frequentista1 <-
  logbin::logbin(
    outcome ~ center + treat + baseline,
    data = respiratory4,
    start = chutes_iniciais$chute1
  )

# Ajuste 2
modelo_logbin_frequentista2 <-
  logbin::logbin(
    outcome ~ center + treat + baseline,
    data = respiratory4,
    start = chutes_iniciais$chute2
  )

# Ajuste 3
modelo_logbin_frequentista3 <-
  logbin::logbin(
    outcome ~ center + treat + baseline,
    data = respiratory4,
    start = chutes_iniciais$chute3
  )

# Ajuste 4
modelo_logbin_frequentista4 <-
  logbin::logbin(
    outcome ~ center + treat + baseline,
    data = respiratory4,
    start = chutes_iniciais$chute4
  )

# Ajuste 5
modelo_logbin_frequentista5 <-
  logbin::logbin(
    outcome ~ center + treat + baseline,
    data = respiratory4,
    start = chutes_iniciais$chute5
  )

# Ajuste 6
modelo_logbin_frequentista6 <-
  logbin::logbin(
    outcome ~ center + treat + baseline,
    data = respiratory4,
    start = chutes_iniciais$chute6
  )

# Ajuste 7
modelo_logbin_frequentista7 <-
  logbin::logbin(
    outcome ~ center + treat + baseline,
    data = respiratory4,
    start = chutes_iniciais$chute7
  )

# Ajuste 8
modelo_logbin_frequentista8 <-
  logbin::logbin(
    outcome ~ center + treat + baseline,
    data = respiratory4,
    start = chutes_iniciais$chute8
  )

# Ajuste 9
modelo_logbin_frequentista9 <-
  logbin::logbin(
    outcome ~ center + treat + baseline,
    data = respiratory4,
    start = chutes_iniciais$chute9
  )

# Ajuste 10
modelo_logbin_frequentista10 <-
  logbin::logbin(
    outcome ~ center + treat + baseline,
    data = respiratory4,
    start = chutes_iniciais$chute10
  )


# Resultados --------------------------------------------------------------

modelos <- list(
  chute1 = modelo_logbin_frequentista1,
  chute2 = modelo_logbin_frequentista2,
  chute3 = modelo_logbin_frequentista3,
  chute4 = modelo_logbin_frequentista4,
  chute5 = modelo_logbin_frequentista5,
  chute6 = modelo_logbin_frequentista6,
  chute7 = modelo_logbin_frequentista7,
  chute8 = modelo_logbin_frequentista8,
  chute9 = modelo_logbin_frequentista9,
  chute10 = modelo_logbin_frequentista10
)

coeficientes <- lapply(modelos, function(x) x$coefficients)

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

saveRDS(tabela_resultados_estimativas, "saidas/1-saida-aplicacao/chutes_iniciais_log_binomial_frequentista_estimativas.rds")
