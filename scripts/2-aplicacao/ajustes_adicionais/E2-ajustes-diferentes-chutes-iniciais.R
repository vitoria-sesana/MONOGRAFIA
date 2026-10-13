# rotina ------------------------------------------------------------------
source("scripts/0-rotina.R", encoding = "UTF-8")

# Chutes iniciais ---------------------------------------------------------

chutes_iniciais <- list(
  chute1  = list(b0 = -0.1, b1 = -0.1, b2 = -0.1, b3 = -0.1),
  chute2  = list(b0 = -0.8, b1 = -0.3, b2 = -0.4, b3 = -0.2),
  chute3  = list(b0 = -1.2, b1 = -0.6, b2 = -0.3, b3 = -0.7),
  chute4  = list(b0 = -0.1, b1 = -0.1, b2 = -0.1, b3 = -0.1),
  chute5  = list(b0 = -0.9, b1 = -0.4, b2 = -0.2, b3 = -0.6),
  chute6  = list(b0 = -1.1, b1 = -0.2, b2 = -0.5, b3 = -0.4),
  chute7  = list(b0 = -0.7, b1 = -0.6, b2 = -0.4, b3 = -0.3),
  chute8  = list(b0 = -1.3, b1 = -0.4, b2 = -0.6, b3 = -0.2),
  chute9  = list(b0 = -0.5, b1 = -0.2, b2 = -0.3, b3 = -0.6),
  chute10 = list(b0 = -5.0, b1 = -3.7, b2 = -7.2, b3 = -9.5)
)


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

tabela_resultados <- data.frame(
  Chute_inicial = names(coeficientes),
  do.call(rbind, coeficientes),
  row.names = NULL,
  check.names = FALSE
)

tabela_resultados[-1] <- lapply(
  tabela_resultados[-1],
  round,
  digits = 6
)

print(tabela_resultados, row.names = FALSE)

