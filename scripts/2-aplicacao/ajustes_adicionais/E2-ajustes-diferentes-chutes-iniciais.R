# rotina ------------------------------------------------------------------
source("scripts/0-rotina.R", encoding = "UTF-8")

# Chutes iniciais ---------------------------------------------------------

chutes_iniciais <- list(
  chute1  = list(b0 = -0.1,  b1 = -0.5,  b2 = -0.5,  b3 = -0.5),
  chute2  = list(b0 = -0.1,  b1 = -0.1,  b2 = -0.1,  b3 = -0.1),
  chute3  = list(b0 = -1.2,  b1 = -0.6,  b2 = -0.3,  b3 = -0.7),
  chute4  = list(b0 = -0.2,  b1 = -0.5,  b2 = -0.8,  b3 = -0.4),
  chute5  = list(b0 = -0.9,  b1 = -1.4,  b2 = -0.2,  b3 = -0.6),
  chute6  = list(b0 = -1.1,  b1 = -0.2,  b2 = -0.5,  b3 = -1.3),
  chute7  = list(b0 = -0.7,  b1 = -1.6,  b2 = -0.4,  b3 = -0.9),
  chute8  = list(b0 = -1.8,  b1 = -0.9,  b2 = -0.2,  b3 = -1.5),
  chute9  = list(b0 = -2.5,  b1 = -0.7,  b2 = -1.3,  b3 = -0.4),
  chute10 = list(b0 = -5.0,  b1 = -3.7,  b2 = -7.2,  b3 = -9.5)
)

tabela_chutes <- do.call(
  rbind,
  lapply(names(chutes_iniciais), function(nome) {
    data.frame(
      Chute = nome,
      b0 = chutes_iniciais[[nome]]$b0,
      b1 = chutes_iniciais[[nome]]$b1,
      b2 = chutes_iniciais[[nome]]$b2,
      b3 = chutes_iniciais[[nome]]$b3
    )
  })
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


# saídas ------------------------------------------------------------------

tabela_chutes
tabela_resultados

# csv
write.csv(
  tabela_chutes,
  file = "saidas/1-saida-aplicacao/tabela_chutes_iniciais.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)


write.csv(
  tabela_resultados,
  file = "saidas/1-saida-aplicacao/tabela_estimativas_pontuais.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# latex
kbl(
  tabela_chutes,
  format = "latex",
  booktabs = TRUE,
  caption = "Tabela de resultados",
  label = "tab:resultados",
  align = "lcccccccc"
)

kbl(
  tabela_resultados,
  format = "latex",
  booktabs = TRUE,
  caption = "Tabela de resultados",
  label = "tab:resultados",
  align = "lcccccccc"
)

