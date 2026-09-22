
l = readRDS("saidas/3-saida-simulacao-completo/NC-saidas/NC-M1-logbin-frequentista/NC-ajustes_logbin_frequentista.rds")

modelo_logbin_frequentista = l$amostra_50_replica_15

k = readRDS("saidas/3-saida-simulacao-completo/C-saidas/C-M1-logbin-frequentista/C-ajustes_logbin_frequentista.rds")

modelo_logbin_frequentista = k$amostra_50_replica_15


beta <- coef(modelo_logbin_frequentista)

X <- model.matrix(modelo_logbin_frequentista)

p <- predict(modelo_logbin_frequentista, type = "response")

y <- model.response(model.frame(modelo_logbin_frequentista))

gradiente <- t(X) %*% ((y - p) / (1 - p))

gradiente
