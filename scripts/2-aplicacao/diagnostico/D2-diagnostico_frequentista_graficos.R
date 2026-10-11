# alavanca, resíduos e distância de cook

# rotina ------------------------------------------------------------------
source("scripts/0-rotina.R", encoding = "UTF-8")

# ajuste logbin -----------------------------------------------------------
modelo_logbin_freq <- readRDS("saidas/1-saida-aplicacao/ajuste_logbin_frequentista.rds")
modelo_logbin_freq <- modelo_logbin_freq$modelo_logbin_frequentista
sum_logbin <- summary(modelo_logbin_freq)


# ajuste poisson ------------------------------------
resultado_poisson <- readRDS("saidas/1-saida-aplicacao/ajuste_poisson_robusto.rds")
modelo_poisson <- resultado_poisson$modelo_poisson

# ajuste poisson sandwich ------------------------------------
modelo_sandwich <- resultado_poisson$modelo_sandwich
matriz_covariancia_variancia_sandwich <- resultado_poisson$matriz_covariancia_variancia_sandwich

# função calcular diagnosticos --------------------------------------------

diagnosticos_glm <- function(modelo, nome_modelo) {
  fitted_vals <- if (!is.null(modelo$fitted.values)) modelo$fitted.values else fitted(modelo)
  pearson_r   <- residuals(modelo, type = "pearson")
  dev_r       <- residuals(modelo, type = "deviance")
  
  n <- length(fitted_vals)
  
  lev <- tryCatch(hatvalues(modelo), error = function(e) rep(NA, n))
  cook <- tryCatch(cooks.distance(modelo), error = function(e) rep(NA, n))
  
  data.frame(
    obs = 1:n,
    fitted = fitted_vals,
    pearson_resid = pearson_r,
    deviance_resid = dev_r,
    leverage = lev,
    cookd = cook,
    modelo = nome_modelo
  )
}


# função plotar graficos --------------------------------------------------

criar_graficos <- function(diag_data) {
  
  # 1. Alavancagem vs observações ===============
  g1 <- ggplot(diag_data, aes(x = obs, y = leverage)) +
    geom_point(color = "purple") +
    labs(x = "Observação", y = "Alavanca") +
    theme_minimal() +
    theme(
      title = element_blank(),
      panel.border = element_rect(color = "grey70", fill = NA, size = 0.5),
      axis.text.x   = element_text(color = "grey20",size = 7),
      axis.text.y   = element_text(color = "grey20",size = 7),
      axis.title.x  = element_text(color = "grey20",face = "bold"),
      axis.title.y  = element_text(color = "grey20",face = "bold")
    ) +
    scale_x_continuous(limits = c(0, 120), breaks = seq(0, 120, by = 20)) +
    scale_y_continuous(limits = c(0, 0.08), breaks = seq(0, 0.08, by = 0.01)) 
  
  # 2. Distância de Cook vs observações  ===============
  g2 <- ggplot(diag_data, aes(x = obs, y = cookd)) +
    geom_point(color="darkgreen") +
    geom_text(aes(label = ifelse(cookd > 4/nrow(diag_data), obs, "")), vjust = -0.5, size = 2) +
    labs(x = "Observação", y = "Distância de Cook") + # Corrigido pequeno typo "Distrância"
    theme_minimal() +
    theme(
      title = element_blank(),
      panel.border = element_rect(color = "grey70", fill = NA, size = 0.5),
      axis.text.x   = element_text(color = "grey20",size = 7),
      axis.text.y   = element_text(color = "grey20",size = 7),
      axis.title.x  = element_text(color = "grey20",face = "bold"),
      axis.title.y  = element_text(color = "grey20",face = "bold")
    ) +
    scale_x_continuous(limits = c(0, 120), breaks = seq(0, 120, by = 20)) +
    scale_y_continuous(limits = c(-0.1, 1.3), breaks = seq(-0.1, 1.3, by = 0.1)) 
  
  # 3. Resíduos de Pearson vs valores ajustados  ===============
  g3 <- ggplot(diag_data, aes(x = fitted, y = pearson_resid)) +
    geom_point(color = "orange") +
    geom_hline(yintercept = 0, linetype="dashed") +
    labs(x = "Valores ajustados", y = "Resíduos de Pearson") +
    theme_minimal() +
    theme(
      title = element_blank(),
      panel.border = element_rect(color = "grey70", fill = NA, size = 0.5),
      axis.text.x   = element_text(color = "grey20",size = 7),
      axis.text.y   = element_text(color = "grey20",size = 7),
      axis.title.x  = element_text(color = "grey20",face = "bold"),
      axis.title.y  = element_text(color = "grey20",face = "bold")
    ) +
    scale_y_continuous(limits = c(-14, 5), breaks =seq(-14, 5, by = 2)) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) 
  
  # 4. Resíduos Deviance vs valores ajustados ===============
  g4 <- ggplot(diag_data, aes(x = fitted, y = deviance_resid)) +
    geom_point(color = "orange") + # Mudei para azul para diferenciar visualmente do Pearson
    geom_hline(yintercept = 0, linetype="dashed") +
    labs(x = "Valores ajustados", y = "Resíduos Deviance") +
    theme_minimal() +
    theme(
      title = element_blank(),
      panel.border = element_rect(color = "grey70", fill = NA, size = 0.5),
      axis.text.x   = element_text(color = "grey20",size = 7),
      axis.text.y   = element_text(color = "grey20",size = 7),
      axis.title.x  = element_text(color = "grey20",face = "bold"),
      axis.title.y  = element_text(color = "grey20",face = "bold")
    ) +
    scale_y_continuous(limits = c(-14, 5), breaks =seq(-14, 5, by = 2)) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2))
  
  # Retorna todos os gráficos organizados em uma lista nomeada
  return(list(
    alavancagem = g1, 
    cook = g2, 
    pearson = g3, 
    deviance = g4
  ))
}

# função diagnostico poisson robusto --------------------------------------

diagnosticos_poisson_robusto <- function(modelo_poisson, matriz_covariancia_variancia_sandwich, dados) {
  library(tibble)
  
  X <- model.matrix(modelo_poisson)
  mu <- fitted(modelo_poisson)
  y <- dados$outcome
  p <- ncol(X)
  n <- nrow(X)
  
  # matriz de pesos W para Poisson
  W <- diag(mu)
  
  # alavanca h_ii
  H <- X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
  leverages <- diag(H)
  
  # resíduos de Pearson
  residuos_pearson <- (y - mu) / sqrt(mu)  # padrão Poisson
  
  # resíduos de Pearson com sandwich
  var_robusta <- diag(X %*% matriz_covariancia_variancia_sandwich %*% t(X))
  residuos_pearson_sandwich <- (y - mu) / sqrt(var_robusta)
  
  # resíduos de Deviance padrão para Poisson
  residuos_deviance <- residuals(modelo_poisson, type = "deviance")
  
  # resíduos de Deviance com correção Sandwich
  residuos_deviance_sandwich <- residuos_deviance * sqrt(mu / var_robusta)
  
  # Distâncias de Cook (Pearson e Deviance)
  cook <- (residuos_pearson^2 * leverages) / (p * (1 - leverages)^2)
  cook_sandwich <- (residuos_pearson_sandwich^2 * leverages) / (p * (1 - leverages)^2)
  
  cook_deviance <- (residuos_deviance^2 * leverages) / (p * (1 - leverages)^2)
  cook_deviance_sandwich <- (residuos_deviance_sandwich^2 * leverages) / (p * (1 - leverages)^2)
  
  # criar dataframe para ggplot atualizado
  df_plot <- tibble(
    obs = 1:n,
    leverage = leverages,
    pearson = residuos_pearson,
    pearson_sandwich = residuos_pearson_sandwich,
    deviance = residuos_deviance,
    deviance_sandwich = residuos_deviance_sandwich,
    cook = cook,
    cook_sandwich = cook_sandwich,
    cook_deviance = cook_deviance,
    cook_deviance_sandwich = cook_deviance_sandwich,
    fitted = mu
  )
  
  return(df_plot)
}


# função graficos poisson robusto -----------------------------------------

criar_graficos_poisson_robusto <- function(diag_data) {
  
  # 1. Alavancagem vs observações ===============
  g1 <- ggplot(diag_data, aes(x = obs, y = leverage)) +
    geom_point(color = "purple") +
    labs(x = "Observação", y = "Alavanca") +
    theme_minimal() +
    theme(
      title = element_blank(),
      panel.border = element_rect(color = "grey70", fill = NA, size = 0.5),
      axis.text.x   = element_text(color = "grey20", size = 7),
      axis.text.y   = element_text(color = "grey20", size = 7),
      axis.title.x  = element_text(color = "grey20", face = "bold"),
      axis.title.y  = element_text(color = "grey20", face = "bold")
    ) +
    scale_x_continuous(limits = c(0, 120), breaks = seq(0, 120, by = 20)) +
    scale_y_continuous(limits = c(0, 0.08), breaks = seq(0, 0.08, by = 0.01)) 
  
  # 2. Distância de Cook (Sandwich) vs observações ===============
  g2 <- ggplot(diag_data, aes(x = obs, y = cook_sandwich)) +
    geom_point(color = "darkgreen") +
    geom_text(aes(label = ifelse(cook_sandwich > 4/nrow(diag_data), obs, "")), vjust = -0.5, size = 2) +
    labs(x = "Observação", y = "Distância de Cook") + 
    theme_minimal() +
    theme(
      title = element_blank(),
      panel.border = element_rect(color = "grey70", fill = NA, size = 0.5),
      axis.text.x   = element_text(color = "grey20", size = 7),
      axis.text.y   = element_text(color = "grey20", size = 7),
      axis.title.x  = element_text(color = "grey20", face = "bold"),
      axis.title.y  = element_text(color = "grey20", face = "bold")
    ) +
    scale_x_continuous(limits = c(0, 120), breaks = seq(0, 120, by = 20)) +
    scale_y_continuous(limits = c(-0.1, 1.3), breaks = seq(-0.1, 1.3, by = 0.1)) 
  
  # 3. Resíduos de Pearson (Sandwich) vs valores ajustados ===============
  g3 <- ggplot(diag_data, aes(x = fitted, y = pearson_sandwich)) +
    geom_point(color = "orange") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x = "Valores ajustados", y = "Resíduos de Pearson") +
    theme_minimal() +
    theme(
      title = element_blank(),
      panel.border = element_rect(color = "grey70", fill = NA, size = 0.5),
      axis.text.x   = element_text(color = "grey20", size = 7),
      axis.text.y   = element_text(color = "grey20", size = 7),
      axis.title.x  = element_text(color = "grey20", face = "bold"),
      axis.title.y  = element_text(color = "grey20", face = "bold")
    ) +
    scale_y_continuous(limits = c(-14, 5), breaks =seq(-14, 5, by = 2)) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) 
  
  # 4. Resíduos Deviance (Sandwich) vs valores ajustados ===============
  g4 <- ggplot(diag_data, aes(x = fitted, y = deviance_sandwich)) +
    geom_point(color = "orange") + 
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x = "Valores ajustados", y = "Resíduos Deviance") +
    theme_minimal() +
    theme(
      title = element_blank(),
      panel.border = element_rect(color = "grey70", fill = NA, size = 0.5),
      axis.text.x   = element_text(color = "grey20", size = 7),
      axis.text.y   = element_text(color = "grey20", size = 7),
      axis.title.x  = element_text(color = "grey20", face = "bold"),
      axis.title.y  = element_text(color = "grey20", face = "bold")
    ) +
    scale_y_continuous(limits = c(-14, 5), breaks =seq(-14, 5, by = 2)) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2))
  
  # Retorna todos os gráficos organizados em uma lista nomeada
  return(list(
    alavancagem = g1, 
    cook = g2, 
    pearson = g3, 
    deviance = g4
  ))
}

# Resultados --------------------------------------------------------------
# diagnosticos poisson e logbinomial --------------------------------------
diag_poisson <- diagnosticos_glm(modelo_poisson, "Poisson")
diag_logbin <- diagnosticos_glm(modelo_logbin_freq, "Log-binomial")
diag_poisson_robusto <- 
  diagnosticos_poisson_robusto(
    modelo_poisson = modelo_poisson, 
    matriz_covariancia_variancia_sandwich = matriz_covariancia_variancia_sandwich, 
    dados = respiratory4
  )

graficos_poisson <- criar_graficos(diag_poisson)
graficos_logbin <- criar_graficos(diag_logbin)
graficos_poisson_robusto <- criar_graficos_poisson_robusto(diag_poisson_robusto)

graficos_logbin$pearson
graficos_logbin$deviance
graficos_poisson$pearson
graficos_poisson$deviance
graficos_poisson_robusto$pearson
graficos_poisson_robusto$deviance



# Observações discrepantes ------------------------------------------------

respiratory4[c(81,107),] # outliers detectados
respiratory4[c(20,33,54,62,74,91,102,104),] # outliers detectados apenas no poisson robusto

# saídas ------------------------------------------------------------------

## alavanca ----------
ggsave(
  "plots/2-plots-aplicacao/alavanca_logbin.pdf", 
  graficos_logbin$alavancagem,
  width = 14,
  height = 12,
  units = "cm"
)

ggsave(
  "plots/2-plots-aplicacao/alavanca_poisson.pdf", 
  graficos_poisson$alavancagem, 
  width = 14,
  height = 12,
  units = "cm"
)


## resíduo pearson ----------

ggsave(
  "plots/2-plots-aplicacao/residuo_logbin.pdf", 
  graficos_logbin$pearson, 
  width = 14,
  height = 12,
  units = "cm"
)

ggsave(
  "plots/2-plots-aplicacao/residuo_poisson.pdf", 
  graficos_poisson$pearson, 
  width = 14,
  height = 12,
  units = "cm"
)

ggsave(
  "plots/2-plots-aplicacao/residuo_poisson_sandwich.pdf", 
  graficos_poisson_robusto$pearson, 
  width = 14,
  height = 12,
  units = "cm"
)

## resíduo deviance ----------

ggsave(
  "plots/2-plots-aplicacao/residuo_deviance_logbin.pdf", 
  graficos_logbin$deviance, 
  width = 14,
  height = 12,
  units = "cm"
)

ggsave(
  "plots/2-plots-aplicacao/residuo_deviance_poisson.pdf", 
  graficos_poisson$deviance, 
  width = 14,
  height = 12,
  units = "cm"
)

ggsave(
  "plots/2-plots-aplicacao/residuo_deviance_poisson_sandwich.pdf", 
  graficos_poisson_robusto$deviance, 
  width = 14,
  height = 12,
  units = "cm"
)

## cook ----------
ggsave(
  "plots/2-plots-aplicacao/distancia_cook_logbin.pdf", 
  graficos_logbin$cook, 
  width = 14,
  height = 12,
  units = "cm"
)

ggsave(
  "plots/2-plots-aplicacao/distancia_cook_poisson.pdf", 
  graficos_poisson$cook, 
  width = 14,
  height = 12,
  units = "cm"
)

ggsave(
  "plots/2-plots-aplicacao/distancia_cook_poisson_sandwich.pdf", 
  graficos_poisson_robusto$cook, 
  width = 14,
  height = 12,
  units = "cm"
)

