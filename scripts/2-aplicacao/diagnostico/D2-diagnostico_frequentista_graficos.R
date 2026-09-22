# alavanca, resíduos e distância de cook

# rotina ------------------------------------------------------------------
source("scripts/0-rotina.R", encoding = "UTF-8")

# ajuste logbin -----------------------------------------------------------
resultado_poisson <- readRDS("saidas/1-saida-aplicacao/ajuste_logbin_frequentista.rds")

sum_logbin <- summary(mod_lb)


# ajuste poisson ------------------------------------
resultado_poisson <- readRDS("saidas/1-saida-aplicacao/ajuste_poisson_robusto.rds")
modelo_poisson <- resultado_poisson$modelo_poisson

# ajuste poisson sandwich ------------------------------------
modelo_sandwich <- resultado_poisson$modelo_sandwich
matriz_covariancia_variancia_sandwich <- resultado_poisson$matriz_covariancia_variancia_sandwich



# diagnósticos ------------------------------------------------------------

sum_logbin
summary(modelo_poisson)
modelo_sandwich 
matriz_covariancia_variancia_sandwich

diagnosticos_glm <- function(modelo, nome_modelo) {
  data.frame(
    obs = 1:length(modelo$fitted.values),
    fitted = modelo$fitted.values,
    pearson_resid = residuals(modelo, type = "pearson"),
    leverage = hatvalues(modelo),
    cookd = cooks.distance(modelo),
    modelo = nome_modelo
  )
}

# diagnósticos
diag_poisson <- diagnosticos_glm(modelo_poisson, "Poisson")
diag_logbin <- diagnosticos_glm(mod_lb, "Log-binomial")

X <- model.matrix(modelo_poisson)
p <- ncol(X)
n <- nrow(X)

# Função para criar gráficos separados
criar_graficos <- function(diag_data, nome_modelo) {
  
  # 1. Alavancagem vs observações ===============
  g1 <- ggplot(diag_data, aes(x = obs, y = leverage)) +
    geom_point(color = "purple") +
    # geom_hline(yintercept = 2*p/n, linetype="dashed", color="red") +
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
    # geom_hline(yintercept = 4/n, linetype="dashed", color="red") +
    geom_text(   aes(label = ifelse(cookd > 4/nrow(respiratory4), obs, "")),   vjust = -0.5,   size = 2 ) +
    labs(x = "Observação", y = "Distrância de Cook") +
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
    scale_y_continuous(limits = c(-10, 5), breaks = seq(-10, 5, by = 2)) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) 
  
  list(alavancagem = g1, cook = g2, pearson = g3)
}

# Criar gráficos para cada modelo =============
graficos_poisson <- criar_graficos(diag_poisson, "Poisson")
graficos_logbin <- criar_graficos(diag_logbin, "Log-binomial")

# resíduos sandwich -------------------------------------------------------
X <- model.matrix(modelo_poisson)
mu <- fitted(modelo_poisson)
y <- respiratory4$outcome
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
# Variância robusta por observação:
var_robusta <- diag(X %*% matriz_covariancia_variancia_sandwich %*% t(X))
residuos_pearson_sandwich <- (y - mu) / sqrt(var_robusta)

# distância de Cook
cook <- (residuos_pearson^2 * leverages) / (p * (1 - leverages)^2)
cook_sandwich <- (residuos_pearson_sandwich^2 * leverages) / (p * (1 - leverages)^2)

# criar dataframe para ggplot
df_plot <- tibble(
  obs = 1:n,
  leverage = leverages,
  pearson = residuos_pearson,
  pearson_sandwich = residuos_pearson_sandwich,
  cook = cook,
  cook_sandwich = cook_sandwich,
  fitted = mu
)

# graficos ggplot

# # Alavanca vs observação
# p1 <- ggplot(df_plot, aes(x=obs, y=leverage)) +
#   geom_point(color="blue") +
#   geom_hline(yintercept = 2*p/n, linetype="dashed", color="red") +
#   labs(title="Alavanca vs Observação", y="Alavanca (h_ii)", x="Observação")

# # Resíduo de Pearson vs Valor ajustado
# p2 <- ggplot(df_plot, aes(x=fitted, y=pearson)) +
#   geom_point(color="darkgreen") +
#   geom_hline(yintercept = 0, linetype="dashed") +
#   labs(title="Resíduo de Pearson vs Valor Ajustado", y="Resíduo de Pearson", x="Valor Ajustado")

# Resíduo de Pearson vs Valor ajustado (com sandwich)
p3 <- ggplot(df_plot, aes(x=fitted, y=pearson_sandwich)) +
  geom_point(color="orange") +
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
  scale_y_continuous(limits = c(-10, 5), breaks = seq(-10, 5, by = 2)) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) 


# # Distância de Cook vs Observação
# p4 <- ggplot(df_plot, aes(x=obs, y=cook)) +
#   geom_point(color="blue") +
#   geom_hline(yintercept = 4/n, linetype="dashed", color="red") +
#   labs(x = "Observação", y = "Distrância de Cook") +
#   theme_minimal() +
#   theme(
#     title = element_blank(),
#     panel.border = element_rect(color = "grey70", fill = NA, size = 0.5),
#     axis.text.x   = element_text(color = "grey20",size = 7),
#     axis.text.y   = element_text(color = "grey20",size = 7),
#     axis.title.x  = element_text(color = "grey20",face = "bold"),
#     axis.title.y  = element_text(color = "grey20",face = "bold")
#   ) +
#   scale_x_continuous(limits = c(0, 120), breaks = seq(0, 120, by = 20)) +
#   scale_y_continuous(limits = c(-0.1, 1.3), breaks = seq(-0.1, 1.3, by = 0.1)) 


# Distância de Cook vs Observação (com sandwich)
p5 <- ggplot(df_plot, aes(x=obs, y=cook_sandwich)) +
  geom_point(color="darkgreen") +
  # geom_hline(yintercept = 4/n, linetype="dashed", color="red") +
  labs(x = "Observação", y = "Distrância de Cook") +
  geom_text(   aes(label = ifelse(cook_sandwich > 4/nrow(respiratory4), obs, "")),   vjust = -0.5,   size = 2 )  +
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

##
graficos_logbin$alavancagem
graficos_poisson$alavancagem

graficos_logbin$pearson
graficos_poisson$pearson
p3

graficos_logbin$cook
graficos_poisson$cook
p5

respiratory4[c(81,107),] # outliers detectados
respiratory4[c(20,33,54,62,74,91,102,104),] # outliers detectados apenas no poisson robusto

# saídas ------------------------------------------------------------------

# alavanca
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

# resíduo 

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
  p3, 
  width = 14,
  height = 12,
  units = "cm"
)

# cook 
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
  p5, 
  width = 14,
  height = 12,
  units = "cm"
)

