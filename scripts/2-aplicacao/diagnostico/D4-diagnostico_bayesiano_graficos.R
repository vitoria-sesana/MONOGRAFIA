rm(list = ls())

# Leitura -----------------------------------------------------------------

resultados_aplicacao_bayesiana <- readRDS("saidas/1-saida-aplicacao/ajuste_logbin_bayesiano.rds")

posterior <- resultados_aplicacao_bayesiana$posterior
library(ggplot2)

# Transformar posterior em data frame (caso ainda não seja)
posterior_df <- as.data.frame(as.matrix(posterior))


# Densidade ---------------------------------------------------------------

# b0
p_b0 <- ggplot(posterior_df, aes(x = b0)) +
  geom_density(fill = "blue", alpha = 0.5) +
  ylim(0, 2.5) +
  labs(x = "Densidade", y = "") +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "grey70", fill = NA, size = 0.5),
    axis.text.x   = element_text(color = "grey20",size = 7),
    axis.text.y   = element_text(color = "grey20",size = 7),
    axis.title.x  = element_text(color = "grey20",face = "bold"),
    axis.title.y  = element_text(color = "grey20",face = "bold")
  )

# b1
p_b1 <- ggplot(posterior_df, aes(x = b1)) +
  geom_density(fill = "blue", alpha = 0.5) +
  ylim(0, 2.5) +
  labs(x = "Densidade", y = "") +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "grey70", fill = NA, size = 0.5),
    axis.text.x   = element_text(color = "grey20",size = 7),
    axis.text.y   = element_text(color = "grey20",size = 7),
    axis.title.x  = element_text(color = "grey20",face = "bold"),
    axis.title.y  = element_text(color = "grey20",face = "bold")
  )

# b2
p_b2 <- ggplot(posterior_df, aes(x = b2)) +
  geom_density(fill = "blue", alpha = 0.5) +
  ylim(0, 2.5) +
  labs(x = "Densidade", y = "") +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "grey70", fill = NA, size = 0.5),
    axis.text.x   = element_text(color = "grey20",size = 7),
    axis.text.y   = element_text(color = "grey20",size = 7),
    axis.title.x  = element_text(color = "grey20",face = "bold"),
    axis.title.y  = element_text(color = "grey20",face = "bold")
  )


# b3
p_b3 <- ggplot(posterior_df, aes(x = b3)) +
  geom_density(fill = "blue", alpha = 0.5) +
  ylim(0, 2.5) +
  labs(x = "Densidade", y = "") +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "grey70", fill = NA, size = 0.5),
    axis.text.x   = element_text(color = "grey20",size = 7),
    axis.text.y   = element_text(color = "grey20",size = 7),
    axis.title.x  = element_text(color = "grey20",face = "bold"),
    axis.title.y  = element_text(color = "grey20",face = "bold")
  )

p_b0
p_b1
p_b2
p_b3


# Trace Plot --------------------------------------------------------------

# Transformar posterior em data frame (caso ainda não seja)
# posterior_df <- posterior

# Criar um índice de iteração
posterior_df$iter <- 1:nrow(posterior_df)

# b0
t_b0 <- ggplot(posterior_df, aes(x = iter, y = b0)) +
  geom_line(color = "red", linetype = 1, size = 0.2) +
  ylim(-2, 2) +
  labs(x = "Interações", y = "Intercepto") +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "grey70", fill = NA, size = 0.5),
    axis.text.x   = element_text(color = "grey20",size = 7),
    axis.text.y   = element_text(color = "grey20",size = 7),
    axis.title.x  = element_text(color = "grey20",face = "bold"),
    axis.title.y  = element_text(color = "grey20",face = "bold")
  )


# b1
t_b1 <- ggplot(posterior_df, aes(x = iter, y = b1)) +
  geom_line(color = "red", linetype = 1, size = 0.2) +
  ylim(-2, 2) +
  labs(x = "Interações", y = "Centro") +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "grey70", fill = NA, size = 0.5),
    axis.text.x   = element_text(color = "grey20",size = 7),
    axis.text.y   = element_text(color = "grey20",size = 7),
    axis.title.x  = element_text(color = "grey20",face = "bold"),
    axis.title.y  = element_text(color = "grey20",face = "bold")
  )


# b2
t_b2 <- ggplot(posterior_df, aes(x = iter, y = b2)) +
  geom_line(color = "red", linetype = 1, size = 0.2) +
  ylim(-2, 2) +
  labs(x = "Interações", y = "Tratamento") +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "grey70", fill = NA, size = 0.5),
    axis.text.x   = element_text(color = "grey20",size = 7),
    axis.text.y   = element_text(color = "grey20",size = 7),
    axis.title.x  = element_text(color = "grey20",face = "bold"),
    axis.title.y  = element_text(color = "grey20",face = "bold")
  )


# b3
t_b3 <- ggplot(posterior_df, aes(x = iter, y = b3)) +
  geom_line(color = "red", linetype = 1, size = 0.2) +
  ylim(-2, 2) +
  labs(x = "Interações", y = "Estado inicial") +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "grey70", fill = NA, size = 0.5),
    axis.text.x   = element_text(color = "grey20",size = 7),
    axis.text.y   = element_text(color = "grey20",size = 7),
    axis.title.x  = element_text(color = "grey20",face = "bold"),
    axis.title.y  = element_text(color = "grey20",face = "bold")
  )


t_b0
t_b1
t_b2
t_b3


# Autocorrelação ----------------------------------------------------------

# Função para criar autocorrelação com linhas finas e bandas de confiança
autocorr_gg2 <- function(x, varname) {
  n <- length(x)
  acf_values <- acf(x, plot = FALSE)
  acf_df <- data.frame(
    lag = acf_values$lag,  # remover lag 0
    acf = acf_values$acf
  )
  
  # Banda de confiança aproximada
  conf <- 1.96 / sqrt(n)
  
  ggplot(acf_df, aes(x = lag, y = acf)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "darkblue") +
    geom_ribbon(aes(ymin = -conf, ymax = conf), fill = "lightgray", alpha = 0.3) +
    geom_segment(aes(xend = lag, yend = 0), color = "darkblue", size = 0.3) +
    labs(x = "Lag", y = "Autocorrelação", title = varname) +
    theme_minimal() +
    theme(
      panel.border = element_rect(color = "grey70", fill = NA, size = 0.5),
      axis.text.x   = element_text(color = "grey20",size = 7),
      axis.text.y   = element_text(color = "grey20",size = 7),
      axis.title.x  = element_text(color = "grey20",face = "bold"),
      axis.title.y  = element_text(color = "grey20",face = "bold"),
      title = element_blank()
    ) +
    ylim(-0.2, 1)
}

# b0
a_b0 <- autocorr_gg2(posterior_df$b0, "b0")

# b1
a_b1 <- autocorr_gg2(posterior_df$b1, "b1")

# b2
a_b2 <- autocorr_gg2(posterior_df$b2, "b2")

# b3
a_b3 <- autocorr_gg2(posterior_df$b3, "b3")

a_b0
a_b1
a_b2
a_b3

# outras analises ---------------------------------------------------------

## verificando a convergência
# effectiveSize(posterior)
# gelman.diag(posterior)
# heidel.diag(posterior)

## menu coda ---------------------------------------------------------------
# posterior
# mcmc_posterior <- as.mcmc(posterior)
# codamenu()

## verificação do resíduo do modelo ---------------------------------------------------
# library(DHARMa)
# simulations = model$BUGSoutput$sims.list$beetlesPred
# pred = apply(model$BUGSoutput$sims.list$lambda, 2, median)
# dim(simulations)
# sim = createDHARMa(simulatedResponse = t(simulations), observedResponse = data$beetles, fittedPredictedResponse = pred, integerResponse = T)
# plotSimulatedResiduals(sim)


# saída -------------------------------------------------------------------

# --- Densidades ---
ggsave(
  filename = "plots/2-plots-aplicacao/dens_b0.pdf",
  plot = p_b0,
  device = cairo_pdf,
  width = 14,
  height = 12,
  units = "cm"
  )
ggsave("plots/2-plots-aplicacao/dens_b1.pdf", plot = p_b1, device = cairo_pdf, width = 14, height = 12, units = "cm")
ggsave("plots/2-plots-aplicacao/dens_b2.pdf", plot = p_b2, device = cairo_pdf, width = 14, height = 12, units = "cm")
ggsave("plots/2-plots-aplicacao/dens_b3.pdf", plot = p_b3, device = cairo_pdf, width = 14, height = 12, units = "cm")

# --- Traceplots ---
ggsave("plots/2-plots-aplicacao/trace_b0.pdf", plot = t_b0, device = cairo_pdf, width = 14, height = 12, units = "cm")
ggsave("plots/2-plots-aplicacao/trace_b1.pdf", plot = t_b1, device = cairo_pdf, width = 14, height = 12, units = "cm")
ggsave("plots/2-plots-aplicacao/trace_b2.pdf", plot = t_b2, device = cairo_pdf, width = 14, height = 12, units = "cm")
ggsave("plots/2-plots-aplicacao/trace_b3.pdf", plot = t_b3, device = cairo_pdf, width = 14, height = 12, units = "cm")

# --- Autocorrelações ---
ggsave("plots/2-plots-aplicacao/acorr_b0.pdf", plot = a_b0, device = cairo_pdf, width = 14, height = 12, units = "cm")
ggsave("plots/2-plots-aplicacao/acorr_b1.pdf", plot = a_b1, device = cairo_pdf, width = 14, height = 12, units = "cm")
ggsave("plots/2-plots-aplicacao/acorr_b2.pdf", plot = a_b2, device = cairo_pdf, width = 14, height = 12, units = "cm")
ggsave("plots/2-plots-aplicacao/acorr_b3.pdf", plot = a_b3, device = cairo_pdf, width = 14, height = 12, units = "cm")

