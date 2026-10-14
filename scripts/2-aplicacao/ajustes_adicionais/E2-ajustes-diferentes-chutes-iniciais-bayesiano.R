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


## dados -----------
attach(respiratory4)

dataList <- 
  list(
    N = length(outcome), 
    Y = outcome,
    X1 = center,
    X2 = treat,
    X3 = baseline
  )

## modelo -----------
model_string <- "
model{

  # --- Likelihood -----------------------------------------------------
  for (i in 1:N) {
    pp[i] <- exp(b0 + b1*X1[i] + b2*X2[i] + b3*X3[i])
    Y[i] ~ dbern(pp[i])
  }
  
  # --- Priors ---------------------------------------------------------
  b0 ~ dnorm(0, 0.01)
  b1 ~ dnorm(0, 0.01)
  b2 ~ dnorm(0, 0.01)
  b3 ~ dnorm(0, 0.01)
  
  # --- Constraints for each observation ------------------------------
  for (i in 1:N) {
    ones[i] ~ dbern(C1[i])     # use ones[i] supplied from R
    C1[i] <- step(1 - pp[i])   # ensures pp[i] ≤ 1
  }
  
  # --- Constraints for the 8 possible combinations of (X1,X2,X3) -----
  ones.e ~ dbern(C1.e)         # use ones.e supplied from R
  C1.e <-
    step(1 - exp(b0)) *
    step(1 - exp(b0 + b1)) *
    step(1 - exp(b0 + b2)) *
    step(1 - exp(b0 + b3)) *
    step(1 - exp(b0 + b1 + b2)) *
    step(1 - exp(b0 + b1 + b3)) *
    step(1 - exp(b0 + b2 + b3)) *
    step(1 - exp(b0 + b1 + b2 + b3))
}
"



# ajustes chutes iniciais -------------------------------------------------

modelos_bayesianos <- lapply(
  seq_along(chutes_iniciais),
  function(i) {
    
    # Valores iniciais do ajuste
    inits_i <- chutes_iniciais[[i]]
    
    inits_i$.RNG.name <- "base::Mersenne-Twister"
    inits_i$.RNG.seed <- 34 
    
    # Criar o modelo JAGS
    modelo_i <- rjags::jags.model(
      file = textConnection(model_string),
      data = dataList,
      inits = inits_i,
      n.chains = 1,
      n.adapt = 0
    )
    
    # Atualizar o modelo
    update(modelo_i, n.iter = 1000)
    
    # Extrair amostras posteriores
    posterior_i <- rjags::coda.samples(
      model_i <- modelo_i,
      variable.names = c("b0", "b1", "b2", "b3"),
      n.iter = 10000,
      thin = 1
    )
    
    posterior_i
  }
)


# resultados chutes iniciais bayesianos -----------------------------------

names(modelos_bayesianos) <- names(chutes_iniciais)
posterior_todos <- do.call(
  coda::mcmc.list,
  lapply(modelos_bayesianos, function(x) x[[1]])
)

resultado_bayesiano <- summary(posterior_todos)
resultado_bayesiano


# traceplot ---------------------------------------------------------------

library(coda)

traceplot(
  posterior_todos,
  smooth = FALSE,
  col = rainbow(10),
  ask = FALSE
)
graphics::par(mfrow = c(2, 2))

traceplot(
  posterior_todos,
  smooth = FALSE,
  col = rainbow(10),
  ask = FALSE
)

graphics::par(mfrow = c(1, 1))


# saídas ------------------------------------------------------------------

library(coda)
library(ggplot2)
library(RColorBrewer) 

# Pasta de destino
pasta <- "plots/2-plots-aplicacao"

if (!dir.exists(pasta)) {
  dir.create(pasta, recursive = TRUE)
}

coeficientes <- c("b0", "b1", "b2", "b3")

rotulos_y <- c(
  b0 = "Intercepto",
  b1 = "b1",
  b2 = "b2",
  b3 = "b3"
)

# 1. Tratamento para extrair as cadeias mantendo a separação por chute
if (inherits(posterior_todos, "mcmc.list")) {
  n_chains <- length(posterior_todos)
  n_iter <- nrow(posterior_todos[[1]])
  
  posterior_df <- do.call(rbind, lapply(1:n_chains, function(c) {
    df_c <- as.data.frame(as.matrix(posterior_todos[[c]]))
    df_c$chain <- factor(c)
    df_c$iter <- 1:n_iter
    df_c
  }))
} else {
  posterior_df <- as.data.frame(as.matrix(posterior_todos))
  posterior_df$iter <- 1:nrow(posterior_df)
  posterior_df$chain <- factor(1)
}

num_chains <- length(unique(posterior_df$chain))

if (num_chains <= 8) {
  cores_chutes <- brewer.pal(max(3, num_chains), "Dark2")[1:num_chains]
} else {
  cores_chutes <- rainbow(num_chains, s = 0.9, v = 0.9)
}

for (coef in coeficientes) {
  
  p <- ggplot(posterior_df, aes(x = iter, y = .data[[coef]], group = chain, color = chain)) +
    # Aumentado para 0.4 para destacar bem a cor (ajuste se quiser mais grosso/fino)
    geom_line(linetype = 1, linewidth = 0.4) + 
    scale_color_manual(values = cores_chutes) +
    ylim(-2, 2) +
    labs(
      x = "Iterações", 
      y = rotulos_y[coef]
    ) +
    theme_minimal() +
    theme(
      legend.position = "none", # Remove totalmente a legenda
      panel.border = element_rect(color = "grey70", fill = NA, linewidth = 0.5),
      axis.text.x   = element_text(color = "grey20", size = 7),
      axis.text.y   = element_text(color = "grey20", size = 7),
      axis.title.x  = element_text(color = "grey20", face = "bold"),
      axis.title.y  = element_text(color = "grey20", face = "bold")
    )
  
  ggsave(
    filename = file.path(pasta, paste0("trace_", coef, ".pdf")),
    plot = p,
    width = 14,
    height = 12,
    units = "cm",
    device = cairo_pdf
  )
}

# salvar resultados -------------------------------------------------------

saveRDS(posterior_todos, "saidas/1-saida-aplicacao/tabela_resultados_posteirores_chutes_iniciais.rds")
