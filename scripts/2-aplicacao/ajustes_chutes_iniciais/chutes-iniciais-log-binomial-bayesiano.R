# avaliando resultados das estimativas do modelo logbin bayesiano sob diferentes chutes iniciais

# rotina ------------------------------------------------------------------
source("scripts/0-rotina.R", encoding = "UTF-8")

# Chutes iniciais ---------------------------------------------------------
chutes_iniciais <- readRDS("saidas/1-saida-aplicacao/chutes_iniciais_lista.rds")


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

# Ajuste 1
chute_inicial1 <- 
  list(
    b0 = chutes_iniciais$chute1$b0,
    b1 = chutes_iniciais$chute1$b1,
    b2 = chutes_iniciais$chute1$b2,
    b3 = chutes_iniciais$chute1$b3,
    .RNG.name = "base::Mersenne-Twister", # gerador de semente
    .RNG.seed = 34 # semente pra manter os mesmos resultados
  )

model1 <- 
  rjags::jags.model(
    file = textConnection(model_string),
    data = dataList,
    inits = chute_inicial1,
    n.chains = 1, # quantas cadeias
    n.adapt = 0 # sem adaptação
  )

update(model1, n.iter = 1000)

posterior1 <- 
  rjags::coda.samples(
    model1,
    variable.names = c("b0", "b1", "b2", "b3"),
    n.iter = 10000,
    thin = 75
  )

resultado1 <- summary(posterior1)

# Ajuste 2
chute_inicial2 <- list(
  b0 = chutes_iniciais$chute2$b0,
  b1 = chutes_iniciais$chute2$b1,
  b2 = chutes_iniciais$chute2$b2,
  b3 = chutes_iniciais$chute2$b3,
  .RNG.name = "base::Mersenne-Twister",
  .RNG.seed = 34
)


model2 <- rjags::jags.model(
  file = textConnection(model_string),
  data = dataList,
  inits = chute_inicial2,
  n.chains = 1,
  n.adapt = 0
)

update(model2, n.iter = 1000)

posterior2 <- rjags::coda.samples(
  model2,
  variable.names = c("b0", "b1", "b2", "b3"),
  n.iter = 10000,
  thin = 75
)

resultado2 <- summary(posterior2)

# Ajuste 3
chute_inicial3 <- list(
  b0 = chutes_iniciais$chute3$b0,
  b1 = chutes_iniciais$chute3$b1,
  b2 = chutes_iniciais$chute3$b2,
  b3 = chutes_iniciais$chute3$b3,
  .RNG.name = "base::Mersenne-Twister",
  .RNG.seed = 34
)

model3 <- rjags::jags.model(
  file = textConnection(model_string),
  data = dataList,
  inits = chute_inicial3,
  n.chains = 1,
  n.adapt = 0
)

update(model3, n.iter = 1000)

posterior3 <- rjags::coda.samples(
  model3,
  variable.names = c("b0", "b1", "b2", "b3"),
  n.iter = 10000,
  thin = 75
)

resultado3 <- summary(posterior3)


# Ajuste 4
chute_inicial4 <- list(
  b0 = chutes_iniciais$chute4$b0,
  b1 = chutes_iniciais$chute4$b1,
  b2 = chutes_iniciais$chute4$b2,
  b3 = chutes_iniciais$chute4$b3,
  .RNG.name = "base::Mersenne-Twister",
  .RNG.seed = 34
)

model4 <- rjags::jags.model(
  file = textConnection(model_string),
  data = dataList,
  inits = chute_inicial4,
  n.chains = 1,
  n.adapt = 0
)

update(model4, n.iter = 1000)

posterior4 <- rjags::coda.samples(
  model4,
  variable.names = c("b0", "b1", "b2", "b3"),
  n.iter = 10000,
  thin = 75
)

resultado4 <- summary(posterior4)


# Ajuste 5
chute_inicial5 <- list(
  b0 = chutes_iniciais$chute5$b0,
  b1 = chutes_iniciais$chute5$b1,
  b2 = chutes_iniciais$chute5$b2,
  b3 = chutes_iniciais$chute5$b3,
  .RNG.name = "base::Mersenne-Twister",
  .RNG.seed = 34
)

model5 <- rjags::jags.model(
  file = textConnection(model_string),
  data = dataList,
  inits = chute_inicial5,
  n.chains = 1,
  n.adapt = 0
)

update(model5, n.iter = 1000)

posterior5 <- rjags::coda.samples(
  model5,
  variable.names = c("b0", "b1", "b2", "b3"),
  n.iter = 10000,
  thin = 75
)

resultado5 <- summary(posterior5)


# Ajuste 6
chute_inicial6 <- list(
  b0 = chutes_iniciais$chute6$b0,
  b1 = chutes_iniciais$chute6$b1,
  b2 = chutes_iniciais$chute6$b2,
  b3 = chutes_iniciais$chute6$b3,
  .RNG.name = "base::Mersenne-Twister",
  .RNG.seed = 34
)

model6 <- rjags::jags.model(
  file = textConnection(model_string),
  data = dataList,
  inits = chute_inicial6,
  n.chains = 1,
  n.adapt = 0
)

update(model6, n.iter = 1000)

posterior6 <- rjags::coda.samples(
  model6,
  variable.names = c("b0", "b1", "b2", "b3"),
  n.iter = 10000,
  thin = 75
)

resultado6 <- summary(posterior6)


# Ajuste 7
chute_inicial7 <- list(
  b0 = chutes_iniciais$chute7$b0,
  b1 = chutes_iniciais$chute7$b1,
  b2 = chutes_iniciais$chute7$b2,
  b3 = chutes_iniciais$chute7$b3,
  .RNG.name = "base::Mersenne-Twister",
  .RNG.seed = 34
)

model7 <- rjags::jags.model(
  file = textConnection(model_string),
  data = dataList,
  inits = chute_inicial7,
  n.chains = 1,
  n.adapt = 0
)

update(model7, n.iter = 1000)

posterior7 <- rjags::coda.samples(
  model7,
  variable.names = c("b0", "b1", "b2", "b3"),
  n.iter = 10000,
  thin = 75
)

resultado7 <- summary(posterior7)


# Ajuste 8
chute_inicial8 <- list(
  b0 = chutes_iniciais$chute8$b0,
  b1 = chutes_iniciais$chute8$b1,
  b2 = chutes_iniciais$chute8$b2,
  b3 = chutes_iniciais$chute8$b3,
  .RNG.name = "base::Mersenne-Twister",
  .RNG.seed = 34
)

model8 <- rjags::jags.model(
  file = textConnection(model_string),
  data = dataList,
  inits = chute_inicial8,
  n.chains = 1,
  n.adapt = 0
)

update(model8, n.iter = 1000)

posterior8 <- rjags::coda.samples(
  model8,
  variable.names = c("b0", "b1", "b2", "b3"),
  n.iter = 10000,
  thin = 75
)

resultado8 <- summary(posterior8)


# Ajuste 9
chute_inicial9 <- list(
  b0 = chutes_iniciais$chute9$b0,
  b1 = chutes_iniciais$chute9$b1,
  b2 = chutes_iniciais$chute9$b2,
  b3 = chutes_iniciais$chute9$b3,
  .RNG.name = "base::Mersenne-Twister",
  .RNG.seed = 34
)

model9 <- rjags::jags.model(
  file = textConnection(model_string),
  data = dataList,
  inits = chute_inicial9,
  n.chains = 1,
  n.adapt = 0
)

update(model9, n.iter = 1000)

posterior9 <- rjags::coda.samples(
  model9,
  variable.names = c("b0", "b1", "b2", "b3"),
  n.iter = 10000,
  thin = 75
)

resultado9 <- summary(posterior9)


# Ajuste 10
chute_inicial10 <- list(
  b0 = chutes_iniciais$chute10$b0,
  b1 = chutes_iniciais$chute10$b1,
  b2 = chutes_iniciais$chute10$b2,
  b3 = chutes_iniciais$chute10$b3,
  .RNG.name = "base::Mersenne-Twister",
  .RNG.seed = 34
)

model10 <- rjags::jags.model(
  file = textConnection(model_string),
  data = dataList,
  inits = chute_inicial10,
  n.chains = 1,
  n.adapt = 0
)

update(model10, n.iter = 1000)

posterior10 <- rjags::coda.samples(
  model10,
  variable.names = c("b0", "b1", "b2", "b3"),
  n.iter = 10000,
  thin = 75
)

resultado10 <- summary(posterior10)



# Posterior ---------------------------------------------------------------

posterior_todos <- coda::mcmc.list(
  posterior1[[1]],
  posterior2[[1]],
  posterior3[[1]],
  posterior4[[1]],
  posterior5[[1]],
  posterior6[[1]],
  posterior7[[1]],
  posterior8[[1]],
  posterior9[[1]],
  posterior10[[1]]
)


# Estimativas média e mediana ---------------------------------------------

tabela_resultados_estimativas <- bind_rows(
  lapply(1:10, function(i) {
    
    x <- summary(posterior_todos[[i]])
    
    bind_rows(
      data.frame(
        chute = i,
        estatistica = "Media",
        t(x$statistics[, 1])
      ),
      data.frame(
        chute = i,
        estatistica = "Mediana",
        t(x$quantiles[, 3])
      )
    )
  })
) %>%
  as.data.frame()

str(tabela_resultados_estimativas)

# saídas ------------------------------------------------------------------

## traceplot -----

library(coda)
library(ggplot2)
library(RColorBrewer) 

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

saveRDS(posterior_todos, "saidas/1-saida-aplicacao/chutes_iniciais_bayesiano_posterior.rds")

saveRDS(tabela_resultados_estimativas, "saidas/1-saida-aplicacao/chutes_iniciais_log_binomial_bayesiano_estimativas.rds")
