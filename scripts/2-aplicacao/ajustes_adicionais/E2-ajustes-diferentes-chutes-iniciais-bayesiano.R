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

attach(respiratory4)
## dados -----------
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


# Chutes iniciais ---------------------------------------------------------

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

names(modelos_bayesianos) <- names(chutes_iniciais)
posterior_todos <- do.call(
  coda::mcmc.list,
  lapply(modelos_bayesianos, function(x) x[[1]])
)

coda::traceplot(
  posterior_todos,
  col = 1:length(chutes_iniciais),
  ask = FALSE
)

resultado_bayesiano <- summary(posterior_todos)

resultado_bayesiano

# Intervalos HPD de 95%
intervalos_HPD <- coda::HPDinterval(
  posterior_todos,
  prob = 0.95
)

intervalos_HPD

summary(modelos_bayesianos$chute1)

coda::HPDinterval(
  modelos_bayesianos$chute1,
  prob = 0.95
)



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
