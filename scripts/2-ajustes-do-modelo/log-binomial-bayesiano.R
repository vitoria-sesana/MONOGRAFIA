# Ajuste do modelo log-binomial sob estimação bayesiana

# rotina ------------------------------------------------------------------
source("scripts/0-rotina.R", encoding = "UTF-8")

# ajuste log-binomial bayesiano -------------------------------------------
attach(respiratory4)
colnames(respiratory4)

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

## inits -----------
inits_list <- 
  list(
    b0 = -1,
    b1 = -0.5,
    b2 = -0.5,
    b3 = -0.5,
    .RNG.name = "base::Mersenne-Twister",
    .RNG.seed = 34)
# list(b0 = 0, b1 = 0, b2 = 0, b3 = 0, .RNG.name = "base::Mersenne-Twister", .RNG.seed = 34)


# ajuste ------------------------------------------------------------------

## especificando modelo -----------
model <- 
  rjags::jags.model(
    file = textConnection(model_string),
    data = dataList,
    inits = inits_list,
    n.chains = 1, # quantas cadeias
    n.adapt = 0 # sem adaptação
    
  )

## atualizando/update -----------
update(model, n.iter = 1000)

## amostras posteriores -----------
posterior <- 
  rjags::coda.samples(
    model,
    variable.names = c("b0", "b1", "b2", "b3"),
    n.iter = 10000,
    thin = 40
  )


# resultados --------------------------------------------------------------
posterior

resumo <- summary(posterior)
resumo 
resumo$statistics 
resumo$quantiles


# saída -------------------------------------------------------------------
# saveRDS(resumo, "E-NOVA-SIMULACAO/0-tabelas/ajuste_respiratory_jags.rds")


# # coda: analises de diagnostico -------------------------------------------
# require(coda)
# coda::as.mcmc(posterior) |> nrow()
# coda::as.mcmc(posterior) |> ncol()
# coda::as.mcmc(posterior) |> head()
# 	
# # diagnosticos visuais ----------------------------------------------------
# par(mar = rep(1, 4))
# plot(posterior)
# coda::autocorr.plot(as.mcmc(posterior))
# 
# 
# # menu coda ---------------------------------------------------------------
# posterior
# mcmc_posterior <- as.mcmc(posterior)
# codamenu()