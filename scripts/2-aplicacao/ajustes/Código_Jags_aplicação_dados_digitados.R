library(rjags)
library(coda)

#----------------------------------------------------------
# 1. Modelo JAGS (string)
#----------------------------------------------------------
model_string <- "
model {

  # Verossimilhança
  for (i in 1:N) {
    pp[i] <- exp(b0 + b1*X1[i] + b2*X2[i] + b3*X3[i])
    Y[i] ~ dbern(pp[i])
  }

  # Priors
  b0 ~ dnorm(0, 0.01)
  b1 ~ dnorm(0, 0.01)
  b2 ~ dnorm(0, 0.01)
  b3 ~ dnorm(0, 0.01)

  # Restrições para cada observação
  for (i in 1:N) {
    ones[i] ~ dbern(C1[i])
    C1[i] <- step(1 - pp[i])
  }

  # Restrições das combinações das covariáveis
  ones_e ~ dbern(C1_e)

  C1_e <-
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

#----------------------------------------------------------
# 2. Dados
#----------------------------------------------------------

# leitura --------------------------------------------------------------------
#bd_convergiram <- readRDS("bases_convergiram_respiratory.rds") 
#base <- bd_convergiram$amostra_500_replica_344
#attach(base)
#colnames(base)

## dados -------------------------------------------------------------------
#data <- list(N = length(y), 
#             Y = y,
#             center = x_center,
#             treat = x_treat,
#             baseline = x_baseline)

#N <- data$N
#X1 <- data$center
#X2 <- data$treat
#X3 <- data$baseline


Y <- c(0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 1, 1, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 0, 1, 1, 1, 1, 0, 0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 0, 1, 0, 0, 0, 0, 1, 1, 1, 1)
X1 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0) 
X2 <- c(1, 1, 0, 1, 1, 0, 1, 0, 0, 1, 0, 0, 1, 1, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 1, 1, 1, 1, 0, 0, 1, 0, 1, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0)
X3 = c(1, 1, 0, 0, 0, 1, 1, 1, 0, 0, 0, 1, 0, 1, 0, 1, 1, 1, 1, 0, 1, 0, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 1, 1, 0, 0)


#list(b0 =  -1.694, b1 =0.412, b2 = 0.319, b3 = 0.815)

#list(b0 =  -1, b1 =-0.5, b2 = -0.5, b3 = -0.5)


# Vetores de "ones"
N <- length(Y)
ones <- rep(1, N)
ones_e <- 1

  
# Lista de dados para JAGS
data_jags <- list(
  N = N, Y = Y,
  X1 = X1, X2 = X2, X3 = X3,
  ones = ones,
  ones_e = ones_e
)

#----------------------------------------------------------
# 3. Valores iniciais 
#----------------------------------------------------------
#inits <- function() list(
#  b0 = -1,
#  b1 = -0.5,
#  b2 = -0.5,
#  b3 = -0.5
#)


inits <- list(
  list(b0 = -1.0, b1 = -0.5, b2 = -0.5, b3 = -0.5, .RNG.name = "base::Mersenne-Twister", .RNG.seed = 34),
  list(b0 = -0.1, b1 =  -0.1, b2 =  -0.1, b3 =  -0.1, .RNG.name = "base::Mersenne-Twister", .RNG.seed = 34),
  list(b0 = -5.0, b1 = -3.7, b2 = -7.2, b3 = -9.5, .RNG.name = "base::Mersenne-Twister", .RNG.seed = 34)
)

#----------------------------------------------------------
# 4. Rodando o modelo
#----------------------------------------------------------
model <- jags.model(
  textConnection(model_string),
  data = data_jags,
  inits = inits,
  n.chains = 3,
  n.adapt = 1000
)

update(model, 1000)   # burn-in

# Amostrar
samples <- coda.samples(
  model,
  variable.names = c("b0", "b1", "b2", "b3"),
  n.iter = 40000, 
  thin = 10
)

#----------------------------------------------------------
# 5. Resultados
#----------------------------------------------------------
summary(samples)
plot(samples)

amostras <- samples[, c("b0", "b1", "b2", "b3")]

#codamenu()

gelman.diag(amostras)      # Gelman e Rubin (R-hat)
geweke.diag(amostras)      # Geweke
heidel.diag(amostras)      # Heidelberger e Welch
raftery.diag(amostras)     # Raftery e Lewis
autocorr.diag(amostras)    # autocorrelações
effectiveSize(amostras)    # tamanho amostral efetivo
gelman.plot(amostras)      # evolução do R-hat ao longo das iterações


res <- summary(amostras)

res$statistics   # Mean, SD, Naive SE, Time-series SE (cadeias combinadas)
res$quantiles    # 2,5%, 25%, 50%, 75%, 97,5% (cadeias combinadas)

##############################
# Resumo das cadeias separadas
##############################

summary(amostras[[3]])          # só a cadeia 1
lapply(amostras, summary)       # as 3, uma por vez

# só as médias, cadeia a cadeia
sapply(amostras, function(c) colMeans(as.matrix(c)))

todas <- as.mcmc(as.matrix(amostras))   # empilha as 3 cadeias
HPDinterval(todas, prob = 0.95)

todas <- as.matrix(amostras)
hpd   <- HPDinterval(as.mcmc(todas), prob = 0.95)
res   <- summary(amostras)

tabela <- data.frame(
  media        = res$statistics[, "Mean"],
  dp           = res$statistics[, "SD"],
  erro_MC      = res$statistics[, "Naive SE"],
  erro_ts      = res$statistics[, "Time-series SE"],
  q2.5         = res$quantiles[, "2.5%"],
  q25          = res$quantiles[, "25%"],
  mediana      = res$quantiles[, "50%"],
  q75          = res$quantiles[, "75%"],
  q97.5        = res$quantiles[, "97.5%"],
  amp_quant    = res$quantiles[, "97.5%"] - res$quantiles[, "2.5%"],
  HPD_inf      = hpd[, "lower"],
  HPD_sup      = hpd[, "upper"],
  amp_HPD      = hpd[, "upper"] - hpd[, "lower"],
  RP           = exp(res$statistics[, "Mean"])   # razão de prevalência (exp da média)
#  P_negativo   = colMeans(todas < 0)              # P(beta < 0)
)

print(round(tabela, 4))

plot(samples[[2]]) ### 

# Para levar ao Word/LaTeX
write.csv2(round(tabela, 4), "resumo_bayesiano.csv")   # separador ; e vírgula decimal



