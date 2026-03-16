# Ajuste do modelo log-binomial sob estimação bayesiana

# rotina ------------------------------------------------------------------
source("scripts/0-rotina.R", encoding = "UTF-8")

# ajuste log-binomial bayesiano -------------------------------------------
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

## inits -----------
chutes_iniciais <- 
  list(
    list(b0=-1, b1=-0.5, b2=-0.5, b3=-0.5, 
         .RNG.name = "base::Mersenne-Twister", # gerador de semente
         .RNG.seed = 34),
    list(b0=-0.8, b1=-0.3, b2=-0.4, b3=-0.2,
         .RNG.name = "base::Mersenne-Twister", # gerador de semente
         .RNG.seed = 34
         ),
    list(b0=-1.2, b1=-0.6, b2=-0.3, b3=-0.7,
         .RNG.name = "base::Mersenne-Twister", # gerador de semente
         .RNG.seed = 34
         ),
    list(b0=-0.1, b1=-0.1, b2=-0.1, b3=-0.1,
         .RNG.name = "base::Mersenne-Twister", # gerador de semente
         .RNG.seed = 34
         )
  )

# ajuste ------------------------------------------------------------------

## especificando modelo -----------
model <- 
  rjags::jags.model(
    file = textConnection(model_string),
    data = dataList,
    inits = chutes_iniciais,
    n.chains = 4, # quantas cadeias
    n.adapt = 0 # sem adaptação
    
  )

## atualizando/update -----------
update(model, n.iter = 4000)

## amostras posteriores -----------

posterior <- coda.samples(
  model,
  variable.names = c("b0", "b1", "b2", "b3"), # nomes dos parâmetros
  n.iter = 5000,
  thin = 50
)

# resultados --------------------------------------------------------------
amostras
nchain(amostras)
length(amostras)
resultado_bayesiano <- summary(amostras)

gelman.plot(amostras)
gelman.diag(amostras)

x <- summary(amostras)
x$quantiles
HPDinterval(amostras)

summary(amostras[1])$quantiles
HPDinterval(amostras)[1]

summary(amostras[2])$quantiles
HPDinterval(amostras)[2]

summary(amostras[3])$quantiles
HPDinterval(amostras)[3]

summary(amostras[4])$quantiles
HPDinterval(amostras)[4]

# média ergótica ----------------------------------------------------------

# média ergótica para uma cadeia

samples_b0 <- as.numeric(amostras[[1]][,"b0"])

ergodic_mean <- cumsum(samples_b0) / seq_along(samples_b0)

plot(ergodic_mean, type="l",
     xlab="Iteração",
     ylab="Média ergódica",
     main="Média ergódica de b0")
abline(h = mean(samples_b0), col="red")

# para qualquer parâmetro
ergodic_plot <- function(samples, param){
  
  x <- as.numeric(samples[[1]][,param])
  
  erg_mean <- cumsum(x) / seq_along(x)
  
  plot(erg_mean, type="l",
       xlab="Iteração",
       ylab="Média ergódica",
       main=paste("Média ergódica:", param))
  
  abline(h=mean(x), col="red")
}

ergodic_plot(amostras, "b0")
ergodic_plot(amostras, "b1")
ergodic_plot(amostras, "b2")
ergodic_plot(amostras, "b3")


## Para todas as cadeias juntas

x <- as.numeric(as.matrix(amostras)[,"b1"])

erg_mean <- cumsum(x) / seq_along(x)

plot(erg_mean, type="l")


# critério de geweke -------------------------------------------------------

geweke <- coda::geweke.diag(amostras)
print(geweke)

geweke.plot(amostras)
