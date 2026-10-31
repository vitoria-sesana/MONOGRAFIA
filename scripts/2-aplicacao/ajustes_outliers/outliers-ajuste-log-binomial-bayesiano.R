# ajuste do modelo log-binomial bauyesiano com a inclusão e retirada dos outliers

# rotina ------------------------------------------------------------------
source("scripts/0-rotina.R", encoding = "UTF-8")

# outliers ----------------------------------------------------------------
outliers <- c(81, 107)

# definição das combinações -----------------------------------------------

dados_cenarios <- list(
  "Dados completos" = respiratory4,
  "Sem 81" = respiratory4 %>%
    filter(id != 81),
  "Sem 107" = respiratory4 %>%
    filter(id != 107),
  "Sem 81 e 107" = respiratory4 %>%
    filter(!id %in% c(81, 107))
)

# ajuste log-binomial bayesiano -------------------------------------------

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
    b0 = -1,
    b1 = -0.5,
    b2 = -0.5,
    b3 = -0.5,
    .RNG.name = "base::Mersenne-Twister", # gerador de semente
    .RNG.seed = 34 # semente pra manter os mesmos resultados
  )



# Ajustes -----------------------------------------------------------------


# Ajuste completo 
dataList1 <- 
  list(
    N = length(dados_cenarios$`Dados completos`$outcome), 
    Y = dados_cenarios$`Dados completos`$outcome,
    X1 = dados_cenarios$`Dados completos`$center,
    X2 = dados_cenarios$`Dados completos`$treat,
    X3 = dados_cenarios$`Dados completos`$baseline
  )

model1 <- 
  rjags::jags.model(
    file = textConnection(model_string),
    data = dataList1,
    inits = chutes_iniciais,
    n.chains = 1, # quantas cadeias
    n.adapt = 0 # sem adaptação
  )

update(model1, n.iter = 1000)

## amostras posteriores -----------
posterior1 <- 
  rjags::coda.samples(
    model1,
    variable.names = c("b0", "b1", "b2", "b3"),
    n.iter = 10000,
    thin = 75
  )

posterior1 %>% summary()


# Ajuste sem 81
dataList2 <- 
  list(
    N = length(dados_cenarios$`Sem 81`$outcome), 
    Y = dados_cenarios$`Sem 81`$outcome,
    X1 = dados_cenarios$`Sem 81`$center,
    X2 = dados_cenarios$`Sem 81`$treat,
    X3 = dados_cenarios$`Sem 81`$baseline
  )

model2 <- 
  rjags::jags.model(
    file = textConnection(model_string),
    data = dataList2,
    inits = chutes_iniciais,
    n.chains = 1,
    n.adapt = 0
  )

update(model2, n.iter = 1000)

posterior2 <- 
  rjags::coda.samples(
    model2,
    variable.names = c("b0", "b1", "b2", "b3"),
    n.iter = 10000,
    thin = 75
  )


# Ajuste sem 107
dataList3 <- 
  list(
    N = length(dados_cenarios$`Sem 107`$outcome), 
    Y = dados_cenarios$`Sem 107`$outcome,
    X1 = dados_cenarios$`Sem 107`$center,
    X2 = dados_cenarios$`Sem 107`$treat,
    X3 = dados_cenarios$`Sem 107`$baseline
  )

model3 <- 
  rjags::jags.model(
    file = textConnection(model_string),
    data = dataList3,
    inits = chutes_iniciais,
    n.chains = 1,
    n.adapt = 0
  )

update(model3, n.iter = 1000)

posterior3 <- 
  rjags::coda.samples(
    model3,
    variable.names = c("b0", "b1", "b2", "b3"),
    n.iter = 10000,
    thin = 75
  )


# Ajuste sem 81 e 107 
dataList4 <- 
  list(
    N = length(dados_cenarios$`Sem 81 e 107`$outcome), 
    Y = dados_cenarios$`Sem 81 e 107`$outcome,
    X1 = dados_cenarios$`Sem 81 e 107`$center,
    X2 = dados_cenarios$`Sem 81 e 107`$treat,
    X3 = dados_cenarios$`Sem 81 e 107`$baseline
  )

model4 <- 
  rjags::jags.model(
    file = textConnection(model_string),
    data = dataList4,
    inits = chutes_iniciais,
    n.chains = 1,
    n.adapt = 0
  )

update(model4, n.iter = 1000)

posterior4 <- 
  rjags::coda.samples(
    model4,
    variable.names = c("b0", "b1", "b2", "b3"),
    n.iter = 10000,
    thin = 75
  )

# Posterior ---------------------------------------------------------------

posterior_todos <- coda::mcmc.list(
  posterior1[[1]],
  posterior2[[1]],
  posterior3[[1]],
  posterior4[[1]]
)


# Estimativas média e mediana ---------------------------------------------
nomes_cenarios <- c("Dados completos", "Sem 81", "Sem 107", "Sem 81 e 107")

tabela_resultados_estimativas <- bind_rows(
  lapply(1:4, function(i) {
    
    # Resumo estatístico do cenário i
    x <- summary(posterior_todos[[i]])
    
    bind_rows(
      data.frame(
        cenario = nomes_cenarios[i],
        estatistica = "Media",
        t(x$statistics[, "Mean"]) # Pega especificamente a coluna da média
      ),
      data.frame(
        cenario = nomes_cenarios[i],
        estatistica = "Mediana",
        t(x$quantiles[, "50%"])   # Pega especificamente a mediana (percentil 50)
      )
    )
  })
) %>%
  as.data.frame() %>% 
  mutate(
    b0_exp = exp(b0),
    b1_exp = exp(b1),
    b2_exp = exp(b2),
    b3_exp = exp(b3)
  ) %>%
  select(cenario, estatistica, b0, b0_exp, b1, b1_exp, b2, b2_exp, b3, b3_exp) %>% 
  mutate(
    across(
      where(is.numeric),
      ~ sprintf("%.4f", .x)
    )
  ) 


# saída -------------------------------------------------------------------

print(tabela_resultados_estimativas)

saveRDS(
  posterior_todos, 
  "saidas/1-saida-aplicacao/outliers_log_binomial_bayesiano_posterior.rds"
)

saveRDS(
  tabela_resultados_estimativas, 
  "saidas/1-saida-aplicacao/outliers_log_binomial_bayesiano_estimativas.rds"
)
