# avaliando resultados das estimativas do modelo logbin bayesiano sob diferentes chutes iniciais

# rotina ------------------------------------------------------------------
source("scripts/0-rotina.R", encoding = "UTF-8")

# chutes iniciais ---------------------------------------------------------
chutes_iniciais <- readRDS("saidas/1-saida-aplicacao/chutes_iniciais_lista.rds")

inits <- lapply(chutes_iniciais, function(chute) {
  c(chute, list(.RNG.name = "base::Mersenne-Twister", .RNG.seed = 34))
})

inits <- unname(inits)

## dados -----------

respiratory4 <- 
  respiratory4 %>% 
  mutate(
    as.numeric(baseline),
    center = as.numeric(ifelse(center == 1, 1, 0)),
    treat = as.numeric(ifelse(treat == "P", 1, 0)),
    baseline = as.numeric(ifelse(baseline == 0, 1, 0))
  )

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

model <- 
  rjags::jags.model(
    file = textConnection(model_string),
    data = dataList,
    inits = inits,
    n.chains = 10, # quantas cadeias
    n.adapt = 1000 # sem adaptação
  )

update(model, n.iter = 1000)

posterior <- 
  rjags::coda.samples(
    model,
    variable.names = c("b0", "b1", "b2", "b3"),
    n.iter = 40000, 
    thin = 10
  )

resultado <- summary(posterior)



# Posterior ---------------------------------------------------------------

posterior_todos <- coda::mcmc.list(posterior)


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
