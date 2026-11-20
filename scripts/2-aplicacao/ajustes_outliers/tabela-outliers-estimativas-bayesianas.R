# tabela resultados das estimativas do modelo bayesiano sob inclusão e exclusão de outliers

# rotina ------------------------------------------------------------------
source("scripts/0-rotina.R", encoding = "UTF-8")


outliers_logbin_bayes_posterior <- 
  "saidas/1-saida-aplicacao/outliers_log_binomial_bayesiano_posterior.rds" %>% 
  readRDS()

amostras <- outliers_logbin_bayes_posterior

# resumo ------------------------------------------------------------------
library(coda)
library(dplyr)

nomes_cenarios <- c("Dados completos", "Sem 81", "Sem 107", "Sem 81 e 107")

tabela_resumo <- lapply(seq_along(amostras), function(n) {
  
  amostra_n <- amostras[[n]]
  
  todas <- as.matrix(amostra_n)
  
  hpd <- HPDinterval(as.mcmc(todas), prob = 0.95)
  
  res <- summary(amostra_n)
  effes <- effectiveSize(amostra_n) 
  
  data.frame(
    cenario      = nomes_cenarios[n],
    parametro    = rownames(res$statistics),
    # tabela 1
    media        = res$statistics[, "Mean"],
    RP           = exp(res$statistics[, "Mean"]),
    dp           = res$statistics[, "SD"],
    erro_MC      = res$statistics[, "Naive SE"],
    erro_ts      = res$statistics[, "Time-series SE"],
    ESS          = effes[rownames(res$statistics)],
    
    # tabela 2 
    q2.5         = res$quantiles[, "2.5%"],
    q25          = res$quantiles[, "25%"],
    mediana      = res$quantiles[, "50%"],
    q75          = res$quantiles[, "75%"],
    q97.5        = res$quantiles[, "97.5%"],
    amp_quant    = res$quantiles[, "97.5%"] - res$quantiles[, "2.5%"],
    HPD_inf      = hpd[, "lower"],
    HPD_sup      = hpd[, "upper"],
    amp_HPD      = hpd[, "upper"] - hpd[, "lower"],
    row.names = NULL
  )
  
}) %>%
  bind_rows() %>%
  mutate(
    across(where(is.numeric), ~ round(.x, 4))
  )


tabela_resumo

tabela_resumo_1 <- tabela_resumo %>%
  select(
    cenario, parametro,
    media, RP, dp, erro_MC, erro_ts, ESS
  )

tabela_resumo_2 <- tabela_resumo %>%
  select(
    cenario, parametro,
    q2.5, q25, mediana, q75, q97.5,
    amp_quant, HPD_inf, HPD_sup, amp_HPD
  )






# saída latex -------------------------------------------------------------

library(knitr)
library(kableExtra)

## resumo

kbl(
  tabela_resumo_1,
  format = "latex",
  booktabs = TRUE,
  caption = "Tabela de resultados",
  label = "tab:resultados",
  align = "lcccccccc",
  linesep = ""
)

kbl(
  tabela_resumo_2,
  format = "latex",
  booktabs = TRUE,
  caption = "Tabela de resultados",
  label = "tab:resultados",
  align = "lcccccccc",
  linesep = ""
)

