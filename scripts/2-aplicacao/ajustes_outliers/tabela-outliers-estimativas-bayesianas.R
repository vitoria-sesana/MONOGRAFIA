# tabela resultados das estimativas do modelo bayesiano sob inclusão e exclusão de outliers

# rotina ------------------------------------------------------------------
source("scripts/0-rotina.R", encoding = "UTF-8")


outliers_logbin_bayes_posterior <- 
  "saidas/1-saida-aplicacao/outliers_log_binomial_bayesiano_posterior.rds" %>% 
  readRDS()

 
# Dados completos ---------------------------------------------------------

cenario = "Dados completos"

posterior_1 <-
  outliers_logbin_bayes_posterior[[1]]

resumo_1 <-
  posterior_1 %>% 
  summary()

bayes_statistics_1 <- 
  resumo_1$statistics %>% 
  round(4) %>% 
  as.data.frame() %>% 
  mutate(cenario = cenario)

bayes_quantiles_1 <-  
  resumo_1$quantiles %>% 
  round(4) %>% 
  as.data.frame() %>% 
  mutate(cenario = cenario)


intervalo_HPD_1 <- 
  coda::HPDinterval(as.mcmc.list(posterior_1), prob = 0.95)[[1]] %>% 
  as.data.frame() %>% 
  mutate(cenario = cenario)

prob_exata_intervalo_HPD_1 <-
  attr(coda::HPDinterval(as.mcmc.list(posterior_1))[[1]], "Probability") %>% 
  as.data.frame() %>% 
  mutate(cenario = cenario)

hpd_quanti_1 <- 
  cbind(
    bayes_statistics_1,
    bayes_quantiles_1,
    intervalo_HPD_1
  ) %>% 
  select(Mean, SD, `2.5%`, `97.5%`, lower, upper) %>% 
  mutate(
    quantilico       = sprintf("[%.4f, %.4f]", `2.5%`, `97.5%`),
    ampli_quantilico = `97.5%` - `2.5%`,
    hpd              = sprintf("[%.4f, %.4f]", lower, upper),
    ampli_hpd        = upper - lower
  ) %>%
  select(
    quantilico, 
    ampli_quantilico, 
    hpd,
    ampli_hpd
  ) %>% 
  mutate(cenario = cenario)




# Sem 81 ------------------------------------------------------------------

cenario = "Sem 81"

posterior_2 <-
  outliers_logbin_bayes_posterior[[1]]

resumo_2 <-
  posterior_2 %>% 
  summary()

bayes_statistics_2 <- 
  resumo_2$statistics %>% 
  round(4) %>% 
  as.data.frame() %>% 
  mutate(cenario = cenario)

bayes_quantiles_2 <-  
  resumo_2$quantiles %>% 
  round(4) %>% 
  as.data.frame() %>% 
  mutate(cenario = cenario)


intervalo_HPD_2 <- 
  coda::HPDinterval(as.mcmc.list(posterior_2), prob = 0.95)[[1]] %>% 
  as.data.frame() %>% 
  mutate(cenario = cenario)

prob_exata_intervalo_HPD_2 <-
  attr(coda::HPDinterval(as.mcmc.list(posterior_2))[[1]], "Probability") %>% 
  as.data.frame() %>% 
  mutate(cenario = cenario)


hpd_quanti_2 <- 
  cbind(
    bayes_statistics_2,
    bayes_quantiles_2,
    intervalo_HPD_2
  ) %>% 
  select(Mean, SD, `2.5%`, `97.5%`, lower, upper) %>% 
  mutate(
    quantilico       = sprintf("[%.4f, %.4f]", `2.5%`, `97.5%`),
    ampli_quantilico = `97.5%` - `2.5%`,
    hpd              = sprintf("[%.4f, %.4f]", lower, upper),
    ampli_hpd        = upper - lower
  ) %>%
  select(
    quantilico, 
    ampli_quantilico, 
    hpd,
    ampli_hpd
  ) %>% 
  mutate(cenario = cenario)


# Sem 107 -----------------------------------------------------------------

cenario = "Sem 107"

posterior_3 <-
  outliers_logbin_bayes_posterior[[1]]

resumo_3 <-
  posterior_3 %>% 
  summary()

bayes_statistics_3 <- 
  resumo_3$statistics %>% 
  round(4) %>% 
  as.data.frame() %>% 
  mutate(cenario = cenario)

bayes_quantiles_3 <-  
  resumo_3$quantiles %>% 
  round(4) %>% 
  as.data.frame() %>% 
  mutate(cenario = cenario)


intervalo_HPD_3 <- 
  coda::HPDinterval(as.mcmc.list(posterior_3), prob = 0.95)[[1]] %>% 
  as.data.frame() %>% 
  mutate(cenario = cenario)

prob_exata_intervalo_HPD_3 <-
  attr(coda::HPDinterval(as.mcmc.list(posterior_3))[[1]], "Probability") %>% 
  as.data.frame() %>% 
  mutate(cenario = cenario)

hpd_quanti_3 <- 
  cbind(
    bayes_statistics_3,
    bayes_quantiles_3,
    intervalo_HPD_3
  ) %>% 
  select(Mean, SD, `2.5%`, `97.5%`, lower, upper) %>% 
  mutate(
    quantilico       = sprintf("[%.4f, %.4f]", `2.5%`, `97.5%`),
    ampli_quantilico = `97.5%` - `2.5%`,
    hpd              = sprintf("[%.4f, %.4f]", lower, upper),
    ampli_hpd        = upper - lower
  ) %>%
  select(
    quantilico, 
    ampli_quantilico, 
    hpd,
    ampli_hpd
  ) %>% 
  mutate(cenario = cenario)


# Sem 81 e 107 ------------------------------------------------------------

cenario = "Sem 81 e 107"

posterior_4 <-
  outliers_logbin_bayes_posterior[[1]]

resumo_4 <-
  posterior_4 %>% 
  summary()

bayes_statistics_4 <- 
  resumo_4$statistics %>% 
  round(4) %>% 
  as.data.frame() %>% 
  mutate(cenario = cenario)

bayes_quantiles_4 <-  
  resumo_4$quantiles %>% 
  round(4) %>% 
  as.data.frame() %>% 
  mutate(cenario = cenario)


intervalo_HPD_4 <- 
  coda::HPDinterval(as.mcmc.list(posterior_4), prob = 0.95)[[1]] %>% 
  as.data.frame() %>% 
  mutate(cenario = cenario)

prob_exata_intervalo_HPD_4 <-
  attr(coda::HPDinterval(as.mcmc.list(posterior_4))[[1]], "Probability") %>% 
  as.data.frame() %>% 
  mutate(cenario = cenario)

hpd_quanti_4 <- 
  cbind(
    bayes_statistics_4,
    bayes_quantiles_4,
    intervalo_HPD_4
  ) %>% 
  select(Mean, SD, `2.5%`, `97.5%`, lower, upper) %>% 
  mutate(
    quantilico       = sprintf("[%.4f, %.4f]", `2.5%`, `97.5%`),
    ampli_quantilico = `97.5%` - `2.5%`,
    hpd              = sprintf("[%.4f, %.4f]", lower, upper),
    ampli_hpd        = upper - lower
  ) %>%
  select(
    quantilico, 
    ampli_quantilico, 
    hpd,
    ampli_hpd
  ) %>% 
  mutate(cenario = cenario)

# Tabelas finais ----------------------------------------------------------



## estatística bayesiana -----

bayes_statistics <- 
  bind_rows(
    bayes_statistics_1,
    bayes_statistics_2,
    bayes_statistics_3,
    bayes_statistics_4
  ) %>% 
  select(cenario, everything()) %>% 
  mutate(across(where(is.numeric), ~ round(.x, 4)))

## estatística quantílica -----

bayes_quantiles <- 
  bind_rows(
    bayes_quantiles_1,
    bayes_quantiles_2,
    bayes_quantiles_3,
    bayes_quantiles_4
  ) %>% 
  select(cenario, everything()) %>% 
  mutate(across(where(is.numeric), ~ round(.x, 4)))

## intervalo hpd -----

intervalo_HPD <- 
  bind_rows(
    intervalo_HPD_1,
    intervalo_HPD_2,
    intervalo_HPD_3,
    intervalo_HPD_4
  ) %>% 
  select(cenario, everything()) %>% 
  mutate(across(where(is.numeric), ~ round(.x, 4)))



## prob exata -----

prob_exata_intervalo_HPD <- 
  bind_rows(
    prob_exata_intervalo_HPD_1,
    prob_exata_intervalo_HPD_2,
    prob_exata_intervalo_HPD_3,
    prob_exata_intervalo_HPD_4
  ) %>% 
  select(cenario, everything()) %>% 
  rename(probabilidade_exata = ".") %>% 
  mutate(across(where(is.numeric), ~ round(.x, 4)))

## quantilico e hpd ----

hpd_quanti <- 
  bind_rows(
    hpd_quanti_1,
    hpd_quanti_2,
    hpd_quanti_3,
    hpd_quanti_4
  ) %>% 
  tibble::rownames_to_column(var = "coeficiente") %>% 
  mutate(coeficiente = sub("\\.\\.\\.[0-9]+$", "", coeficiente)) %>% 
  select(cenario, coeficiente, everything()) %>% 
  mutate(across(where(is.numeric), ~ round(.x, 4)))


# saída latex -------------------------------------------------------------

library(knitr)
library(kableExtra)

## estatística bayesiana -----

bayes_statistics %>% 
  kbl(
    format = "latex",
    booktabs = TRUE, 
    align = "lccccc", 
    caption = "XXXXXXXXXXXXXXX"
  ) 

## estatística quantílica -----

bayes_quantiles %>% 
  kbl(
    format = "latex",
    booktabs = TRUE, 
    align = "lccccc", 
    caption = "XXXXXXXXXXXXXXX"
  ) 

## intervalo hpd -----

intervalo_HPD %>% 
  kbl(
    format = "latex",
    booktabs = TRUE, 
    align = "lccccc", 
    caption = "XXXXXXXXXXXXXXX"
  ) 



## prob exata -----

prob_exata_intervalo_HPD %>% 
  kbl(
    format = "latex",
    booktabs = TRUE, 
    align = "lccccc", 
    caption = "XXXXXXXXXXXXXXX"
  ) 

## quantilico e hpd ----

hpd_quanti %>% 
  kbl(
    format = "latex",
    booktabs = TRUE, 
    align = "lccccc", 
    caption = "XXXXXXXXXXXXXXX"
    ) 
