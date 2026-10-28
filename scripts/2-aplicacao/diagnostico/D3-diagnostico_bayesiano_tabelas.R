rm(list = ls())

# rotina ------------------------------------------------------------------
source("scripts/0-rotina.R", encoding = "UTF-8")

# Leitura -----------------------------------------------------------------
resultados_aplicacao_bayesiana <- 
  readRDS(
    "saidas/1-saida-aplicacao/ajuste_logbin_bayesiano.rds"
    )

# resultados --------------------------------------------------------------

posterior <- resultados_aplicacao_bayesiana$posterior

## transformar posterior em data frame 
# posterior_df <- as.data.frame(as.matrix(posterior))

posterior

resumo <- summary(posterior); resumo

bayes_statistics <- resumo$statistics %>% round(4); bayes_statistics
bayes_quantiles <-  resumo$quantiles %>% round(4); bayes_quantiles

# tabela todos os modelos -------------------------------------------------

# média
bayes_statistics %>% 
  as.data.frame() %>% 
  select(Mean) %>% 
  mutate(EXPMean = exp(Mean)) %>% 
  round(4)

# mediana
bayes_quantiles %>% as.data.frame() %>% 
  select(`50%`) %>% 
  mutate(EXPMean = exp(`50%`)) %>% 
  round(4)

# saveRDS(posterior, "E-NOVA-SIMULACAO/saida_real_posterior.rds")


# HPD ---------------------------------------------------------------------
intervalo_HPD <- 
  coda::HPDinterval(posterior, prob = 0.95)[[1]] %>% 
  as.data.frame(); intervalo_HPD

prob_exata_intervalo_HPD <-
  attr(coda::HPDinterval(posterior)[[1]], "Probability"); prob_exata_intervalo_HPD

# resultados gerais -------------------------------------------------------
bayes_statistics 
bayes_quantiles
intervalo_HPD

resultados_gerais <- 
  cbind(
    bayes_statistics,
    bayes_quantiles,
    intervalo_HPD
    ) %>% 
  select(Mean, SD, `2.5%`, `97.5%`, lower, upper) %>% 
  round(4)


# quantilico x HPD --------------------------------------------------------


resultados_quantilico_hpd <- resultados_gerais %>%
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
  )
  
# saídas latex --------------------------------------------------------------
library(knitr)
library(kableExtra)

## tabela quantis x hpd -------------------
bayes_statistics
kbl(bayes_statistics, format = "latex", booktabs = TRUE, align = "lccccc", caption = "XXXXXXXXXXXXXXX") %>%
  kable_styling(latex_options = c("hold_position", "striped"))

## tabela quantis de credibilidade --------
bayes_quantiles
kbl(bayes_quantiles, format = "latex", booktabs = TRUE, align = "lrrrrr", caption = "XXXXXXXXXXXXXXX") %>%
  kable_styling(latex_options = c("hold_position", "striped"))

## tabela quantis x hpd -------------------
# Definindo os nomes das 5 colunas na parte inferior
# A 1ª coluna recebe vazio ("") e as seguintes recebem "Intervalo" e "Amplitude"
colnames(resultados_quantilico_hpd) <- c("", "Intervalo", "Amplitude", "Intervalo", "Amplitude")

# 2. Criação da tabela LaTeX com o cabeçalho mesclado
kbl(resultados_quantilico_hpd, format = "latex", booktabs = TRUE, align = "lcccc", caption = "XXXXXXXXXXXXXXX") %>%
  add_header_above(c(" " = 1, "Quantílico" = 2, "HPD" = 2)) %>%
  kable_styling(latex_options = c("hold_position", "striped"))
