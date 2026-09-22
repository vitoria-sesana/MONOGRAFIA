rm(list = ls())
source('scripts/0-rotina.R', encoding = 'UTF-8')

# Leitura -----------------------------------------------------------------

modelo_logbin <- readRDS("saidas/1-saida-aplicacao/ajuste_logbin_frequentista.rds")$modelo_logbin_frequentista
modelo_poisson_sandwich <- readRDS("saidas/1-saida-aplicacao/ajuste_poisson_robusto.rds")
modelo_poisson_sandwich <- readRDS("saidas/1-saida-aplicacao/ajuste_poisson_robusto.rds")




# tratamento --------------------------------------------------------------

# logbin frequentista
coef_logbin_freq <- 
  summary(modelo_logbin)$coefficients; coef_logbin_freq

ic_logbin_freq <- 
  confint(modelo_logbin); ic_logbin_freq

resultado_logbin_freq <- 
  merge(
    as.data.frame(coef_logbin_freq),
    as.data.frame(ic_logbin_freq),
    by = "row.names") %>%
  mutate(across(where(is.numeric), ~ round(., 4))) %>% 
  mutate(modelo = "Log-binomial frequentista")


# poisson clássico
coef_pois_classico <- 
  summary(modelo_poisson)$coefficients; coef_pois_classico

ic_pois_classico <- 
  confint(modelo_poisson); ic_pois_classico

resultado_pois_classico <- 
  merge(
    as.data.frame(coef_pois_classico),
    as.data.frame(ic_pois_classico),
    by = "row.names") %>%
  mutate(across(where(is.numeric), ~ round(., 4))) %>% 
  mutate(modelo = "Poisson clássico")

# poisson robusto
modelo_poisson_sandwich
modelo_poisson_sandwich %>% as.data.frame.array()
confint(modelo_poisson_sandwich)

coef_pois_robusto <- 
  as.data.frame.array(modelo_poisson_sandwich); coef_pois_robusto

ic_pois_robusto <- 
  confint(modelo_poisson_sandwich); ic_pois_robusto

resultado_pois_robusto <- 
  merge(
    as.data.frame(coef_pois_robusto),
    as.data.frame(ic_pois_robusto),
    by = "row.names") %>%
  mutate(across(where(is.numeric), ~ round(., 4))) %>% 
  mutate(modelo = "Poisson robusto")

# completo
resultado_logbin_freq
resultado_pois_classico
resultado_pois_robusto

tabela_estimativas_aplicacao <- 
  rbind(
  resultado_logbin_freq,
  resultado_pois_classico,
  resultado_pois_robusto
  ) %>% 
  janitor::clean_names() %>% 
  mutate(
    row_names = factor(
      row_names,
      levels = c(
        "(Intercept)",
        "center1",
        "treatP",
        "baseline0"
        )
      )
  ) %>% 
  arrange(modelo, row_names) %>% 
  select(
    modelo, 
    row_names, 
    estimate, 
    std_error, 
    x2_5_percent, 
    x97_5_percent,
    z_value, 
    pr_z
    )

# saída -------------------------------------------------------------------
tabela_estimativas_aplicacao
kbl(
  tabela_estimativas_aplicacao, 
  format = "latex",
  booktabs = TRUE, 
  align = "lccccccc", 
  caption = "XXXXXXXXXXXXXXX") %>%
  kable_styling(latex_options = c("hold_position", "striped"))


