

require(logbin)
require(geepack)
require(dplyr)
require(sandwich)

# leitura dados -----------------------------------------------------------
data(respiratory, package="geepack")
respiratory$center <- factor(respiratory$center, levels = c("2", "1"))
respiratory$baseline <- factor(respiratory$baseline, levels = c("1", "0"))
respiratory4 <- subset(respiratory, visit == 4)

# ajuste logbin -----------------------------------------------------------
modelo_logbin <- 
  logbin::logbin(
    outcome ~ center  + treat + baseline, 
    data=respiratory4
  ) 

# ajuste poisson ----------------------------------------------------------
modelo_poisson <- 
  glm(
    outcome ~ center  + treat + baseline,
    family = poisson(link=log), 
    data = respiratory4
  )

modelo_poisson_sandwich <- 
  lmtest::coeftest(modelo_poisson, vcov = sandwich::sandwich)

modelo_poisson_sandwich %>% confint()
modelo_poisson_sandwich %>% class()



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


