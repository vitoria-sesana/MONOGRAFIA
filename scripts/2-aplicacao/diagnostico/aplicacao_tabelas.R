# rotina ------------------------------------------------------------------
source("scripts/0-rotina.R", encoding = "UTF-8")

# ajuste logbin -----------------------------------------------------------
modelo_logbin_frequentista <-
  readRDS("saidas/1-saida-aplicacao/ajuste_logbin_frequentista.rds")

modelo_logbin_frequentista <- modelo_logbin_frequentista$modelo_logbin_frequentista 

# ajuste poisson ----------------------------------------------------------
modelo_poisson_sandwich <- 
  readRDS("saidas/1-saida-aplicacao/ajuste_poisson_robusto.rds")

modelo_poisson_sandwich <- modelo_poisson_sandwich$modelo_poisson

# ajuste logbinomial bayesiano -----------------------------------------
modelo_logbin_bayesiano <- 
  readRDS("saidas/1-saida-aplicacao/ajuste_logbin_bayesiano.rds")

modelo_logbin_bayesiano <- modelo_logbin_bayesiano$resultado_bayesiano

# analises ----------------------------------------------------------------


# modelos
modelo_logbin_frequentista
modelo_poisson_sandwich

# coeficiente
coef(modelo_logbin_frequentista)
coef(modelo_poisson_sandwich)

# intervalo de confiança
modelo_logbin_frequentista %>% confint()
modelo_poisson_sandwich %>% confint()

# resumo
summary(modelo_logbin_frequentista)
summary(modelo_poisson_sandwich)

# significancia
summary(modelo_logbin_frequentista)
modelo_poisson_sandwich

# coeficientes ------------------------------------------------------------

coef_logbin <- 
  modelo_logbin_frequentista %>% 
  coef() %>% 
  as.data.frame() %>% 
  mutate(
    parametro = c("Intercepto", "Center", "Treat", "Baseline"),
    modelo = "Log-Binomial Frequentista"
  ) %>% 
  rename(coef = ".") %>% 
  mutate(exp_coef = exp(coef))

coef_poisson_sandwich <- 
  modelo_poisson_sandwich %>% 
  coef() %>% 
  as.data.frame() %>% 
  mutate(
    parametro = c("Intercepto", "Center", "Treat", "Baseline"),
    modelo = "Poisson Robusto"
  ) %>% 
  rename(coef = ".") %>% 
  mutate(exp_coef = exp(coef))


coef_logbin_bayesiano_media <-
  modelo_logbin_bayesiano$statistics %>% 
  as.data.frame() %>% 
  select(Mean) %>% 
  mutate(
    parametro = c("Intercepto", "Center", "Treat", "Baseline"),
    modelo = "Log-Binomial Bayesiano (média)"
  ) %>% 
  rename(coef = Mean) %>% 
  mutate(exp_coef = exp(coef))


coef_logbin_bayesiano_mediana <-
  modelo_logbin_bayesiano$quantiles %>%  
  as.data.frame() %>% 
  select(`50%`) %>% 
  mutate(
    parametro = c("Intercepto", "Center", "Treat", "Baseline"),
    modelo = "Log-Binomial Bayesiano (mediana)"
  ) %>% 
  rename(coef = `50%`) %>% 
  mutate(exp_coef = exp(coef))

coef_logbin
coef_poisson_sandwich
coef_logbin_bayesiano_media
coef_logbin_bayesiano_mediana

## coefs final ------
coefs <- 
  rbind(
    coef_logbin,
    coef_poisson_sandwich,
    coef_logbin_bayesiano_media,
    coef_logbin_bayesiano_mediana
  )

rownames(coefs) <- NULL

coefs_final <- 
  coefs %>%
  mutate(
    coef = round(coef, 4),
    exp_coef = round(exp_coef, 4)
  ) %>% 
  tidyr::pivot_wider(
    names_from = parametro,
    values_from = c(coef,exp_coef),
    names_sep = "_"
  ) %>% 
  select(
    modelo,
    coef_Intercepto, exp_coef_Intercepto,
    coef_Center, exp_coef_Center,
    coef_Treat, exp_coef_Treat,
    coef_Baseline, exp_coef_Baseline
  )

# colnames(coefs_final) <-
#   c(
#     "Modelo", 
#     "$\beta_{0}$", "$exp(\beta_{0})$",
#     "$\beta_{1}$", "$exp(\beta_{1})$",
#     "$\beta_{2}$", "$exp(\beta_{2})$",
#     "$\beta_{3}$", "$exp(\beta_{3})$"
#     )
# latex -------------------------------------------------------------------


library(knitr)
library(kableExtra)
kbl(coefs_final, format = "latex", booktabs = TRUE, align = "lcccccccc", caption = "Modelo com Métricas") %>%
  add_header_above(c(" " = 1, 
                     "(Intercept)" = 2, 
                     "center2" = 2, 
                     "treatP" = 2, 
                     "baseline" = 2)) %>%
  add_header_above(c(" " = 1, 
                     " " = 1, " " = 1,
                     " " = 1, " " = 1,
                     " " = 1, " " = 1,
                     " " = 1, " " = 1)) %>%
  kable_styling(latex_options = c("hold_position", "striped"))



