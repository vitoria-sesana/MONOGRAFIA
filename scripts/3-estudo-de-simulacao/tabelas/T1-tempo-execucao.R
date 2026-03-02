library(knitr)
library(kableExtra)

# tempo de execução -------------------------------------------------------
tempo_logbin_freq <- readRDS("saidas/2-saida-simulacao/M1-logbin-frequentista/tempo_execucao_logbin_frequentista.rds")
tempo_poisson_sandwich <- readRDS("saidas/2-saida-simulacao/M2-poisson-robusto/tempo_execucao_ajustes_poisson_robusto.rds") 
tempo_logbin_bayesiano <- readRDS("saidas/2-saida-simulacao/M3-logbin-bayesiano/tempo_execucao_ajustes_logbin_bayesiano.rds")

# tabela ------------------------------------------------------------------
resultado_tempo <- 
  data.frame(
    modelo = c("Log-Binomial Frequentista", "Poisson Robusto", "Log-Binomial Bayesiano"),
    c = c(tempo_logbin_freq, tempo_poisson_sandwich, tempo_logbin_bayesiano)
  ) %>% 
  mutate(
    hms = round(lubridate::seconds_to_period(c),4),
    c = round(c,4)
  )

# latex -------------------------------------------------------------------
kbl(resultado_tempo, format = "latex", booktabs = TRUE, align = "crr",
    caption = "Tabela tempo de execução dos ajustes de cada modelo.") %>%
  # add_header_above(c(" " = 1, 
  #                    "Convergiu" = 1, 
  #                    "Não convergiu" = 1)) %>%
  # add_header_above(c(" " = 1, 
  #                    " " = 1, " " = 1)) %>%
  kable_styling(latex_options = c("hold_position", "striped"))
