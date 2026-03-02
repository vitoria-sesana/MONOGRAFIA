rm(list = ls())

coefs <- read.csv("E-NOVA-SIMULACAO/0-tabelas/nao_convergiram_coeficientes.csv") %>% select(-X)
vies <- read.csv("E-NOVA-SIMULACAO/0-tabelas/nao_convergiram_vies.csv") %>% select(-X)
cob_amp <- read.csv("E-NOVA-SIMULACAO/0-tabelas/nao_convergiram_cob_amp.csv") %>% select(-X)

# latex -------------------------------------------------------------------
library(knitr)
library(kableExtra)

# coefs -----
kbl(coefs, format = "latex", booktabs = TRUE, align = "lcrrrrrrrr",
    caption = "nao convergiram: media e desvio-padrão") %>%
  add_header_above(c(" " = 1, " " = 1, 
                     "Freq" = 2, 
                     "Pois" = 2, 
                     "Bayes Media" = 2,
                     "Bayes Mediana" = 2
  )) %>%
  add_header_above(c(" " = 1, " " = 1, 
                     " " = 1, " " = 1,
                     " " = 1, " " = 1,
                     " " = 1, " " = 1,
                     " " = 1, " " = 1)) %>%
  kable_styling(latex_options = c("hold_position", "striped"))

# vies -----
kbl(vies, format = "latex", booktabs = TRUE, align = "lcrrrrrrrr",
    caption = "nao convergiram: vies") %>%
  add_header_above(c(" " = 1, " " = 1, 
                     "Freq" = 2, 
                     "Pois" = 2, 
                     "Bayes Media" = 2,
                     "Bayes Mediana" = 2
  )) %>%
  add_header_above(c(" " = 1, " " = 1, 
                     " " = 1, " " = 1,
                     " " = 1, " " = 1,
                     " " = 1, " " = 1,
                     " " = 1, " " = 1)) %>%
  kable_styling(latex_options = c("hold_position", "striped"))

# cob_amp -----
kbl(cob_amp, format = "latex", booktabs = TRUE, align = "lcrrrrrrrr",
    caption = "nao convergiram: prob e amplitude") %>%
  add_header_above(c(" " = 1, " " = 1, 
                     " " = 1, 
                     "Pois" = 2, 
                     "Bayes Media" = 2
  )) %>%
  add_header_above(c(" " = 1, " " = 1, 
                     " " = 1, 
                     " " = 1, " " = 1,
                     " " = 1, " " = 1)) %>%
  kable_styling(latex_options = c("hold_position", "striped"))
