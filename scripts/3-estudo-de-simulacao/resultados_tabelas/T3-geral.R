rm(list = ls())

# leitura dos resultados --------------------------------------------------

media_sd <- 
  read.csv("saidas/2-saida-simulacao/R2-geral/geral_media_sd.csv") %>% 
  select(-X)

vies_reqm <- 
  read.csv("saidas/2-saida-simulacao/R2-geral/geral_vies_reqm.csv") %>% 
  select(-X)

pc_am <- 
  read.csv("saidas/2-saida-simulacao/R2-geral/geral_pc_am.csv") %>% 
  select(-X)


# latex -------------------------------------------------------------------
library(knitr)
library(kableExtra)

## coefs -----
kbl(media_sd, format = "latex", booktabs = TRUE, align = "lcrrrrrrrr",
    caption = "geral: media e desvio-padrão") %>%
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

## vies -----
kbl(vies_reqm, format = "latex", booktabs = TRUE, align = "lcrrrrrrrr",
    caption = "geral: vies") %>%
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

## cob_amp -----
kbl(pc_am, format = "latex", booktabs = TRUE, align = "lcrrrrrrrr",
    caption = "geral: prob e amplitude") %>%
  add_header_above(c(" " = 1, " " = 1, 
                     "Freq" = 2, 
                     "Pois" = 2, 
                     "Bayes Media" = 2
  )) %>%
  add_header_above(c(" " = 1, " " = 1, 
                     " " = 1, " " = 1,
                     " " = 1, " " = 1,
                     " " = 1, " " = 1)) %>%
  kable_styling(latex_options = c("hold_position", "striped"))

