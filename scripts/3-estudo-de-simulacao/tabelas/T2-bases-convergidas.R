rm(list = ls())

# pacotes -----------------------------------------------------------------
require(dplyr)
library(knitr)
library(kableExtra)

# leitura -----------------------------------------------------------------
base_conv <- 
  read.csv( "saidas/2-saida-simulacao/bases-convergidas.csv") 

# tratamento --------------------------------------------------------------
bd_conv <- 
  base_conv %>% 
  mutate(
    amostra_categoria = sub("_replica_[0-9]+$", "", modelo),
    amostra_categoria = stringr::str_sub(amostra_categoria, 9),
    amostra_categoria = as.numeric(amostra_categoria)
  ) %>% 
  group_by(amostra_categoria, convergencia) %>%
  summarise(
    n = n()
  ) %>% 
  mutate(
    perc =  scales::number(100 * n / sum(n), accuracy = 0.1)
  ) %>% 
  ungroup() %>% 
  arrange(amostra_categoria) %>% 
  mutate(valor = paste0(n, " (",perc,")%" )) %>% 
  select(amostra_categoria, convergencia, valor)


# tabela ------------------------------------------------------------------
resultado_conv <- 
  bd_conv %>% 
  tidyr::pivot_wider(
    names_from = convergencia,
    values_from = c(valor),
    names_sep = "_"
  ) 


# latex -------------------------------------------------------------------
kbl(resultado_conv, format = "latex", booktabs = TRUE, align = "crr",
    caption = "Tabela status de convergência por tamanho amostral.") %>%
  # add_header_above(c(" " = 1, 
  #                    "Convergiu" = 1, 
  #                    "Não convergiu" = 1)) %>%
  # add_header_above(c(" " = 1, 
  #                    " " = 1, " " = 1)) %>%
  kable_styling(latex_options = c("hold_position", "striped"))

