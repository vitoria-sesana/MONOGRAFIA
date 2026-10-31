# rotina ------------------------------------------------------------------
source("scripts/0-rotina.R", encoding = "UTF-8")


# Cenário 1 ---------------------------------------------------------------

mv1 <- 
  read.csv(
    "saidas/3-saida-simulacao-completo/C-saidas/C-resultados/C-T1_apendice_mean_vies.csv"
  )
  
sr1 <-  
  read.csv(
    "saidas/3-saida-simulacao-completo/C-saidas/C-resultados/C-T2_apendice_sd_reqm.csv"
  )
  
pa1 <-  
  read.csv(
    "saidas/3-saida-simulacao-completo/C-saidas/C-resultados/C-T3_apendice_pc_am.csv"
  )


# Cenário 2 ---------------------------------------------------------------

mv2 <- 
  read.csv(
    "saidas/3-saida-simulacao-completo/NC-saidas/NC-resultados/NC-T1_apendice_mean_vies.csv"
  )

sr2 <-
  read.csv(
    "saidas/3-saida-simulacao-completo/NC-saidas/NC-resultados/NC-T2_apendice_sd_reqm.csv"
  )
  
pa2 <-   
  read.csv(
    "saidas/3-saida-simulacao-completo/NC-saidas/NC-resultados/NC-T3_apendice_pc_am.csv"
  )


# Tabela final ------------------------------------------------------------

mv <- 
  bind_rows(
    mv1,
    mv2
  ) %>% 
  arrange(
    cenario, parametro
  ) %>% 
  select(-X)



sr <- 
  bind_rows(
    sr1,
    sr2, 
  ) %>% 
  arrange(
    cenario, parametro
  ) %>% 
  select(-X)



pa <- 
  bind_rows(
    pa1,
    pa2
  ) %>% 
  arrange(
    cenario, parametro
  ) %>% 
  select(-X)



# saída -------------------------------------------------------------------

mv
sr
pa


# Média e viés ------------------------------------------------------------


kbl(
  mv,
  format = "latex",
  booktabs = TRUE,
  align = "rrrrrrrrrr",
  caption = "XXXXXXXXXXX"
) 


# desvio-padrão e REQM ----------------------------------------------------

kbl(
  sr,
  format = "latex",
  booktabs = TRUE,
  align = "rrrrrrrrrr",
  caption = "XXXXXXXXXXX"
) 




# probabilidade cobertura e amplitude média -------------------------------


kbl(
  pa,
  format = "latex",
  booktabs = TRUE,
  align = "rrrrrrrrrr",
  caption = "XXXXXXXXXXX"
) 


