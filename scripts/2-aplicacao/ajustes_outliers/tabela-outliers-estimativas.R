# tabela resultados das estimativas dos modelos sob inclusão e exclusão de outliers

# rotina ------------------------------------------------------------------
source("scripts/0-rotina.R", encoding = "UTF-8")

# Leitura -----------------------------------------------------------------

outliers_logbin_freq <- 
  "saidas/1-saida-aplicacao/outliers_log_binomial_frequentista_estimativas.rds" %>% 
  readRDS() 

outliers_pois_robusto <- 
  "saidas/1-saida-aplicacao/outliers_poisson_robusto_estimativas.rds" %>% 
  readRDS() 

outliers_logbin_bayes <- 
  "saidas/1-saida-aplicacao/outliers_log_binomial_bayesiano_estimativas.rds" %>% 
  readRDS() %>% 
  rename(modelo = estatistica) %>% 
  mutate(
    modelo = 
      case_when(
        modelo == "Media" ~ "Log-binomial bayesiano (média)",
        modelo == "Mediana" ~ "Log-binomial bayesiano (mediana)",
        TRUE ~ modelo
      )
  ) 


# tabela com todas as estimativas -----------------------------------------

tabela_consolidada_outliers <- 
  bind_rows(
    outliers_logbin_freq,
    outliers_pois_robusto,
    outliers_logbin_bayes
  )  %>%
  mutate(
    across(everything(), ~ ifelse(is.na(.x), "-", as.character(.x)))
  ) 

# Saída -------------------------------------------------------------------

kbl(
  tabela_consolidada_outliers,
  format = "latex",
  booktabs = TRUE,
  caption = "Tabela de resultados",
  label = "tab:resultados",
  align = "lcccccccc"
)



# saidas latex ------------------------------------------------------------

# tabela com os resultados 
kbl(tabela_final, format = "latex", booktabs = TRUE, align = "lcccccccc", caption = "Modelo com Métricas") %>%
  add_header_above(c(" " = 1, 
                     " " = 1, " " = 1,
                     " " = 1, " " = 1,
                     " " = 1, " " = 1,
                     " " = 1, " " = 1)) %>%
  kable_styling(latex_options = c("hold_position", "striped"))


##

library(kableExtra)

tab <- kbl(
  tabela_final,
  format = "latex",
  booktabs = TRUE,
  align = "lcccccccc",
  caption = "Modelo com Métricas"
) %>%
  add_header_above(c(
    " " = 1, " " = 1, " " = 1,
    " " = 1, " " = 1, " " = 1,
    " " = 1, " " = 1, " " = 1
  )) %>%
  kable_styling(latex_options = "hold_position")

# Cinza em duas linhas, branco nas duas seguintes
for (i in seq_len(nrow(tabela_final))) {
  if ((i - 1) %% 4 < 2) {
    tab <- tab %>%
      row_spec(i, background = "gray!10")
  }
}

tab

