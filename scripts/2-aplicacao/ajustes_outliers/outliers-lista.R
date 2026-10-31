# ajuste do modelo log-binomial frequentista com a retirada de outliers

# rotina ------------------------------------------------------------------
source("scripts/0-rotina.R", encoding = "UTF-8")

# outliers ----------------------------------------------------------------
outliers <- c(81, 107)

tabela_outliers <- 
  respiratory4 %>% 
  filter(
    id %in% outliers
  ) %>% 
  select(-id_centro) %>% 
  # select(id, everything()) %>% 
  select(id, center, treat, baseline, outcome) %>%
  arrange(
    id
  )

# saída latex -------------------------------------------------------------

kbl(
  tabela_outliers, 
  format = "latex", 
  booktabs = TRUE, 
  align = "lcccccccc", 
  caption = "xxxxxxxxxxxxxxxxx"
) 

