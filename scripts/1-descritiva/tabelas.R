# Tabela

# rotina ------------------------------------------------------------------
source("scripts/0-rotina.R", encoding = "UTF-8")

# variáveis dicotômicas  --------------------------------------------------
tab_dicotomicas <- 
  respiratory4 %>% 
  select(
    baseline, center, outcome, sex, treat) %>%
    mutate(across(everything(), as.character)) %>%   
    tidyr::pivot_longer(cols = everything(), 
              names_to = "variavel", 
              values_to = "valor") %>% 
  group_by(variavel, valor) %>% 
  summarise(Frequencia = n(), .groups = "drop") %>% 
  group_by(variavel) %>% 
  mutate(Percentual = ((100 * Frequencia) / nrow(respiratory4)))

# Variável idade ----------------------------------------------------------

tab_idade <-  
  summary(respiratory4$age) %>% 
  as.matrix() %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column() %>% 
  rbind( c("sd", sd(respiratory4$age))) %>%  
  mutate(V1 = round(as.numeric(V1), 1))

# tabela contingência  ----------------------------------------------------

tab_contigencia <- 
  respiratory4 %>% 
  select(baseline, center, outcome, treat) %>%
  count(center, baseline, treat, outcome) %>% 
  group_by(center, baseline) %>% 
  mutate(
    prop = n / 111,
    tabela = paste0(n, " (", round(prop * 100, 1), "%)")
  ) %>% 
  ungroup() %>% 
  select(-n, -prop) %>% 
  tidyr::pivot_wider(
    names_from = c(treat, outcome),
    values_from = tabela,
    values_fill = "0 (0%)"
  ) %>% 
  arrange(desc(center), baseline)

