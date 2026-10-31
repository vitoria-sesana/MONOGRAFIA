# tabela resultados das estimativas dos modelos sob diferentes chutes iniciais

# rotina ------------------------------------------------------------------
source("scripts/0-rotina.R", encoding = "UTF-8")

# Leitura -----------------------------------------------------------------

chutes_logbin_freq <- 
  "saidas/1-saida-aplicacao/chutes_iniciais_log_binomial_frequentista_estimativas.rds" %>% 
  readRDS() %>% 
  mutate(
    across(where(is.numeric), ~ round(.x, 4))
  ) %>% 
  mutate(
    across(
      where(is.numeric),
      ~ sprintf("%.4f", .x)
    )
  ) %>% 
  mutate(
    modelo = "Log-binomial frequentista" 
  )  %>% 
  select(
    modelo,
    Chute_inicial,
    `(Intercept)`,
    center1,
    treatP,
    baseline0
  ) %>% 
  janitor::clean_names()

chutes_pois_robusto <- 
  "saidas/1-saida-aplicacao/chutes_iniciais_poisson_robusto_estimativas.rds" %>% 
  readRDS() %>%  
  mutate(
    across(where(is.numeric), ~ round(.x, 4))
  ) %>% 
  bind_rows(
    tibble::tibble(
      Chute_inicial = "chute10",
      !!!setNames(
        as.list(rep(NA_real_, ncol(.) - 1)),
        setdiff(names(.), "Chute_inicial")
      )
    )
  ) %>% 
  mutate(
    across(
      where(is.numeric),
      ~ sprintf("%.4f", .x)
    )
  ) %>% 
  mutate(
    modelo = "Poisson robusto"
  ) %>% 
  select(
    modelo,
    Chute_inicial,
    `(Intercept)`,
    center1,
    treatP,
    baseline0
  ) %>% 
  janitor::clean_names()

chutes_logbin_bayes <- 
  "saidas/1-saida-aplicacao/chutes_iniciais_log_binomial_bayesiano_estimativas.rds" %>% 
  readRDS() %>% 
  mutate(
    Chute_inicial = paste0("chute", chute),
    modelo = 
      case_when(
        estatistica == "Media" ~ "Log-binomial bayesiano (média)",
        estatistica == "Mediana" ~ "Log-binomial bayesiano (mediana)",
        TRUE ~ estatistica
        )
    ) %>% 
  mutate(
    across(where(is.numeric), ~ round(.x, 4))
  ) %>% 
  mutate(
    across(
      where(is.numeric),
      ~ sprintf("%.4f", .x)
    )
  ) %>% 
  rename(
    `(Intercept)` = b0,
    center1 = b1,
    treatP = b2,
    baseline0 = b3
  ) %>% 
  select(
    modelo,
    Chute_inicial,
    `(Intercept)`,
    center1,
    treatP,
    baseline0
  ) %>% 
  janitor::clean_names()



# tabela com todas as estimativas -----------------------------------------

tabela_consolidada_chutes_iniciais <- 
  bind_rows(
    chutes_logbin_freq,
    chutes_pois_robusto,
    chutes_logbin_bayes
  )  %>%
  mutate(
    across(everything(), ~ ifelse(is.na(.x), "-", as.character(.x)))
  ) %>% 
  mutate(
    chute_inicial = gsub("chute(\\d+)", "Chute \\1", chute_inicial)
  ) %>% 
  arrange(readr::parse_number(chute_inicial))

# Saída -------------------------------------------------------------------

kbl(
  tabela_consolidada_chutes_iniciais,
  format = "latex",
  booktabs = TRUE,
  caption = "Tabela de resultados",
  label = "tab:resultados",
  align = "lcccccccc"
)

