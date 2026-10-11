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

# definição das combinações -----------------------------------------------

dados_cenarios <- list(
  "Com ambas (81 e 107)" = respiratory4,
  "Somente sem 81" = respiratory4 %>%
    filter(id != 81),
  "Somente sem 107" = respiratory4 %>%
    filter(id != 107),
  "Sem ambas (81 e 107)" = respiratory4 %>%
    filter(!id %in% c(81, 107))
)


# ajustes log-binomiais ---------------------------------------------------

modelos_logbin <- lapply(dados_cenarios, function(dados) {
  logbin::logbin(
    outcome ~ center + treat + baseline,
    data = dados
  )
})


# ajustes poisson robusto -------------------------------------------------

modelos_poisson <- lapply(dados_cenarios, function(dados) {
  glm(
    outcome ~ center + treat + baseline,
    family = poisson(link = "log"),
    data = dados
  )
})


# função para extração dos resultados --------------------------------------

extrair_resultados <- function(modelo, cenario) {
  
  tab <- as.data.frame(
    summary(modelo)$coefficients
  )
  
  nomes <- tolower(
    gsub("[^[:alnum:]]", "", names(tab))
  )
  
  # Identificar as colunas da tabela de coeficientes
  localizar <- function(padroes) {
    for (p in padroes) {
      idx <- which(grepl(p, nomes))
      if (length(idx) > 0) return(idx[1])
    }
    NA_integer_
  }
  
  col_est <- localizar(c("^estimate$", "^est$", "^coef$"))
  col_ep  <- localizar(c("^stderror$", "^stderr$", "^se$"))
  col_z   <- localizar(c("^zvalue$", "^zstatistic$", "^tvalue$"))
  col_p   <- localizar(c("^pr", "^pvalue$"))
  
  if (is.na(col_est) || is.na(col_ep)) {
    stop(
      "Não foi possível identificar as colunas de estimativa e ",
      "erro-padrão. Confira summary(modelo)$coefficients."
    )
  }
  
  estimativa <- tab[[col_est]]
  erro_padrao <- tab[[col_ep]]
  
  # Estatística z: usar a fornecida pelo summary, se disponível
  z <- if (!is.na(col_z)) {
    tab[[col_z]]
  } else {
    estimativa / erro_padrao
  }
  
  # Valor-p: usar o fornecido pelo summary, se disponível
  valor_p <- if (!is.na(col_p)) {
    tab[[col_p]]
  } else {
    2 * pnorm(-abs(z))
  }
  
  tibble(
    Cenario = cenario,
    Coeficiente = rownames(tab),
    Estimativa = estimativa,
    `Erro-padrão` = erro_padrao,
    `2.5%` = estimativa - qnorm(0.975) * erro_padrao,
    `97.5%` = estimativa + qnorm(0.975) * erro_padrao,
    z = z,
    `Valor-p` = valor_p
  )
}

extrair_poisson_robusto <- function(modelo, cenario) {
  
  # Matriz de covariância robusta
  vcov_robusta <- sandwich::sandwich(modelo)
  
  # Coeficientes com erros-padrão robustos
  teste <- lmtest::coeftest(
    modelo,
    vcov. = vcov_robusta
  )
  
  estimativa <- teste[, 1]
  erro_padrao <- teste[, 2]
  z <- estimativa / erro_padrao
  valor_p <- 2 * pnorm(-abs(z))
  
  # Intervalo de confiança de Wald de 95%
  z_critico <- qnorm(0.975)
  
  tibble(
    Cenario = cenario,
    Coeficiente = rownames(teste),
    Estimativa = estimativa,
    `Erro-padrão` = erro_padrao,
    `2.5%` = estimativa - z_critico * erro_padrao,
    `97.5%` = estimativa + z_critico * erro_padrao,
    z = z,
    `Valor-p` = valor_p
  )
}


# resultados --------------------------------------------------------------

# logbinomial
tabela_final_logbin_frequentista <- bind_rows(
  lapply(names(modelos_logbin), function(cenario) {
    extrair_resultados(
      modelo = modelos_logbin[[cenario]],
      cenario = cenario
    )
  })
) %>%
  mutate(
    Cenario = factor(
      Cenario,
      levels = names(dados_cenarios)
    )
  ) %>%
  arrange(Cenario) %>%
  mutate(
    across(
      c(
        Estimativa, `Erro-padrão`,
        `2.5%`, `97.5%`, z, `Valor-p`
      ),
      ~ round(.x, 4)
    )
  ) %>%
  select(
    Cenario,
    Coeficiente,
    Estimativa,
    `Erro-padrão`,
    `2.5%`,
    `97.5%`,
    z,
    `Valor-p`
  ) %>% 
  mutate(
    modelo = "Log-binomial frequentista"
  ) %>% 
  select(modelo, everything())

# poisson robusto
tabela_final_poisson_robusto <- bind_rows(
  lapply(names(modelos_poisson), function(cenario) {
    extrair_poisson_robusto(
      modelo = modelos_poisson[[cenario]],
      cenario = cenario
    )
  })
) %>%
  mutate(
    Cenario = factor(
      Cenario,
      levels = names(dados_cenarios)
    )
  ) %>%
  arrange(Cenario) %>%
  mutate(
    across(
      c(
        Estimativa, `Erro-padrão`,
        `2.5%`, `97.5%`, z, `Valor-p`
      ),
      ~ round(.x, 4)
    )
  ) %>%
  select(
    Cenario,
    Coeficiente,
    Estimativa,
    `Erro-padrão`,
    `2.5%`,
    `97.5%`,
    z,
    `Valor-p`
  ) %>% 
  mutate(
    modelo = "Poisson robusto"
  ) %>% 
  select(modelo, everything())



# análise individual dos ajustes ------------------------------------------

# Modelo log-binomial com todas as observações
modelo_logbin_completo <-
  modelos_logbin[["Sem ambas (81 e 107)"]]

# Resumo do modelo
summary(modelo_logbin_completo)

# Coeficientes e erros-padrão
coeficientes_logbin <-
  summary(modelo_logbin_completo)$coefficients

coeficientes_logbin


## 

tabela_final_logbin_frequentista
tabela_final_poisson_robusto

tabela_final <- 
  rbind(
    tabela_final_logbin_frequentista,
    tabela_final_poisson_robusto
    ) %>% 
  select(Cenario, modelo, Coeficiente, everything()) %>% 
  arrange(
    Cenario, Coeficiente, modelo
  )


# saidas ------------------------------------------------------------------

write.csv(
  tabela_final,
  "resultados_sensibilidade_logbin.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)



# saidas latex ------------------------------------------------------------

# tabela com informações dos outliers identificados
kbl(
  tabela_outliers, 
  format = "latex", 
  booktabs = TRUE, 
  align = "lcccccccc", 
  caption = "Modelo com Métricas"
  ) 


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

