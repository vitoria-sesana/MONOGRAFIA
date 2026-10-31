# ajuste do modelo log-binomial frequentista com a inclusão e retirada de outliers

# rotina ------------------------------------------------------------------
source("scripts/0-rotina.R", encoding = "UTF-8")

# outliers ----------------------------------------------------------------
outliers <- c(81, 107)

# definição das combinações -----------------------------------------------

dados_cenarios <- list(
  "Dados completos" = respiratory4,
  "Somente sem 81" = respiratory4 %>%
    filter(id != 81),
  "Somente sem 107" = respiratory4 %>%
    filter(id != 107),
  "Sem 81 e 107" = respiratory4 %>%
    filter(!id %in% c(81, 107))
)


# ajustes log-binomial frequentista ---------------------------------------

modelos_logbin <- 
  lapply(dados_cenarios, function(dados) {
    logbin::logbin(
      outcome ~ center + treat + baseline,
      data = dados,
      start = c(-1, -0.5,-0.5, -0.5)
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



# resultados --------------------------------------------------------------

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


tabela_final_logbin_frequentista

# saidas ------------------------------------------------------------------

saveRDS(
  tabela_final_logbin_frequentista, 
  "saidas/1-saida-aplicacao/outliers_log_binomial_frequentista_estimativas.rds"
)

