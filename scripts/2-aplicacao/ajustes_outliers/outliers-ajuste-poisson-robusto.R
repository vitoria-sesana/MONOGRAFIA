# ajuste do modelo poisson robusto com a inclusão e retirada de outliers

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


# ajustes poisson robusto -------------------------------------------------

modelos_poisson <- 
  lapply(dados_cenarios, function(dados) {
    glm(
      outcome ~ center + treat + baseline,
      family = poisson(link = "log"),
      data = dados,
      start = c(-1, -0.5,-0.5, -0.5)
    )
  })


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



# saidas ------------------------------------------------------------------

saveRDS(
  tabela_final_poisson_robusto, 
  "saidas/1-saida-aplicacao/outliers_poisson_robusto_estimativas.rds"
)

