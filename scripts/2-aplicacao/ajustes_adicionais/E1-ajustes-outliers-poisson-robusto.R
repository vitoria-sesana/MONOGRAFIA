
# rotina ------------------------------------------------------------------
source("scripts/0-rotina.R", encoding = "UTF-8")


# ---------------------------------------------------------
# 1. Definir os quatro cenários
# ---------------------------------------------------------

dados_cenarios <- list(
  "Com ambas (81 e 107)" = respiratory4,
  "Somente sem 81" = respiratory4 %>%
    filter(id != 81),
  "Somente sem 107" = respiratory4 %>%
    filter(id != 107),
  "Sem ambas (81 e 107)" = respiratory4 %>%
    filter(!id %in% c(81, 107))
)

# ---------------------------------------------------------
# 2. Ajustar os modelos de Poisson
# ---------------------------------------------------------

modelos_poisson <- lapply(dados_cenarios, function(dados) {
  glm(
    outcome ~ center + treat + baseline,
    family = poisson(link = "log"),
    data = dados
  )
})

# ---------------------------------------------------------
# 3. Extrair resultados com variância robusta
# ---------------------------------------------------------

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

# ---------------------------------------------------------
# 4. Consolidar os quatro cenários
# ---------------------------------------------------------

tabela_final_poisson <- bind_rows(
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
  )

# Exibir a tabela completa
print(tabela_final_poisson, n = Inf)

# ---------------------------------------------------------
# 5. Exportar a tabela
# ---------------------------------------------------------

write.csv(
  tabela_final_poisson,
  "resultados_sensibilidade_poisson_robusto.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ---------------------------------------------------------
# 6. Acessar individualmente os resultados, se necessário
# ---------------------------------------------------------

# Modelo com todas as observações
modelo_poisson <- modelos_poisson[["Com ambas (81 e 107)"]]

# Matriz de covariância robusta do modelo completo
matriz_covariancia_variancia_sandwich <-
  sandwich::sandwich(modelo_poisson)

# Teste dos coeficientes com erros-padrão robustos
modelo_sandwich <- lmtest::coeftest(
  modelo_poisson,
  vcov. = matriz_covariancia_variancia_sandwich
)


modelo_sandwich
