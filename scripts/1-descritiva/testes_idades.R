# Resultados Wilcoxon e T
# para verificar a dieferença da distribuição da idade entre cada defecho
# idade do grupo a cada desfecho

# rotina ------------------------------------------------------------------
source("scripts/0-rotina.R", encoding = "UTF-8")


# informações quartis -----------------------------------------------------

boxplot_info <- function(x) {
  c(
    mediana = median(x, na.rm = TRUE),
    Q1 = quantile(x, 0.25, na.rm = TRUE),
    Q3 = quantile(x, 0.75, na.rm = TRUE)
  )
}

aggregate(
  age ~ outcome,
  data = respiratory4,
  FUN = boxplot_info
)

# wilcoxon ----------------------------------------------------------------
teste_wilcox <- 
  wilcox.test(
    age ~ outcome, 
    data = respiratory4, 
    conf.int = intervalo_confianca,
    alternative = "two.sided"
  )

teste_wilcox$p.value < 0.05
teste_wilcox$p.value
teste_wilcox

teste_wilcox$conf.int
teste_wilcox$method
teste_wilcox$alternative


# teste t -----------------------------------------------------------------

teste_t <- 
  t.test(
  age ~ outcome, 
    data = respiratory4, 
    conf.int = intervalo_confianca,
    alternative = "two.sided"
  )


teste_t$p.value < 0.05
teste_t$p.value
teste_t

teste_t$conf.int
teste_t$method
teste_t$alternative


# tabela final ------------------------------------------------------------

# Tabela final dos testes
tabela_testes <- data.frame(
  Teste = c(
    teste_wilcox$method,
    teste_t$method
  ),
  `Estatística do teste` = c(
    unname(teste_wilcox$statistic),
    unname(teste_t$statistic)
  ),
  `IC 2,5%` = c(
    teste_wilcox$conf.int[1],
    teste_t$conf.int[1]
  ),
  `IC 97,5%` = c(
    teste_wilcox$conf.int[2],
    teste_t$conf.int[2]
  ),
  `Valor-p` = c(
    teste_wilcox$p.value,
    teste_t$p.value
  ),
  check.names = FALSE
)

tabela_testes[-1] <- lapply(
  tabela_testes[-1],
  function(x) trunc(x * 10000) / 10000
)

tabela_testes

# saída -------------------------------------------------------------------

kbl(
  tabela_testes,
  format = "latex", 
  booktabs = TRUE, 
  align = "crrrr",
  caption = "XXXXXXXXXXXX"
  ) 
