# diagnostico mcmc dos chutes iniciais

# rotina ------------------------------------------------------------------
source("scripts/0-rotina.R", encoding = "UTF-8")


# leitura -----------------------------------------------------------------
amostras <- 
  readRDS("saidas/1-saida-aplicacao/chutes_iniciais_bayesiano_posterior.rds")


# resumo ------------------------------------------------------------------
library(coda)
library(dplyr)

tabela_resumo <- lapply(seq_along(amostras), function(n) {
  
  amostra_n <- amostras[[n]]
  
  todas <- as.matrix(amostra_n)
  
  hpd <- HPDinterval(as.mcmc(todas), prob = 0.95)
  
  res <- summary(amostra_n)
  effes <- effectiveSize(amostra_n) 
  
  data.frame(
    chute        = paste0("Chute ", n),
    parametro    = rownames(res$statistics),
    # tabela 1
    media        = res$statistics[, "Mean"],
    RP           = exp(res$statistics[, "Mean"]),
    dp           = res$statistics[, "SD"],
    erro_MC      = res$statistics[, "Naive SE"],
    erro_ts      = res$statistics[, "Time-series SE"],
    ESS          = effes[rownames(res$statistics)],
    
    # tabela 2 
    q2.5         = res$quantiles[, "2.5%"],
    q25          = res$quantiles[, "25%"],
    mediana      = res$quantiles[, "50%"],
    q75          = res$quantiles[, "75%"],
    q97.5        = res$quantiles[, "97.5%"],
    amp_quant    = res$quantiles[, "97.5%"] - res$quantiles[, "2.5%"],
    HPD_inf      = hpd[, "lower"],
    HPD_sup      = hpd[, "upper"],
    amp_HPD      = hpd[, "upper"] - hpd[, "lower"],
    row.names = NULL
  )
  
}) %>%
  
  # Empilhar os resultados
  bind_rows() %>%
  
  # Arredondar somente colunas numéricas
  mutate(
    across(where(is.numeric), ~ round(.x, 4))
  )

tabela_resumo

tabela_resumo_1 <- tabela_resumo %>%
  select(
    chute, parametro,
    media, RP, dp, erro_MC, erro_ts, ESS
  )

tabela_resumo_2 <- tabela_resumo %>%
  select(
    chute, parametro,
    q2.5, q25, mediana, q75, q97.5,
    amp_quant, HPD_inf, HPD_sup, amp_HPD
  )


# gelman plot -------------------------------------------------------------

library(coda)
library(ggplot2)
library(dplyr)
library(purrr)
library(latex2exp)
library(patchwork)

parametros <- c("b0", "b1", "b2", "b3")

extrai_gelman <- function(p) {
  g <- gelman.plot(amostras[, p, drop = FALSE], autoburnin = FALSE)
  tibble(
    parametro = p,
    iteracao  = g$last.iter,
    mediana   = g$shrink[, 1, "median"],
    limite    = g$shrink[, 1, "97.5%"]
  )
}

dados_g <- map_dfr(parametros, extrai_gelman) |>
  tidyr::pivot_longer(c(mediana, limite),
                      names_to = "tipo", values_to = "valor") |>
  mutate(tipo = recode(tipo,
                       mediana = "Mediana",
                       limite  = "97,5%"))

grafico_gelman <- function(p, titulo) {
  dados_g |>
    filter(parametro == p) |>
    ggplot(aes(x = iteracao, y = valor, color = tipo, linetype = tipo)) +
    geom_hline(yintercept = 1, color = "grey70") +
    geom_line(linewidth = 0.65) +
    labs(title = titulo, x = "Última iteração na cadeia",
         y = bquote(bold("Fator de redução" ~ hat(R))), color = "", linetype = "") +
    theme_minimal() +
    theme(
      panel.border = element_rect(color = "grey70", fill = NA, linewidth = 0.5),
      axis.text.x = element_text(color = "grey20", size = 11),
      axis.text.y = element_text(color = "grey20", size = 11),
      axis.title.x = element_text(color = "grey20", face = "bold", size = 13),
      axis.title.y = element_text(color = "grey20", face = "bold", size = 13),
      legend.text = element_text(color = "grey20", size = 11)
      # panel.grid.minor.x = element_blank(),
      # panel.grid.major.x = element_blank()
    ) +
    scale_color_manual(values = c("Mediana" = "#009E73", "97,5%" = "#D55E00")) +
    scale_linetype_manual(values = c("Mediana" = "solid", "97,5%" = "dashed")) +
    scale_y_continuous(
      labels = function(x) sprintf("%.2f", trunc(x * 100) / 100),
      limits = c(0.995, 1.04),
      breaks = seq(0.995, 1.04, by = 0.01)
    ) +
    scale_x_continuous(
      breaks = seq(0, max(dados_g$iteracao), by = 5000),
      labels = scales::label_number(big.mark = "")
    ) 
}

g0 <- grafico_gelman("b0", TeX("$\\beta_{0}$: Intercepto"))
g1 <- grafico_gelman("b1", TeX("$\\beta_{1}$: Centro"))
g2 <- grafico_gelman("b2", TeX("$\\beta_{2}$: Tratamento"))
g3 <- grafico_gelman("b3", TeX("$\\beta_{3}$: Estado inicial"))

gg_gelman_plot <- (g0 + g1) / (g2 + g3) + plot_layout(guides = "collect") &
  theme(legend.position = "bottom")



# saídas ------------------------------------------------------------------

## resumo

kbl(
  tabela_resumo_1,
  format = "latex",
  booktabs = TRUE,
  caption = "Tabela de resultados",
  label = "tab:resultados",
  align = "lcccccccc",
  linesep = ""
)

kbl(
  tabela_resumo_2,
  format = "latex",
  booktabs = TRUE,
  caption = "Tabela de resultados",
  label = "tab:resultados",
  align = "lcccccccc",
  linesep = ""
)

## gelman plot


ggsave(
  "plots/2-plots-aplicacao/chutes_iniciais_gelman_plot.pdf",
  gg_gelman_plot,
  width = 16,
  height = 10
)

