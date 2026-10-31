# Chutes iniciais ---------------------------------------------------------

chutes_iniciais <- list(
  chute1  = list(b0 = -1,  b1 = -0.5,  b2 = -0.5,  b3 = -0.5), # escolhida para a análise de diagnósitco e comparação das estimativas
  chute2  = list(b0 = -0.1,  b1 = -0.1,  b2 = -0.1,  b3 = -0.1),
  chute3  = list(b0 = -1.2,  b1 = -0.6,  b2 = -0.3,  b3 = -0.7),
  chute4  = list(b0 = -0.2,  b1 = -0.5,  b2 = -0.8,  b3 = -0.4),
  chute5  = list(b0 = -0.9,  b1 = -1.4,  b2 = -0.2,  b3 = -0.6),
  chute6  = list(b0 = -1.1,  b1 = -0.2,  b2 = -0.5,  b3 = -1.3),
  chute7  = list(b0 = -0.7,  b1 = -1.6,  b2 = -0.4,  b3 = -0.9),
  chute8  = list(b0 = -1.8,  b1 = -0.9,  b2 = -0.2,  b3 = -1.5),
  chute9  = list(b0 = -2.5,  b1 = -0.7,  b2 = -1.3,  b3 = -0.4),
  chute10 = list(b0 = -5.0,  b1 = -3.7,  b2 = -7.2,  b3 = -9.5)
)

tabela_chutes <- do.call(
  rbind,
  lapply(names(chutes_iniciais), function(nome) {
    data.frame(
      Chute = nome,
      b0 = chutes_iniciais[[nome]]$b0,
      b1 = chutes_iniciais[[nome]]$b1,
      b2 = chutes_iniciais[[nome]]$b2,
      b3 = chutes_iniciais[[nome]]$b3
    )
  })
)


# Saídas ------------------------------------------------------------------

saveRDS(
  chutes_iniciais,
  file = "saidas/1-saida-aplicacao/chutes_iniciais_lista.rds"
)

write.csv(
  tabela_chutes,
  file = "saidas/1-saida-aplicacao/chutes_iniciais_tabela.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# latex
kbl(
  tabela_chutes,
  format = "latex",
  booktabs = TRUE,
  caption = "Tabela de resultados",
  label = "tab:resultados",
  align = "lcccccccc"
)

