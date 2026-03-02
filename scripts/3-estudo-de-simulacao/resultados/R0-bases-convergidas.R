rm(list = ls())

# modelo logbin -----------------------------------------------------------
modelos_logbin <- 
  readRDS("saidas/2-saida-simulacao/M1-logbin-frequentista/ajustes_logbin_frequentista.rds")

# calculando matriz de variancia e covariancia ----------------------------
covs <- lapply(modelos_logbin, vcov)

# verificando NA'S na matrix vcov -----------------------------------------
resultado_final <- 
  tibble(
    modelo = names(covs),
    convergencia = purrr::map_chr(covs, ~ {
      tem_na <- any(is.na(.x))
      if (tem_na) {
        "Não Convergiu"
      } else {
        "Convergiu"
      }
    })
  )

# tabela com as bases que convergiram e não convergiram -------------------
resultado_final

# analise das quantidades de convergência e não convergência --------------
resultado_final$convergencia %>% table()

# saida -------------------------------------------------------------------
write.csv(
  resultado_final, 
  "saidas/2-saida-simulacao/bases-convergidas.csv"
  )
