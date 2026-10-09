# Quais variáveis considerar?

# rotina ------------------------------------------------------------------
source("scripts/0-rotina.R", encoding = "UTF-8")


# ajuste logbin completo --------------------------------------------------
modelo_logbin_frequentista_completo <- 
  logbin::logbin(
    outcome ~ center  + treat + baseline + sex + age, 
    data=respiratory4
  ) 

modelo_logbin_frequentista_completo$boundary


modelo_logbin_frequentista_completo %>% 
  summary()

# ajuste logbin selecionado -----------------------------------------------
modelo_logbin_frequentista_selecionado <- 
  logbin::logbin(
    outcome ~ center  + treat + baseline, 
    data=respiratory4
  ) 

modelo_logbin_frequentista_selecionado$boundary

modelo_logbin_frequentista_selecionado %>% 
  summary()

# combinações covariáveis -------------------------------------------------

## covariáveis

variaveis <- c(
  "center",
  "treat",
  "baseline",
  "sex",
  "age"
)

# ---------------------------------------------------------
# 2. Gerar todas as combinações não vazias
# ---------------------------------------------------------

combinacoes <- unlist(
  lapply(seq_along(variaveis), function(k) {
    combn(variaveis, k, simplify = FALSE)
  }),
  recursive = FALSE
)

# ajuste dos modelos para cada combinação ---------------------------------
# utiliza-se o boundary para verificar a convergência ou não da matriz de convariâcia e variância

modelos <- list()

resultados_boundary <- lapply(
  seq_along(combinacoes),
  function(i) {
    
    vars <- combinacoes[[i]]
    
    formula_modelo <- reformulate(
      vars,
      response = "outcome"
    )
    
    modelo <- tryCatch(
      logbin::logbin(
        formula_modelo,
        data = respiratory4
      ),
      error = function(e) NULL
    )
    
    if (is.null(modelo)) {
      return(
        tibble(
          id = i,
          combinacao = paste(vars, collapse = " + "),
          n_variaveis = length(vars),
          boundary = NA,
          status = "Erro no ajuste"
        )
      )
    }
    
    # Guardar o modelo usando seu número como identificador
    modelos[[i]] <<- modelo
    
    boundary <- modelo$boundary
    
    tibble(
      id = i,
      combinacao = paste(vars, collapse = " + "),
      n_variaveis = length(vars),
      boundary = if (length(boundary) == 0) {
        NA
      } else {
        as.character(boundary)[1]
      },
      status = "Ajustado"
    )
  }
)

# resultado todas as combinações ------------------------------------------


tabela_boundary <- 
  bind_rows(resultados_boundary) %>%
  arrange(n_variaveis, combinacao) %>% 
  select(-id)

print(tabela_boundary, n = Inf)



# modelo  -----------------------------------------------------------------
summary(modelos[[16]]) 
summary(modelos[[17]]) 
summary(modelos[[18]])
summary(modelos[[19]])
summary(modelos[[20]]) ## sem convergência
summary(modelos[[21]])
summary(modelos[[22]]) ## sem convergência
summary(modelos[[23]])
summary(modelos[[24]]) 
summary(modelos[[25]]) ## sem convergência

