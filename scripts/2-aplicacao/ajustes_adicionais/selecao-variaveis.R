# Quais variáveis considerar nos ajustes e estudos de simulações

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

modelos[[20]]

# modelo  -----------------------------------------------------------------
summary(modelos[[19]])
summary(modelos[[20]])



# tabela convergencia -----------------------------------------------------



tabela_convergencia <- data.frame(
  Modelo = integer(),
  Tem_NA = logical(),
  Status = character(),
  stringsAsFactors = FALSE
)

for (i in seq_along(modelos)) {
  
  sum_model <- tryCatch({
    summary(modelos[[i]])
  }, error = function(e) {
    NULL
  })
  
  if (is.null(sum_model) || is.null(sum_model$coefficients)) {
    status <- "Não convergiu"
    tem_na <- NA
  } else {
    coefs <- sum_model$coefficients
    
    if (any(is.na(coefs))) {
      tem_na <- TRUE
      status <- "Não convergiu" 
    } else {
      tem_na <- FALSE
      status <- "Convergiu"
    }
  }
  
  tabela_convergencia <- rbind(tabela_convergencia, data.frame(
    Modelo = i,
    Tem_NA = tem_na,
    Status = status
  ))
}

print(tabela_convergencia)



# tabela final ------------------------------------------------------------

tabela_final <- 
  cbind(
    tabela_boundary,
    tabela_convergencia
  ) %>% 
  select(combinacao, Status)



# saídas ------------------------------------------------------------------

tabela_final %>% 
  kbl(
    format = "latex",
    booktabs = TRUE, 
    align = "ll", 
    caption = "XXXXXXXXXXXXXXX"
  ) 
