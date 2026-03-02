# Teste binomial para especificação dos parâmetros para gerar
# valores pseudo aleatórios das covariáveis centro, tratamento e 
# estado inicial respiratório
# (Teste de aderência)

# rotina ------------------------------------------------------------------
source("scripts/0-rotina.R", encoding = "UTF-8")

# center(1 OU 2): 2 é referência, 2 = 0 e 1 = 1
# treat(A ou P): A é referência, A = 0  e P = 1
# baseline(0 ou 1): 1 é referência, 1 = 0 e 0 = 1

# ajuste ------------------------------------------------------------------
logbin4 <- 
  logbin::logbin(
    outcome ~ center  + treat + baseline, 
    data=respiratory4
  ) 

summary(logbin4)

matrix_modelo <- model.matrix(logbin4) %>% as.data.frame() %>% select(-`(Intercept)`)
matrix_respi <- respiratory4 %>% select(center, treat, baseline) 

# base --------------------------------------------------------------------
respiratory4 %>% 
  select(center, treat, baseline) %>% 
  lapply(table)

matrix_modelo %>% 
  lapply(table)

# center <- rbinom(n_obs, 1, 0.5)
# treat <- rbinom(n_obs, 1, 0.51)
# baseline <- rbinom(n_obs, 1, 0.53)

# centro ------------------------------------------------------------------
probabilidade_sugerida_center = 0.5
var_center <- matrix_modelo$center1

resultado_center <- 
  binom.test(
    sum(var_center), 
    length(var_center),
    p = probabilidade_sugerida_center
  )

print(resultado_center)
resultado_center$statistic
resultado_center$p.value

# tratamento --------------------------------------------------------------
probabilidade_sugerida_treat = 0.51
var_treat <- matrix_modelo$treatP

resultado_treat <- 
  binom.test(
    sum(var_treat), 
    length(var_treat),
    p = probabilidade_sugerida_treat
  )

print(resultado_treat)
resultado_treat$statistic
resultado_treat$p.value

# baseline ----------------------------------------------------------------
probabilidade_sugerida_baseline = 0.53 #0.55
var_baseline <- matrix_modelo$baseline0

resultado_baseline <- 
  binom.test(
    sum(var_baseline), 
    length(var_baseline),
    p = probabilidade_sugerida_baseline
  )

print(resultado_baseline)
resultado_baseline$statistic
resultado_baseline$p.value


# resultados --------------------------------------------------------------
resultado_center$p.value
resultado_treat$p.value
resultado_baseline$p.value


# tabela ------------------------------------------------------------------

pvalor <- c(resultado_center$p.value,
            resultado_treat$p.value,
            resultado_baseline$p.value) %>% 
  round(4)

p <- c(
  resultado_center$null.value,
  resultado_treat$null.value,
  resultado_baseline$null.value
)

liminf <- 
  c(
    resultado_center$conf.int[1],
    resultado_treat$conf.int[1],
    resultado_baseline$conf.int[1]
  ) %>% round(4)



testes <- list(
  A = resultado_center,
  B = resultado_treat,
  C = resultado_baseline
)

limsup <- 
  c(
    resultado_center$conf.int[2],
    resultado_treat$conf.int[2],
    resultado_baseline$conf.int[2]
  ) %>% round(4)


tabela_binomial <- do.call(rbind, lapply(names(testes), function(nome) {
  t <- testes[[nome]]
  
  data.frame(
    Teste = nome,
    Sucessos = t$statistic,
    Tentativas = t$parameter,
    Proporcao_observada = round(t$estimate, 3),
    Proporcao_H0 = t$null.value,
    p_valor = round(t$p.value, 4),
    IC_inf = round(t$conf.int[1], 3),
    IC_sup = round(t$conf.int[2], 3)
  )
})) %>%  
  tibble::rownames_to_column() %>% 
  select(-rowname)

tabela_binomial

# saídas ------------------------------------------------------------------
kbl(
  tabela_binomial,
  format = "latex",
  booktabs = TRUE,
  align = "crr",
  caption = "Tabela resultados testes binomiais") %>%
  kable_styling(latex_options = c("hold_position", "striped"))
