# Resultados dO exato de Fisher.    
# para verificar a associação entre desfecho e covariável

# rotina ------------------------------------------------------------------
source("scripts/0-rotina.R", encoding = "UTF-8")

# centro ------------------------------------------------------------------
tab1 <- table(respiratory4$outcome, respiratory4$center)
ft1 <- fisher.test(tab1); ft1

# tratamento --------------------------------------------------------------
tab2 <- table(respiratory4$outcome, respiratory4$treat)
ft2 <- fisher.test(tab2); ft2

# estado inicial ----------------------------------------------------------
tab3 <- table(respiratory4$outcome, respiratory4$baseline)
ft3 <- fisher.test(tab3); ft3


# resultados --------------------------------------------------------------

tab1
tab2
tab3

ft1$p.value
ft2$p.value
ft3$p.value


# tabela ------------------------------------------------------------------

resultado_fisher <- data.frame(
  Comparacao = c("Variável 1", "Variável 2", "Variável 3"),
  Odds_Ratio = c(ft1$estimate,
                 ft2$estimate,
                 ft3$estimate),
  IC_inf = c(ft1$conf.int[1],
             ft2$conf.int[1],
             ft3$conf.int[1]),
  IC_sup = c(ft1$conf.int[2],
             ft2$conf.int[2],
             ft3$conf.int[2]),
  P_valor = c(ft1$p.value,
              ft2$p.value,
              ft3$p.value)
) %>% 
  mutate(
    Odds_Ratio = round(Odds_Ratio, 4),
    IC_inf     = round(IC_inf    , 4),
    IC_sup     = round(IC_sup     , 4),
    P_valor = round(P_valor, 4)
  )

resultado_fisher 

# saídas ------------------------------------------------------------------

kbl(resultado_fisher,
    format = "latex", 
    booktabs = TRUE, 
    align = "crrrr",
    caption = "XXXXXXXXXXXX") %>%
  kable_styling(latex_options = c("hold_position", "striped"))

