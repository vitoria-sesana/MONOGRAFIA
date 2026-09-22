# Resultados dO exato de Fisher.    
# para verificar a associação entre desfecho e covariável

# rotina ------------------------------------------------------------------
source("scripts/0-rotina.R", encoding = "UTF-8")

# centro ------------------------------------------------------------------
tab1 <- table(respiratory4$outcome, respiratory4$center)
ft1 <- fisher.test(tab1, conf.int = intervalo_confianca); ft1

# estado inicial ----------------------------------------------------------
tab2 <- table(respiratory4$outcome, respiratory4$baseline)
ft2 <- fisher.test(tab2, conf.int = intervalo_confianca); ft2

# sexo --------------------------------------------------------------------
tab3 <- table(respiratory4$outcome, respiratory4$sex)
ft3 <- fisher.test(tab3, conf.int = intervalo_confianca); ft3

# tratamento --------------------------------------------------------------
tab4 <- table(respiratory4$outcome, respiratory4$treat)
ft4 <- fisher.test(tab4, conf.int = intervalo_confianca); ft4



# resultados --------------------------------------------------------------

tab1
tab2
tab3
tab4

ft1$p.value
ft2$p.value
ft3$p.value
ft4$p.value


# tabela ------------------------------------------------------------------

resultado_fisher <- data.frame(
  Comparacao = c("Centro", "Estado Inicial", "Sexo", "Tratamento"),
  Odds_Ratio = c(ft1$estimate,
                 ft2$estimate,
                 ft3$estimate,
                 ft4$estimate),
  IC_inf = c(ft1$conf.int[1],
             ft2$conf.int[1],
             ft3$conf.int[1],
             ft4$conf.int[1]),
  IC_sup = c(ft1$conf.int[2],
             ft2$conf.int[2],
             ft3$conf.int[2],
             ft4$conf.int[2]
             ),
  P_valor = c(ft1$p.value,
              ft2$p.value,
              ft3$p.value,
              ft4$p.value)
) %>% 
  mutate(
    Odds_Ratio = round(Odds_Ratio, 4),
    IC_inf     = round(IC_inf    , 4),
    IC_sup     = round(IC_sup     , 4),
    P_valor = round(P_valor, 4)
  )

resultado_fisher 


# idade e outcome ---------------------------------------------------------

teste_idade_outcome <- 
  wilcox.test(
    age ~ outcome, 
    data = respiratory4, 
    conf.int = intervalo_confianca,
    alternative = "two.sided"
    )

teste_idade_outcome$p.value < 0.05
teste_idade_outcome$p.value
teste_idade_outcome

teste_idade_outcome$conf.int
teste_idade_outcome$method
teste_idade_outcome$alternative

# saídas ------------------------------------------------------------------

kbl(resultado_fisher,
    format = "latex", 
    booktabs = TRUE, 
    align = "crrrr",
    caption = "XXXXXXXXXXXX") %>%
  kable_styling(latex_options = c("hold_position", "striped"))

