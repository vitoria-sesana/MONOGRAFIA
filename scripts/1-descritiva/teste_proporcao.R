# Teste de Proporção Iguais
# Pearson's chi-squared test statistic.

# rotina ------------------------------------------------------------------
source("scripts/0-rotina.R", encoding = "UTF-8")

# tamanho -----------------------------------------------------------
n <- nrow(respiratory4); n

# Estado inicial ----------------------------------------------------------
p1 <- 
  respiratory4 %>% 
  select(baseline) %>% 
  table() %>% 
  as.vector()

pt1 <- 
  stats::prop.test(p1[1], n, conf.level = intervalo_confianca); pt1

# Centro ------------------------------------------------------------------
p2 <- 
  respiratory4 %>% 
  select(center) %>% 
  table() %>% 
  as.vector()

pt2 <- 
  stats::prop.test(p2[1], n, conf.level = intervalo_confianca); pt2

# Desfecho ----------------------------------------------------------------
p3 <- 
  respiratory4 %>% 
  select(outcome) %>% 
  table() %>% 
  as.vector()

pt3 <- 
  stats::prop.test(p3[1], n, conf.level = intervalo_confianca); pt3

# Sexo --------------------------------------------------------------------
p4 <- 
  respiratory4 %>% 
  select(sex) %>% 
  table() %>% 
  as.vector()

pt4 <- 
  stats::prop.test(p4[1], n, conf.level = intervalo_confianca); pt4

# Tratamento --------------------------------------------------------------
p5 <- 
  respiratory4 %>% 
  select(treat) %>% 
  table() %>% 
  as.vector()

pt5 <- 
  stats::prop.test(p5[1], n, conf.level = intervalo_confianca); pt5


# resultado final ---------------------------------------------------------
pt1$p.value
pt2$p.value
pt3$p.value
pt4$p.value
pt5$p.value

