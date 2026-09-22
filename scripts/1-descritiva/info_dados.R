rm(list = ls())
options(scipen = 999)

# leitura dados -----------------------------------------------------------
data(respiratory, package="geepack")

# tratamento --------------------------------------------------------------
respiratory$center <- factor(respiratory$center, levels = c("2", "1"))
respiratory$baseline <- factor(respiratory$baseline, levels = c("1", "0"))

# seleção visita 4 --------------------------------------------------------
respiratory4 <- subset(respiratory, visit == 4)

# removendo base original do ambiente
rm(respiratory)


# informações -------------------------------------------------------------

?respiratory


# colunas -----------------------------------------------------------------
respiratory4$center %>% table()
respiratory4$treat %>% table()
respiratory4$baseline %>% table()
