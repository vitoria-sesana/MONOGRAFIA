rm(list = ls())
options(scipen = 999)

# bibliotecas utilizadas --------------------------------------------------

## tratamento
require(dplyr)
require(tidyr)
require(purrr)

## ajustes
require(logbin)
require(geepack)

## inferencia bayesiana
require(rjags)
require(coda)
require(runjags)
require(bayesplot)

## modelo poisson

require(sandwich)

require(lmtest)

## tabelas
require(knitr)
require(kableExtra)

## graficos
require(ggplot2)
require(latex2exp)
require(ggtext)
require(patchwork)


# leitura dados -----------------------------------------------------------
data(respiratory, package="geepack")

# tratamento --------------------------------------------------------------
respiratory$center <- factor(respiratory$center, levels = c("2", "1"))
respiratory$baseline <- factor(respiratory$baseline, levels = c("1", "0"))

# seleção visita 4 --------------------------------------------------------
respiratory4 <- subset(respiratory, visit == 4)

# removendo base original do ambiente
rm(respiratory)
