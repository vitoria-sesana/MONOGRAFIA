require(logbin)
require(geepack)
require(dplyr)

# leitura dados -----------------------------------------------------------
data(respiratory, package="geepack")

# tratamento --------------------------------------------------------------
respiratory$center <- factor(respiratory$center, levels = c("2", "1"))
respiratory$baseline <- factor(respiratory$baseline, levels = c("1", "0"))

# seleção visita 4 --------------------------------------------------------
respiratory4 <- subset(respiratory, visit == 4)

# ajuste logbin -----------------------------------------------------------
mod_lb <- 
  logbin::logbin(
    outcome ~ center  + treat + baseline, 
    data=respiratory4
  ) 

sum_logbin <- summary(mod_lb)

# analises ----------------------------------------------------------------

# coeficientes
sum_logbin
coefs_lb <- sum_logbin$coefficients; coefs_lb

# matriz de variancia e convariancia
vcov_lb <- sum_logbin %>% vcov(); vcov_lb

# intervalo de confiança
ic_lb <- mod_lb %>% confint(); ic_lb


# tabelas -----------------------------------------------------------------

est_lb <- round(cbind(coefs_lb, ic_lb), 4); est_lb 
rn <- rownames(est_lb)
est_lb <- est_lb %>% 
  janitor::clean_names() %>% 
  as_tibble() %>% 
  mutate(rn = rn) %>% 
  mutate(model = "Log-binomial") %>% 
  select(model, rn, estimate , std_error, x2_5_percent, x97_5_percent, z_value, pr_z)


library(knitr)
library(kableExtra)
kbl(est_lb, format = "latex", booktabs = TRUE, align = "lcccccccc", 
    caption = "XXXXXXXXXXXXX") %>%
  kable_styling(latex_options = c("hold_position", "striped"))


# analise de diagnostico --------------------------------------------------
par(mfrow = c(2, 2))
plot(mod_lb)

res_dev <- residuals(mod_lb, type = "deviance")
fitted_lb <- fitted(mod_lb)

plot(fitted_lb, res_dev,
     xlab = "Valores ajustados",
     ylab = "Resíduos deviance")
abline(h = 0, col = "red")



# gráficos de convergência usado------------------------------------------------

p_hat <- fitted(mod_lb)

library(ggplot2)

df <- data.frame(
  p = log(p_hat), 
  exp_p = p_hat,
  iter = seq_along(log(p_hat))
)

gg1 <- ggplot(df, aes(x = p, y = iter)) +
  geom_point() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(
    x = "Log(probabilidade estimada)",
    y = "Iteração"
  ) +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "grey70", fill = NA, size = 0.5),
    axis.text.x   = element_text(color = "grey20",size = 7),
    axis.text.y   = element_text(color = "grey20",size = 7),
    axis.title.x  = element_text(color = "grey20",face = "bold"),
    axis.title.y  = element_text(color = "grey20",face = "bold")
  )

gg2 <- ggplot(df, aes(x = exp_p, y = iter)) +
  geom_point() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  labs(
    x = "Probabilidade estimada",
    y = "Iteração"
  ) +
  theme_minimal() + 
  theme(
    panel.border = element_rect(color = "grey70", fill = NA, size = 0.5),
    axis.text.x   = element_text(color = "grey20",size = 7),
    axis.text.y   = element_text(color = "grey20",size = 7),
    axis.title.x  = element_text(color = "grey20",face = "bold"),
    axis.title.y  = element_text(color = "grey20",face = "bold")
  )

gg1
gg2

ggsave(
  "saidas/1-saida-ajustes/gg1.pdf",
  plot = gg1,
  device = cairo_pdf,
  width = 14,
  height = 12,
  units = "cm"
)


ggsave(
  "saidas/1-saida-ajustes/gg2.pdf",
  plot = gg2,
  device = cairo_pdf,
  width = 14,
  height = 12,
  units = "cm"
)

# envelope ----------------------------------------------------------------

class(mod_lb) <- c("glm", class(mod_lb))

# Envelope para modelo log-binomialmod_lb
set.seed(34)
plot_envelope<- glmtoolbox::envelope(
  mod_lb,
  type = "deviance",   
  col = "red",
  pch = 20,
  nsim = 10000,         
  plot.it = TRUE,
  ylim = c(-2.5, 3.5),
  main = "",
  xlab = "Quantis teóricos",
  ylab = "Resíduos de desvio"
)

df_plot_envelope <-
  plot_envelope %>% 
  as.data.frame()

df_plot_envelope

set.seed(34)
env <- glmtoolbox::envelope(
  mod_lb,
  type = "deviance",   
  nsim = 10000,
  plot.it = FALSE      # não plotar agora
)

df <- data.frame(
  x = 1:nrow(env),            # índice ou quantis teóricos
  y = env[, "Residuals"],     # resíduos observados
  lo = env[, "Lower limit"],  # limite inferior
  med = env[, "Median"],      # mediana do envelope
  hi = env[, "Upper limit"]   # limite superior
)

# Gráfico ggplot
gg_envel_logbin<- 
  ggplot(df, aes(x = x, y = y)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), fill = "grey90", alpha = 0.5) +  # envelope
  geom_line(aes(y = med), linetype = "dashed", color = "grey30", size = 0.1) +                    # linha central (mediana)
  geom_line(aes(y = lo), color = "red", size=0.5) +         # limite inferior
  geom_line(aes(y = hi), color = "red", size=0.5) +         # limite superior
  geom_point(color = "black", size = 1) +                                # resíduos
  labs(
    x = "Quantis teóricos N(0,1)",
    y = "Resíduos de desvio",
    title = ""
  ) +
  ylim(-2.5, 3.5) +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "grey70", fill = NA, size = 0.5),
    axis.text.x   = element_text(color = "grey20",size = 7),
    axis.text.y   = element_text(color = "grey20",size = 7),
    axis.title.x  = element_text(color = "grey20",face = "bold"),
    axis.title.y  = element_text(color = "grey20",face = "bold")
  )

gg_envel_logbin

ggsave(
  "saidas/1-saida-ajustes/envel_lb.pdf",
  plot = gg_envel_logbin,
  device = cairo_pdf,
  width = 14,
  height = 12,
  units = "cm"
)


require(geepack)
require(ggplot2)

# leitura dados -----------------------------------------------------------
data(respiratory, package="geepack")

# tratamento --------------------------------------------------------------
respiratory$center <- factor(respiratory$center, levels = c("2", "1"))
respiratory$baseline <- factor(respiratory$baseline, levels = c("1", "0"))

# seleção visita 4 --------------------------------------------------------
respiratory4 <- subset(respiratory, visit == 4)

# método 3: sandwich aplicado no teste ------------------------------------
modelo_poisson <- 
  glm(
    outcome ~ center  + treat + baseline,
    family = poisson(link=log), 
    data = respiratory4
  )

modelo_sandwich <- 
  lmtest::coeftest(modelo_poisson, vcov = sandwich::sandwich)

matriz_covariancia_variancia_sandwich <- 
  sandwich::sandwich(modelo_poisson)


# analises ----------------------------------------------------------------
modelo_poisson
modelo_sandwich

# coeficientes
summary(modelo_poisson)
modelo_sandwich

info_pois <- summary(modelo_poisson)$coefficients %>% 
  as.data.frame() %>% 
  round(4) %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column()

info_pois_sand <- modelo_sandwich[1:4,] %>% 
  as.data.frame() %>% 
  round(4) %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column()

modelo_poisson %>% vcov
matriz_covariancia_variancia_sandwich # robusta

# intervalos de confiança
ic_pois <- modelo_poisson %>% 
  confint() %>% 
  round(4) %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column()

ic_pois_sand <- modelo_sandwich %>% confint() %>% round(4) %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column()


# tabela de coeficientes

info_pois
info_pois_sand
ic_pois
ic_pois_sand

info_ic <- left_join(
  info_pois,
  ic_pois,
  by = "rowname"
) %>% 
  mutate(model = "Poisson")

info_ic_sand <- left_join(
  info_pois_sand,
  ic_pois_sand,
  by = "rowname"
) %>% 
  mutate(model = "Poisson robusto")

X <- rbind(
  info_ic, 
  info_ic_sand
) %>% 
  janitor::clean_names() 

DF_COEFS <- X %>% 
  select(model, rowname, estimate, std_error, x2_5_percent, x97_5_percent, z_value, pr_z)


library(knitr)
library(kableExtra)
kbl(DF_COEFS, format = "latex", booktabs = TRUE, align = "crr",
    caption = "Tabela status de convergência por tamanho amostral.") %>%
  kable_styling(latex_options = c("hold_position", "striped"))



# # tabela da matriz de covariância e variância
# 
# matriz_covariancia_variancia_sandwich_poisson <- modelo_poisson %>% vcov
# matriz_covariancia_variancia_sandwich # robusta
# 
# 
# kbl(round(matriz_covariancia_variancia_sandwich_poisson,4), format = "latex", booktabs = TRUE, align = "cccc", 
#     caption = "Matriz covariancia poisson")
# 
# 
# kbl(round(matriz_covariancia_variancia_sandwich, 4), format = "latex", booktabs = TRUE, align = "cccc", 
#     caption = "Matriz covariancia poisson robusto")

# tabelas -----------------------------------------------------------------

# library(knitr)
# library(kableExtra)
# kbl(est_lb, format = "latex", booktabs = TRUE, align = "lcccccccc", 
#     caption = "XXXXXXXXXXXXX") %>%
#   kable_styling(latex_options = c("hold_position", "striped"))





# envolope poisson --------------------------------------------------------

faixa <- c(-2.5, 3.5)
###

fit_model <- modelo_poisson

X =   model.matrix(fit_model)
n =   nrow(X)
p =   ncol(X)
w =   fit_model$weights
W =   diag(w)
H =   solve(t(X)%*%W%*%X)
H =   sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
h =   diag(H)
td =   residuals(fit_model)/sqrt((1-h))
e =   matrix(0,n,100)
#
for(i in 1:100){
  nresp =   rpois(n, fitted(fit_model))
  fit =   glm(nresp ~ X, family=poisson)
  w =   fit$weights
  W =   diag(w)
  H =   solve(t(X)%*%W%*%X)
  H =   sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
  h =   diag(H)
  e[,i] =   sort(residuals(fit)/sqrt(1-h))}
#
e1 =   numeric(n)
e2 =   numeric(n)
#
for(i in 1:n){
  eo =   sort(e[i,])
  e1[i] =   (eo[2]+eo[3])/2
  e2[i] =   (eo[97]+eo[98])/2}
#
med =   apply(e,1,mean)
# faixa =   range(td,e1,e2)
par(pty="s")
qqnorm(td,xlab="Quantil da N(0,1)",
       ylab="Componente do Desvio", ylim=faixa, pch=16, main="",cex=0.8, cex.lab=1, cex.axis=1.5)
par(new=TRUE)
#
qqnorm(e1,axes=FALSE,xlab="",ylab="",type="l",ylim=faixa,lty=1, main="",lwd=1, col = "red")
par(new=TRUE)
qqnorm(e2,axes=FALSE,xlab="",ylab="", type="l",ylim=faixa,lty=1, main="",lwd=1, col = "red")
par(new=TRUE)
qqnorm(med,axes=FALSE,xlab="", ylab="", type="l",ylim=faixa,lty=2, main="",lwd=1, col = "darkgray")

n <- length(td)

# Quantis teóricos da N(0,1) para os resíduos ordenados
q_theo <- qnorm(ppoints(n))  

# Ordenar resíduos e envelopes
td_ord <- sort(td)
e1_ord <- sort(e1)
e2_ord <- sort(e2)
med_ord <- sort(med)

# Criar data.frame
df <- data.frame(
  x = q_theo,
  y = td_ord,
  lo = e1_ord,
  hi = e2_ord,
  med = med_ord
)

# Gráfico de envelope correto
gg_envel_poisson <-
  ggplot(df, aes(x = x, y = y)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), fill = "grey90", alpha = 0.5) +  # envelope
  geom_line(aes(y = med), linetype = "dashed", color = "grey30", size = 0.1) + # linha central
  geom_line(aes(y = lo), color = "red", size = 0.5) +                        # limite inferior
  geom_line(aes(y = hi), color = "red", size = 0.5) +                        # limite superior
  geom_point(color = "black", size = 1) +                                    # resíduos observados
  labs(
    x = "Quantis teóricos N(0,1)",
    y = "Resíduos de desvio",
    title = ""
  ) +
  theme_minimal() +
  coord_cartesian(ylim = c(-2.5, 3.5)) +
  theme(
    panel.border = element_rect(color = "grey70", fill = NA, size = 0.5),
    axis.text.x   = element_text(color = "grey20",size = 7),
    axis.text.y   = element_text(color = "grey20",size = 7),
    axis.title.x  = element_text(color = "grey20",face = "bold"),
    axis.title.y  = element_text(color = "grey20",face = "bold")
  )



# envelope poisson robusto --------------------------------------------------------

# Matriz de design
X <- model.matrix(fit_model)
n <- nrow(X)
p <- ncol(X)

# Matriz de covariância sandwich
V <- sandwich::sandwich(fit_model)

H_robusta <- X %*% V %*% t(X)
h_rob <- diag(H_robusta)

td <- residuals(fit_model) / sqrt(1 - h_rob)

# Preparar matriz de simulações
e <- matrix(0, n, 100)

for(i in 1:100){
  nresp <- rpois(n, fitted(fit_model))
  fit <- glm(nresp ~ X - 1, family = poisson) # "-1" porque X já tem intercepto
  V_i <- sandwich::sandwich(fit)
  H_i <- X %*% V_i %*% t(X)
  h_i <- diag(H_i)
  
  e[,i] <- sort(residuals(fit) / sqrt(1 - h_i))
}

# Calcular envelopes
e1 <- numeric(n)
e2 <- numeric(n)
for(i in 1:n){
  eo <- sort(e[i,])
  e1[i] <- (eo[2] + eo[3])/2
  e2[i] <- (eo[97] + eo[98])/2
}

# Média dos resíduos simulados
med <- apply(e, 1, mean)

# Faixa do gráfico
# faixa <- range(td, e1, e2)

# Gráfico Q-Q
par(pty="s")
qqnorm(td, xlab="Quantil da N(0,1)",
       ylab="Componente do Desvio",
       ylim=faixa, pch=16, main="", cex=0.8, cex.lab=1, cex.axis=1)
par(new=TRUE)
qqnorm(e1, axes=FALSE, xlab="", ylab="", type="l", ylim=faixa, lty=1, main="", lwd=1, col="red")
par(new=TRUE)
qqnorm(e2, axes=FALSE, xlab="", ylab="", type="l", ylim=faixa, lty=1, main="", lwd=1, col="red")
par(new=TRUE)
qqnorm(med, axes=FALSE, xlab="", ylab="", type="l", ylim=faixa, lty=2, main="", lwd=1, col="darkgray")


library(ggplot2)

n <- length(td)

# Quantis teóricos da N(0,1) para os resíduos ordenados
q_theo <- qnorm(ppoints(n))  

# Ordenar resíduos e envelopes
td_ord <- sort(td)
e1_ord <- sort(e1)
e2_ord <- sort(e2)
med_ord <- sort(med)

# Criar data.frame
df <- data.frame(
  x = q_theo,
  y = td_ord,
  lo = e1_ord,
  hi = e2_ord,
  med = med_ord
)

# Gráfico de envelope correto
gg_envel_sandwich <-
  ggplot(df, aes(x = x, y = y)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), fill = "grey90", alpha = 0.5) +  # envelope
  geom_line(aes(y = med), linetype = "dashed", color = "grey30", size = 0.1) + # linha central
  geom_line(aes(y = lo), color = "red", size = 0.5) +                        # limite inferior
  geom_line(aes(y = hi), color = "red", size = 0.5) +                        # limite superior
  geom_point(color = "black", size = 1) +                                    # resíduos observados
  labs(
    x = "Quantis teóricos N(0,1)",
    y = "Resíduos de desvio",
    title = ""
  ) +
  theme_minimal() +
  coord_cartesian(ylim = c(-2.5, 3.5)) + 
  theme(
    panel.border = element_rect(color = "grey70", fill = NA, size = 0.5),
    axis.text.x   = element_text(color = "grey20",size = 7),
    axis.text.y   = element_text(color = "grey20",size = 7),
    axis.title.x  = element_text(color = "grey20",face = "bold"),
    axis.title.y  = element_text(color = "grey20",face = "bold")
  )


# saída -------------------------------------------------------------------

gg_envel_logbin
gg_envel_poisson
gg_envel_sandwich


ggsave(
  "saidas/1-saida-ajustes/envel_pois.pdf",
  plot = gg_envel_poisson,
  device = cairo_pdf,
  width = 14,
  height = 12,
  units = "cm"
)



ggsave(
  "saidas/1-saida-ajustes/envel_pois_robust.pdf",
  plot = gg_envel_sandwich,
  device = cairo_pdf,
  width = 14,
  height = 12,
  units = "cm"
)
