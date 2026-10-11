# rotina ------------------------------------------------------------------
source("scripts/0-rotina.R", encoding = "UTF-8")

# verificando os chutes iniciais ------------------------------------------
# 
# modelo_logbin_frequentista1 <- 
#   logbin::logbin(
#     outcome ~ center  + treat + baseline, 
#     data=respiratory4, start = list(b0=-1, b1=-0.5, b2=-0.5, b3=-0.5)
#   ) 
# 
# summary(modelo_logbin_frequentista1)$coefficients
# 
# 
# modelo_logbin_frequentista2 <- 
#   logbin::logbin(
#     outcome ~ center  + treat + baseline, 
#     data=respiratory4, start = list(b0=-0.8, b1=-0.3, b2=-0.4, b3=-0.2)
#   ) 
# 
# summary(modelo_logbin_frequentista2)$coefficients
# 
# 
# modelo_logbin_frequentista3 <- 
#   logbin::logbin(
#     outcome ~ center  + treat + baseline, 
#     data=respiratory4, start = list(b0=-1.2, b1=-0.6, b2=-0.3, b3=-0.7)
#   ) 
# 
# summary(modelo_logbin_frequentista3)$coefficients
# 
# 
# 
# modelo_logbin_frequentista4 <- 
#   logbin::logbin(
#     outcome ~ center  + treat + baseline, 
#     data=respiratory4, start = list(b0=-0.1, b1=-0.1, b2=-0.1, b3=-0.1)
#   ) 
# 
# summary(modelo_logbin_frequentista4)$coefficients