# rotina ------------------------------------------------------------------
source("scripts/0-rotina.R", encoding = "UTF-8")

# leitura e tratamento ----------------------------------------------------

## modelos ajustados --------------------
mod_logbin_freq_all <-  readRDS("saidas/2-saida-simulacao/M1-logbin-frequentista/ajustes_logbin_frequentista.rds")
mod_pois_sandwich_all <- readRDS("saidas/2-saida-simulacao/M2-poisson-robusto/ajustes_poisson_robusto.rds")
mod_logbin_bayes_all <- readRDS("saidas/2-saida-simulacao/M3-logbin-bayesiano/ajustes_logbin_bayesiano.rds")

## listas das bases convergidas --------------------
lista_bases_convergidas <- 
  read.csv("saidas/2-saida-simulacao/bases-convergidas.csv")

nao_convergiram <- 
  lista_bases_convergidas %>% 
  filter(convergencia == "Não Convergiu") %>% 
  select(modelo) %>% 
  as.vector() %>% 
  unlist()

convergiram <- 
  lista_bases_convergidas %>% 
  filter(convergencia == "Convergiu") %>% 
  select(modelo) %>% 
  as.vector() %>% 
  unlist()

## modelos ajustados das bases que não convergiram --------------------
mod_logbin_freq <- mod_logbin_freq_all[convergiram]
mod_pois_sandwich <- mod_pois_sandwich_all[convergiram]
mod_logbin_bayes <- mod_logbin_bayes_all[convergiram]

## valores verdadeiros --------------------
b0 =  -0.1089
b_center = -0.3414
b_treat = -0.2544
b_baseline = -0.5994

valores_reais <- 
  cbind(parametro = c("(Intercept)", "center", "treat", "baseline"),
        valor = c(b0, b_center, b_treat, b_baseline)) %>% 
  as_tibble() %>% 
  mutate(valor = as.numeric(valor))

# 1) logbin frequentista -----------------------------------------------------
## coeficientes ------------------------------------------------------------
coef_logbin_freq_list <- 
  lapply(mod_logbin_freq, coef)

coef_logbin_freq <- bind_rows(
  lapply(names(coef_logbin_freq_list), function(nome) {
    data.frame(
      amostra = nome,
      parametro = names(coef_logbin_freq_list[[nome]]),
      valor = unname(coef_logbin_freq_list[[nome]])
    )
  })
) %>% 
  mutate(
    amostra_categoria = sub("_replica_[0-9]+$", "", amostra),
    amostra_categoria = stringr::str_sub(amostra_categoria, 9),
    amostra_categoria = as.numeric(amostra_categoria)
  ) %>% 
  group_by(amostra_categoria, parametro) %>%
  summarise(
    media_lgb = mean(valor),
    sd_lgb = sd(valor)
  ) %>% 
  arrange(amostra_categoria)


## vies e RMSE --------------------------------------------------------------------
vies_logbin_freq <- 
  bind_rows(
    lapply(names(coef_logbin_freq_list), function(nome) {
      data.frame(
        amostra = nome,
        parametro = names(coef_logbin_freq_list[[nome]]),
        valor = unname(coef_logbin_freq_list[[nome]])
      )
    })
  ) %>% 
  mutate(
    amostra_categoria = sub("_replica_[0-9]+$", "", amostra),
    amostra_categoria = stringr::str_sub(amostra_categoria, 9),
    amostra_categoria = as.numeric(amostra_categoria)
  ) %>% 
  left_join(valores_reais, by = "parametro") %>% 
  mutate(
    vies = valor.x - valor.y,
    vies_abs = abs(vies),
    erro2 = (valor.x - valor.y)^2
  ) %>%  
  group_by(amostra_categoria, parametro) %>%
  summarise(
    vies = mean(vies),
    vies_abs = mean(vies_abs),
    RMSE = sqrt(mean(erro2)),
    .groups = "drop"
  ) %>% 
  mutate(modelo = "Log-binomial freq.") %>% 
  mutate(
    vies_lgb  = round(vies, 4),
    vies_abs_lgb  = round(vies_abs,4),
    RMSE_lgb  = round(RMSE, 4)
  ) %>% 
  select(modelo, amostra_categoria, parametro, vies_lgb, RMSE_lgb)

## amplitude média ---------------------------------------------------------
ic_logbin_freq_lista <- 
  lapply(
    mod_logbin_freq,
    function(modelo) confint(modelo))


ic_logbin_freq <- map_df(
  names(ic_logbin_freq_lista),
  ~ ic_logbin_freq_lista[[.x]] %>%
    as.data.frame() %>%
    mutate(parametro = rownames(.),
           amostra = .x) %>%
    relocate(amostra, parametro)
) 

amplitude_logbin_freq <- 
  ic_logbin_freq %>% 
  mutate(
    amostra_categoria = sub("_replica_[0-9]+$", "", amostra),
    amostra_categoria = stringr::str_sub(amostra_categoria, 9),
    amostra_categoria = as.numeric(amostra_categoria),
    lim_inf = `2.5 %`, 
    lim_sup = `97.5 %`
  ) %>%
  select(amostra_categoria, parametro,lim_inf, lim_sup) %>% 
  na.omit() %>% 
  group_by(amostra_categoria, parametro) %>%
  summarise(
    media_lim_inf_lgb = mean(lim_inf),
    media_lim_sup_lgb = mean(lim_sup),
    sd_lim_inf_lgb = sd(lim_inf),
    sd_lim_sup_lgb = sd(lim_sup),
  ) %>% 
  mutate(
    amplitude_lgb_freq = media_lim_sup_lgb - media_lim_inf_lgb 
  ) %>% 
  select(amostra_categoria, parametro, amplitude_lgb_freq)

## probabilidade de cobertura ----------------------------------------------
cobertura_logbin_freq_df <- 
  ic_logbin_freq_lista %>% 
  map_dfr(
    ~as.data.frame(.x) %>% 
      tibble::rownames_to_column("parametro"),
    .id = "amostra") %>% 
  mutate(
    amostra_categoria = sub("_replica_[0-9]+$", "", amostra),
    amostra_categoria = stringr::str_sub(amostra_categoria, 9),
    amostra_categoria = as.numeric(amostra_categoria),
    amplitude = `97.5 %` - `2.5 %`
  ) %>% 
  left_join(valores_reais, by = "parametro") %>%
  mutate(ind_cobertura = ifelse(valor >= `2.5 %` & valor <= `97.5 %`, 1, 0))


cobertura_logbin_freq <- 
  cobertura_logbin_freq_df %>% 
  filter(!is.na(ind_cobertura)) %>% 
  group_by(amostra_categoria, parametro) %>% 
  summarise(
    total_convergiram = n(),
    total_coberto_ic = sum(ind_cobertura == 1),
    probabilidade_cobertura = total_coberto_ic / total_convergiram,
    probabilidade_cobertura_1000 = total_coberto_ic / 1000
  )


# 2) poisson sandwich --------------------------------------------------------
## coeficientes -----------------
coef_pois_sandwich_list <- 
  lapply(mod_pois_sandwich, coef)

coef_pois_sandwich_df <- 
  do.call(rbind, lapply(names(coef_pois_sandwich_list), function(n){
    data.frame(
      amostra = n,
      parametro = names(coef_pois_sandwich_list[[n]]),
      coef = as.numeric(coef_pois_sandwich_list[[n]]),
      row.names = NULL
    )
  }))

coef_pois_sandwich <- 
  coef_pois_sandwich_df %>% 
  mutate(
    amostra_categoria = sub("_replica_[0-9]+$", "", amostra),
    amostra_categoria = stringr::str_sub(amostra_categoria, 9),
    amostra_categoria = as.numeric(amostra_categoria)
  ) %>% 
  group_by(amostra_categoria, parametro) %>%
  summarise(
    media_pois = mean(coef),
    sd_pois = sd(coef)
  ) %>% 
  arrange(amostra_categoria)


## vies e RMSE --------------------------------------------------------------------
vies_coef_pois_sandwich <- 
  coef_pois_sandwich_df %>% 
  mutate(
    amostra_categoria = sub("_replica_[0-9]+$", "", amostra),
    amostra_categoria = stringr::str_sub(amostra_categoria, 9),
    amostra_categoria = as.numeric(amostra_categoria)
  ) %>% 
  left_join(valores_reais, by = "parametro") %>% 
  mutate(
    vies = coef - valor,
    vies_abs = abs(vies),
    erro2 = (coef - valor)^2
  ) %>%  
  group_by(amostra_categoria, parametro) %>%
  summarise(
    vies = mean(vies),
    vies_abs = mean(vies_abs),
    RMSE = sqrt(mean(erro2)),
    .groups = "drop"
  ) %>% 
  mutate(modelo = "Poisson robusto") %>% 
  mutate(
    vies_pois = round(vies, 4),
    vies_abs_pois = round(vies_abs,4),
    RMSE_pois = round(RMSE, 4)
  ) %>% 
  select(modelo, amostra_categoria, parametro, vies_pois, RMSE_pois)


## amplitude média ---------------------------------------------------------
ic_pois_sandwich_lista <- 
  lapply(
    mod_pois_sandwich,
    function(modelo) confint(modelo))


ic_pois_sandwich <- map_df(
  names(ic_pois_sandwich_lista),
  ~ ic_pois_sandwich_lista[[.x]] %>%
    as.data.frame() %>%
    mutate(parametro = rownames(.),
           amostra = .x) %>%
    relocate(amostra, parametro)
) 

amplitude_pois_sandwich <- 
  ic_pois_sandwich %>% 
  mutate(
    amostra_categoria = sub("_replica_[0-9]+$", "", amostra),
    amostra_categoria = stringr::str_sub(amostra_categoria, 9),
    amostra_categoria = as.numeric(amostra_categoria),
    lim_inf = `2.5 %`, 
    lim_sup = `97.5 %`
  ) %>%
  select(amostra_categoria, parametro,lim_inf, lim_sup) %>% 
  na.omit() %>% 
  group_by(amostra_categoria, parametro) %>%
  summarise(
    media_lim_inf_pois = mean(lim_inf),
    media_lim_sup_pois = mean(lim_sup),
    sd_lim_inf_pois = sd(lim_inf),
    sd_lim_sup_pois = sd(lim_sup),
  ) %>% 
  mutate(
    amplitude_pois = media_lim_sup_pois - media_lim_inf_pois 
  ) %>% 
  select(amostra_categoria, parametro, amplitude_pois)

## probabilidade de cobertura ----------------------------------------------
cobertura_pois_sandwich_df <- 
  ic_pois_sandwich_lista %>% 
  map_dfr(
    ~as.data.frame(.x) %>% 
      tibble::rownames_to_column("parametro"),
    .id = "amostra") %>% 
  mutate(
    amostra_categoria = sub("_replica_[0-9]+$", "", amostra),
    amostra_categoria = stringr::str_sub(amostra_categoria, 9),
    amostra_categoria = as.numeric(amostra_categoria),
    amplitude = `97.5 %` - `2.5 %`
  ) %>% 
  left_join(valores_reais, by = "parametro") %>%
  mutate(ind_cobertura = ifelse(valor >= `2.5 %` & valor <= `97.5 %`, 1, 0))


cobertura_pois_sandwich <- 
  cobertura_pois_sandwich_df %>% 
  filter(!is.na(ind_cobertura)) %>% 
  group_by(amostra_categoria, parametro) %>% 
  summarise(
    total_convergiram = n(),
    total_coberto_ic = sum(ind_cobertura == 1),
    probabilidade_cobertura = total_coberto_ic / total_convergiram
  )

cobertura_pois_sandwich

# 3) logbin bayesiano --------------------------------------------------------

## summary --------------

coef_logbin_bayes_summary <-
  lapply(mod_logbin_bayes, summary)

## statistcs -------------

bayes_statistics <- map_df(
  names(coef_logbin_bayes_summary),
  ~ coef_logbin_bayes_summary[[.x]]$statistics %>%
    as.data.frame() %>%
    mutate(parametro = rownames(.),
           amostra = .x),
  .id = NULL
) %>% 
  mutate(
    amostra_categoria = sub("_replica_[0-9]+$", "", amostra),
    amostra_categoria = stringr::str_sub(amostra_categoria, 9),
    amostra_categoria = as.numeric(amostra_categoria)
  ) %>% 
  mutate(
    parametro = case_when(
      parametro == "b0" ~ "(Intercept)",
      parametro == "b1" ~ "center",
      parametro == "b2" ~ "treat",
      parametro == "b3" ~ "baseline"
    )
  )


## quantiles ------

bayes_quantiles <- map_df(
  names(coef_logbin_bayes_summary),
  ~ coef_logbin_bayes_summary[[.x]]$quantiles %>%
    as.data.frame() %>%
    mutate(parametro = rownames(.),
           amostra = .x),
  .id = NULL
) %>% mutate(
  amostra_categoria = sub("_replica_[0-9]+$", "", amostra),
  amostra_categoria = stringr::str_sub(amostra_categoria, 9),
  amostra_categoria = as.numeric(amostra_categoria)
) %>% 
  mutate(
    parametro = case_when(
      parametro == "b0" ~ "(Intercept)",
      parametro == "b1" ~ "center",
      parametro == "b2" ~ "treat",
      parametro == "b3" ~ "baseline"
    )
  )

## coeficientes ------------------------------------------------------------

#### média -------------
bayes_media <- 
  bayes_statistics %>% 
  select(amostra_categoria, parametro, Mean) %>% 
  group_by(amostra_categoria, parametro) %>%
  summarise(
    media_lgb_bayes_media = mean(Mean),
    sd_lgb_bayes_media = sd(Mean)
  ) %>% 
  arrange(amostra_categoria)


#### mediana ---------
bayes_mediana <- 
  bayes_quantiles %>% 
  select(amostra_categoria, parametro, `50%`) %>% 
  mutate(mediana = `50%`) %>% 
  group_by(amostra_categoria, parametro) %>%
  summarise(
    media_lgb_bayes_mediana = mean(mediana),
    sd_lgb_bayes_mediana = sd(mediana)
  ) %>% 
  arrange(amostra_categoria)

## vies -------------
#### média -------------
vies_bayes_media <- 
  bayes_statistics %>% 
  select(amostra_categoria, parametro, Mean) %>% 
  left_join(valores_reais, by = "parametro") %>% 
  mutate(
    vies = Mean - valor,
    vies_abs = abs(vies),
    erro2 = (Mean - valor)^2
  ) %>% 
  group_by(amostra_categoria, parametro) %>%
  summarise(
    vies = mean(vies),
    vies_abs = mean(vies_abs),
    RMSE = sqrt(mean(erro2)),
  ) %>% 
  arrange(amostra_categoria) %>% 
  mutate(modelo = "Bayesian media") %>% 
  mutate(
    vies_bayes_media = round(vies, 4),
    vies_absbayes_media = round(vies_abs,4),
    RMSE_bayes_media = round(RMSE, 4)
  ) %>% 
  select(modelo, amostra_categoria, parametro, vies_bayes_media, RMSE_bayes_media)

# bayes_media %>% 
# left_join(valores_reais, by = "parametro") %>% 
# mutate(
#   vies = media_lgb_bayes_media - valor,
#   vies_abs = abs(vies)
# )

#### mediana ---------
vies_bayes_mediana <- 
  bayes_quantiles %>% 
  select(amostra_categoria, parametro, `50%`) %>% 
  mutate(mediana = `50%`) %>% 
  select(amostra_categoria, parametro, mediana) %>% 
  left_join(valores_reais, by = "parametro") %>% 
  mutate(
    vies = mediana - valor,
    vies_abs = abs(vies),
    erro2 = (mediana - valor)^2
  ) %>% 
  group_by(amostra_categoria, parametro) %>%
  summarise(
    vies = mean(vies),
    vies_abs = mean(vies_abs),
    RMSE = sqrt(mean(erro2)),
  ) %>% 
  arrange(amostra_categoria) %>% 
  mutate(modelo = "Bayesian media") %>% 
  mutate(
    vies_bayes_mediana = round(vies, 4),
    vies_absbayes_mediana = round(vies_abs,4),
    RMSE_bayes_mediana = round(RMSE, 4)
  ) %>% 
  select(modelo, amostra_categoria, parametro, vies_bayes_mediana, RMSE_bayes_mediana)


## amplitude -------------

amplitude_bayes <- 
  bayes_quantiles %>% 
  select(amostra_categoria, parametro, `2.5%`, `97.5%`) 

amplitude_bayes_media <- 
  amplitude_bayes %>% 
  mutate(
    lim_inf = `2.5%`, 
    lim_sup = `97.5%`
  ) %>%
  select(amostra_categoria, parametro,lim_inf, lim_sup) %>% 
  na.omit() %>% 
  group_by(amostra_categoria, parametro) %>%
  summarise(
    media_lim_inf = mean(lim_inf),
    media_lim_sup = mean(lim_sup),
    sd_lim = sd(lim_inf),
    sd_lim = sd(lim_sup),
  ) %>% 
  mutate(
    amplitude_media = media_lim_sup - media_lim_inf 
  ) 


## probabilidade cobertura -------------
cobertura_bayes_df <- 
  amplitude_bayes %>% 
  left_join(valores_reais, by = "parametro") %>%
  mutate(ind_cobertura = ifelse(valor >= `2.5%` & valor <= `97.5%`, 1, 0))


cobertura_bayes <-
  cobertura_bayes_df %>%
  filter(!is.na(ind_cobertura)) %>%
  group_by(amostra_categoria, parametro) %>%
  summarise(
    total_convergiram = n(),
    total_coberto_ic = sum(ind_cobertura == 1),
    probabilidade_cobertura = total_coberto_ic / total_convergiram
  )



# 4) analise visual -----------------------------------------------------

## coeficientes ------------------------------------------------------------
coef_logbin_freq
coef_pois_sandwich
bayes_media
bayes_mediana

## vies --------------------------------------------------------------------
vies_logbin_freq
vies_coef_pois_sandwich
vies_bayes_media
vies_bayes_mediana

## amplitude ---------------------------------------------------------------
amplitude_logbin_freq 
amplitude_pois_sandwich
amplitude_bayes <- amplitude_bayes_media %>% select(amostra_categoria, parametro, amplitude_media)
amplitude_bayes

## probabilidade cobertura -------------------------------------------------
cobertura_logbin_freq
cobertura_pois_sandwich
cobertura_bayes

# 5) tratamento final  -----------------------------------------------------

## coeficientes ------------------------------------------------------------
coef1 <- coef_logbin_freq
coef2 <- coef_pois_sandwich
coef3 <- bayes_media
coef4 <- bayes_mediana

coef_lista <- list(coef1, coef2, coef3, coef4)

coefs <- reduce(
  coef_lista,
  left_join,
  by = c("amostra_categoria", "parametro")
) %>% 
  mutate(
    media_lgb = round(media_lgb, 4),
    sd_lgb = round(sd_lgb, 4),
    media_pois = round(media_pois, 4),
    sd_pois = round(sd_pois, 4),
    media_lgb_bayes_media = round(media_lgb_bayes_media, 4),
    sd_lgb_bayes_media = round(sd_lgb_bayes_media, 4),
    media_lgb_bayes_mediana = round(media_lgb_bayes_mediana, 4),
    sd_lgb_bayes_mediana = round(sd_lgb_bayes_mediana, 4)
  )

## vies --------------------------------------------------------------------
v1 <- 
  vies_logbin_freq %>% 
  select(-modelo)

v2 <-
  vies_coef_pois_sandwich %>% 
  select(-modelo)

v3 <- 
  vies_bayes_media %>% 
  select(-modelo)

v4 <- 
  vies_bayes_mediana %>% 
  select(-modelo)

v_lista <- list(v1, v2, v3, v4)

vies <- reduce(
  v_lista,
  left_join,
  by = c("amostra_categoria", "parametro")
) 



## amplitude ---------------------------------------------------------------
amp1 <- 
  amplitude_logbin_freq 
amp2 <- 
  amplitude_pois_sandwich 
amp3 <- 
  amplitude_bayes_media %>% 
  select(amostra_categoria, parametro, amplitude_media) 

amp_lista <- list(amp1, amp2, amp3)

amp <- reduce(
  amp_lista,
  left_join,
  by = c("amostra_categoria", "parametro")
) %>% 
  mutate(
    amplitude_lgb_freq = round(amplitude_lgb_freq, 4),
    amplitude_pois = round(amplitude_pois, 4),
    amplitude_media = round(amplitude_media, 4),
  )

## probabilidade cobertura -------------------------------------------------
cob1 <- cobertura_logbin_freq %>% 
  select(amostra_categoria, parametro, probabilidade_cobertura) %>% 
  mutate(probabilidade_cobertura = round(probabilidade_cobertura, 3)) %>% 
  rename(prob_c_freq = probabilidade_cobertura)


cob2 <- cobertura_pois_sandwich %>% 
  select(amostra_categoria, parametro, probabilidade_cobertura) %>% 
  mutate(probabilidade_cobertura = round(probabilidade_cobertura, 4)) %>% 
  rename(prob_c_poiss = probabilidade_cobertura)


cob3 <- cobertura_bayes %>% 
  select(amostra_categoria, parametro, probabilidade_cobertura) %>% 
  mutate(probabilidade_cobertura = round(probabilidade_cobertura, 4)) %>% 
  rename(prob_c_bayes = probabilidade_cobertura)

cob_lista <- list(cob1, cob2, cob3)

cob <- reduce(
  cob_lista,
  left_join,
  by = c("amostra_categoria", "parametro")
) 

# cob_amp -----------------------------------------------------------------
cob_amp_lista <- list(cob, amp)

cob_amp <- reduce(
  cob_amp_lista,
  left_join,
  by = c("amostra_categoria", "parametro")
) %>% 
  select(amostra_categoria, parametro, 
         prob_c_freq, amplitude_lgb_freq,
         prob_c_poiss, amplitude_pois,
         prob_c_bayes, amplitude_media
  )


# reordenando -------------------------------------------------------------
coefs <- 
  coefs %>% 
  mutate(
    parametro = factor(
      parametro,
      levels = c("(Intercept)",
                 "center",
                 "treat",
                 "baseline")
    )
  ) %>% 
  arrange(amostra_categoria, parametro)

vies <-
  vies %>% 
  mutate(
    parametro = factor(
      parametro,
      levels = c("(Intercept)",
                 "center",
                 "treat",
                 "baseline")
    )
  ) %>% 
  arrange(amostra_categoria, parametro)

cob_amp <- 
  cob_amp %>% 
  mutate(
    parametro = factor(
      parametro,
      levels = c("(Intercept)",
                 "center",
                 "treat",
                 "baseline")
    )
  ) %>% 
  arrange(amostra_categoria, parametro)


# 6) saida ----------------------------------------------------------------
coefs
vies
cob_amp

write.csv(coefs, "E-NOVA-SIMULACAO/0-tabelas/convergiram_coeficientes.csv")
write.csv(vies, "E-NOVA-SIMULACAO/0-tabelas/convergiram_vies.csv")
write.csv(cob_amp, "E-NOVA-SIMULACAO/0-tabelas/convergiram_cob_amp.csv")
