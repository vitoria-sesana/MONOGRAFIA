# rotina ------------------------------------------------------------------
source("scripts/0-rotina.R", encoding = "UTF-8")

# valores reais dos parâmetros --------------------------------------------

modelo_base <- 
  readRDS("saidas/1-saida-aplicacao/ajuste_logbin_frequentista.rds") 

valores_reais <- 
  modelo_base$modelo_logbin_frequentista$coefficients %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column() %>% 
  rename(parametro_descricao = "rowname", valor = ".") %>% 
  mutate(
    parametro =  case_when(
      parametro_descricao == "(Intercept)" ~ "b0",
      parametro_descricao == "center1" ~ "b1",
      parametro_descricao == "treatP" ~ "b2",
      parametro_descricao == "baseline0" ~ "b3",
      .default = "NA"
    )
  ) %>% 
  select(parametro, valor) %>% 
  as_tibble()

# leitura e tratamento ----------------------------------------------------

## modelos ajustados --------------------
mod_logbin_freq_all <-  readRDS("saidas/2-saida-simulacao/M1-logbin-frequentista/ajustes_logbin_frequentista.rds")
mod_pois_sandwich_all <- readRDS("saidas/2-saida-simulacao/M2-poisson-robusto/ajustes_poisson_robusto.rds")
mod_logbin_bayes_all <- readRDS("saidas/2-saida-simulacao/M3-logbin-bayesiano/ajustes_logbin_bayesiano.rds")

## listas das bases convergidas --------------------
lista_bases_convergidas <- 
  read.csv("saidas/2-saida-simulacao/bases_convergidas.csv")

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


# 1) LOGBIN FREQUENTISTA -----------------------------------------------------

## Média e Desvio-Padrão -----------------------------------------------------
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
  arrange(amostra_categoria) %>% 
  mutate(
    parametro =  case_when(
      parametro == "(Intercept)" ~ "b0",
      parametro == "center" ~ "b1",
      parametro == "treat" ~ "b2",
      parametro == "baseline" ~ "b3",
      .default = "NA"
    )
  )


## Vieses e REQM --------------------------------------------------------------------

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
  mutate(
    parametro =  case_when(
      parametro == "(Intercept)" ~ "b0",
      parametro == "center" ~ "b1",
      parametro == "treat" ~ "b2",
      parametro == "baseline" ~ "b3",
      .default = "NA"
    )
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

## Amplitude Média ---------------------------------------------------------

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
  select(amostra_categoria, parametro, amplitude_lgb_freq) %>% 
  mutate(
    parametro =  case_when(
      parametro == "(Intercept)" ~ "b0",
      parametro == "center" ~ "b1",
      parametro == "treat" ~ "b2",
      parametro == "baseline" ~ "b3",
      .default = "NA"
    )
  ) 

## Probabilidade de Cobertura ----------------------------------------------

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
  mutate(
    parametro =  case_when(
      parametro == "(Intercept)" ~ "b0",
      parametro == "center" ~ "b1",
      parametro == "treat" ~ "b2",
      parametro == "baseline" ~ "b3",
      .default = "NA"
    )
  ) %>% 
  left_join(valores_reais, by = "parametro") %>% 
  mutate(ind_n_na = ifelse(is.na(`97.5 %`), 0, 1)) %>% 
  mutate(ind_cobertura = ifelse(valor >= `2.5 %` & valor <= `97.5 %`, 1, 0)) %>% 
  mutate(ind_cobertura = ifelse(is.na(ind_cobertura) | ind_cobertura == 0, 0, 1))  


cobertura_logbin_freq <-
  cobertura_logbin_freq_df %>%
  # filter(!is.na(ind_cobertura)) %>%
  mutate(ind_cobertura = ifelse(is.na(ind_cobertura) | ind_cobertura == 0, 0, 1)) %>% 
  group_by(amostra_categoria, parametro) %>% 
  summarise(
    total_convergiram = n(),
    total_coberto_ic = sum(ind_cobertura == 1),
    probabilidade_cobertura = total_coberto_ic / total_convergiram,
    probabilidade_cobertura_1000 = total_coberto_ic / 1000
  ) 


# 2) POISSON SANDWICH --------------------------------------------------------

## Média e SD ------------------------------------------------------------

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
  arrange(amostra_categoria) %>% 
  mutate(
    parametro =  case_when(
      parametro == "(Intercept)" ~ "b0",
      parametro == "center" ~ "b1",
      parametro == "treat" ~ "b2",
      parametro == "baseline" ~ "b3",
      .default = "NA"
    )
  ) 


## Vieses e REQM --------------------------------------------------------------------
vies_coef_pois_sandwich <- 
  coef_pois_sandwich_df %>% 
  mutate(
    amostra_categoria = sub("_replica_[0-9]+$", "", amostra),
    amostra_categoria = stringr::str_sub(amostra_categoria, 9),
    amostra_categoria = as.numeric(amostra_categoria)
  ) %>% 
  mutate(
    parametro =  case_when(
      parametro == "(Intercept)" ~ "b0",
      parametro == "center" ~ "b1",
      parametro == "treat" ~ "b2",
      parametro == "baseline" ~ "b3",
      .default = "NA"
    )
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


## Amplitude Média ---------------------------------------------------------

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
  select(amostra_categoria, parametro, amplitude_pois) %>% 
  mutate(
    parametro =  case_when(
      parametro == "(Intercept)" ~ "b0",
      parametro == "center" ~ "b1",
      parametro == "treat" ~ "b2",
      parametro == "baseline" ~ "b3",
      .default = "NA"
    )
  ) 

## Probabilidade de Cobertura ----------------------------------------------

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
  mutate(
    parametro =  case_when(
      parametro == "(Intercept)" ~ "b0",
      parametro == "center" ~ "b1",
      parametro == "treat" ~ "b2",
      parametro == "baseline" ~ "b3",
      .default = "NA"
    )
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


# 3) LOGBIN BAYESIANO --------------------------------------------------------

## resumo --------------

coef_logbin_bayes_summary <-
  lapply(mod_logbin_bayes, summary)

## estatisticas -------------

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
  ) 

## quantis ------

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
) 

## Média e SD ------------------------------------------------------------

#### Média e SD das média -------------
bayes_media <- 
  bayes_statistics %>% 
  select(amostra_categoria, parametro, Mean) %>% 
  group_by(amostra_categoria, parametro) %>%
  summarise(
    media_lgb_bayes_media = mean(Mean),
    sd_lgb_bayes_media = sd(Mean)
  ) %>% 
  arrange(amostra_categoria)


#### Média e SD das medianas mediana ---------
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

## Vieses e REQM --------------------------------------------------------------------

#### Vieses e REQM das médias -------------
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

#### Vieses e REQM das medianas ---------
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


## Quantílico --------------------------------------------------------------

### Amplitude Média ---------------------------------------------------------

amplitude_bayes_quantilico_geral <- 
  bayes_quantiles %>% 
  select(amostra_categoria, parametro, `2.5%`, `97.5%`) 

amplitude_bayes_quantilica <- 
  amplitude_bayes_quantilico_geral %>% 
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
    amplitude_bayes_quantilica = media_lim_sup - media_lim_inf 
  ) %>% 
  select(amostra_categoria, parametro, amplitude_bayes_quantilica)


### Probabilidade cobertura -------------
cobertura_bayes_quantilica_geral <- 
  amplitude_bayes_quantilico_geral %>% 
  left_join(valores_reais, by = "parametro") %>%
  mutate(ind_cobertura = ifelse(valor >= `2.5%` & valor <= `97.5%`, 1, 0))


cobertura_bayes_quantilica <-
  cobertura_bayes_quantilica_geral %>%
  filter(!is.na(ind_cobertura)) %>%
  group_by(amostra_categoria, parametro) %>%
  summarise(
    total_convergiram = n(),
    total_coberto_ic = sum(ind_cobertura == 1),
    probabilidade_cobertura_quantilica = total_coberto_ic / total_convergiram
  )

## HPD --------------------------------------------------------------

### Amplitude HPD ------
amplitude_bayes_HPD_geral <- 
  lapply(mod_logbin_bayes, coda::HPDinterval) %>% 
  lapply(as.data.frame)


amplitude_bayes_HPD_df <- 
  do.call(rbind, lapply(names(amplitude_bayes_HPD_geral), function(n) {
    df <- as.data.frame(amplitude_bayes_HPD_geral[[n]])
    df$parametro <- rownames(df)
    df$amostra <- n
    df
  })) %>% 
  select(amostra, parametro, lower, upper) %>% 
  mutate(amp_HPD = upper - lower) %>% 
  mutate(
    amostra_categoria = sub("_replica_[0-9]+$", "", amostra),
    amostra_categoria = stringr::str_sub(amostra_categoria, 9),
    amostra_categoria = as.numeric(amostra_categoria)
  ) 


amplitude_bayes_HPD <- 
  amplitude_bayes_HPD_df %>% 
  group_by(amostra_categoria, parametro) %>% 
  summarise(
    amplitude_bayes_HPD = as.numeric(round(mean(amp_HPD), 4))
  )


### Probabilidade Cobertura HPD ------

cobertura_HPD_df <- 
  amplitude_bayes_HPD_df %>% 
  left_join(valores_reais, by = "parametro") %>%
  mutate(ind_cobertura = ifelse(valor >= lower & valor <= upper, 1, 0))

cobertura_bayes_HPD <- 
  cobertura_HPD_df %>% 
  filter(!is.na(ind_cobertura)) %>% 
  group_by(amostra_categoria, parametro) %>% 
  summarise(
    total_convergiram = n(),
    total_coberto_ic = sum(ind_cobertura == 1),
    probabilidade_cobertura_HPD = total_coberto_ic / total_convergiram
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
amplitude_bayes_quantilica
amplitude_bayes_HPD

## probabilidade cobertura -------------------------------------------------
cobertura_logbin_freq
cobertura_pois_sandwich
cobertura_bayes_quantilica
cobertura_bayes_HPD

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
  amplitude_bayes_quantilica
amp4 <- 
  amplitude_bayes_HPD

amp_lista <- list(amp1, amp2, amp3, amp4)

amp <- reduce(
  amp_lista,
  left_join,
  by = c("amostra_categoria", "parametro")
) %>% 
  mutate(
    amplitude_lgb_freq = round(amplitude_lgb_freq, 4),
    amplitude_pois = round(amplitude_pois, 4),
    amplitude_bayes_quantilica = round(amplitude_bayes_quantilica, 4),
    amplitude_bayes_HPD = round(amplitude_bayes_HPD, 4)
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


cob3 <- cobertura_bayes_quantilica %>% 
  select(amostra_categoria, parametro, probabilidade_cobertura_quantilica) %>% 
  mutate(probabilidade_cobertura_quantilica = round(probabilidade_cobertura_quantilica, 4)) 

cob4 <- cobertura_bayes_HPD %>% 
  select(amostra_categoria, parametro, probabilidade_cobertura_HPD) %>% 
  mutate(probabilidade_cobertura_HPD = round(probabilidade_cobertura_HPD, 4))

cob_lista <- list(cob1, cob2, cob3, cob4)

cob <- reduce(
  cob_lista,
  left_join,
  by = c("amostra_categoria", "parametro")
) 

## cobertura e amplitude -----------------------------------------------------
cob_amp_lista <- list(cob, amp)

cob_amp <- reduce(
  cob_amp_lista,
  left_join,
  by = c("amostra_categoria", "parametro")
) %>% 
  select(amostra_categoria, parametro, 
         prob_c_freq, amplitude_lgb_freq,
         prob_c_poiss, amplitude_pois,
         probabilidade_cobertura_quantilica, amplitude_bayes_quantilica,
         probabilidade_cobertura_HPD, amplitude_bayes_HPD
  )


# 6) reordenando -------------------------------------------------------------
coefs <- 
  coefs %>% 
  mutate(
    parametro = factor(
      parametro,
      levels = c("b0",
                 "b1",
                 "b2",
                 "b3")
    )
  ) %>% 
  arrange(amostra_categoria, parametro)

vies <-
  vies %>% 
  mutate(
    parametro = factor(
      parametro,
      levels = c("b0",
                 "b1",
                 "b2",
                 "b3")
    )
  ) %>% 
  arrange(amostra_categoria, parametro)

cob_amp <- 
  cob_amp %>% 
  mutate(
    parametro = factor(
      parametro,
      levels = c("b0",
                 "b1",
                 "b2",
                 "b3")
    )
  ) %>% 
  arrange(amostra_categoria, parametro)



# 6) saida ----------------------------------------------------------------
coefs
vies
cob_amp

write.csv(coefs, "saidas/2-saida-simulacao/R3-convergidas/conv_media_sd.csv")
write.csv(vies, "saidas/2-saida-simulacao/R3-convergidas/conv_vies_reqm.csv")
write.csv(cob_amp, "saidas/2-saida-simulacao/R3-convergidas/conv_pc_am.csv")

