# Gráficos Boxplot e Percentual 

# rotina ------------------------------------------------------------------
source("scripts/0-rotina.R", encoding = "UTF-8")

# Boxplot Age -------------------------------------------------------------

ggbox_age <- 
  ggplot(respiratory4, aes(x = age)) +
  stat_boxplot(
    geom = "errorbar",
    width = 0.2,       
    linewidth = 0.3
  ) +
  geom_boxplot(
    fill      = "skyblue",
    color     = "black",        
      # linetype  = "dotted",        
    width     = 0.2,
    outlier.shape = NA,         
    fatten    = 1.4              
  ) +
  coord_flip() +
  scale_x_continuous(
    breaks = seq(0, 70, 10),
    limits = c(0, 75),
    expand = c(0, 0)
  ) +
  theme_minimal() +
  theme(
    axis.ticks.x = element_blank(),
    axis.text.x  = element_blank(),
    axis.title.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey70", size = 0.3, linetype = "dotted"),
    panel.grid.minor.y = element_line(color = "grey70", size = 0.3, linetype = "dotted"),
    panel.border = element_rect(color = "grey70", fill = NA, size = 0.5),
    axis.text.y   = element_text(color = "grey20",size = 7),
    axis.title.y  = element_text(color = "grey20",face = "bold")
  ) +
  xlab("Idade") +
  ylim(-0.5,0.5) 

ggbox_age



# Boxplot Age x outcome ---------------------------------------------------
df_boxplot_age_outcome <- 
  respiratory4 %>% 
  select(outcome, age) %>% 
  mutate(
    outcome = ifelse(outcome == 0, "Ruim", "Bom"),
    outcome = factor(
      outcome,
      levels = c("Bom", "Ruim")
    )
  )

ggbox_age_outcome <- 
  ggplot(
    df_boxplot_age_outcome,
    aes(
      x = outcome, 
      y = age,
      fill = outcome
    )
  ) +
  stat_boxplot(
    geom = "errorbar",
    width = 0.5,       
    linewidth = 0.7
  ) +
  geom_boxplot(
    color = "black",        
    width = 0.6,
    outlier.shape = NA,         
    fatten = 1.4              
  ) +
  scale_fill_manual(
    values = c(
      "Bom" = "#27AE60",
      "Ruim" = "#C0392B"
    )
  ) +
  coord_flip() +
  scale_y_continuous(
    breaks = seq(0, 70, 10),
    limits = c(0, 75),
    expand = c(0, 0)
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_line(
      color = "grey20", 
      linetype = "dotted", 
      linewidth = 0.05
    ),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.border = element_rect(
      color = "grey70",
      fill = NA,
      linewidth = 0.5
    ),
    axis.text.y = element_text(
      color = "grey20",
      size = 7
    ),
    axis.title.y = element_text(
      color = "grey20",
      face = "bold"
    ),
    legend.position = "none"
  ) +
  labs(
    x = "Desfecho",
    y = "Idade"
  )

ggbox_age_outcome


# gráfico percentual ------------------------------------------------------

## tratamento dos dados -----------

#### desfecho x centro -------
df1 <- 
  respiratory4 %>% 
  group_by(outcome, center) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(center) %>% 
  mutate(pct = round((n / sum(n)), 2) ) %>% 
  mutate(
    outcome = ifelse(outcome == 0, "Ruim", "Bom"),
    center = ifelse(center == 1, "1", "2")
  ) %>% 
  mutate(outcome = as.factor(outcome))

#### desfecho x tratamento -------
df2 <- 
  respiratory4 %>% 
  group_by(outcome, treat) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(treat) %>% 
  mutate(pct = round((n / sum(n)), 2) ) %>% 
  mutate(
    outcome = ifelse(outcome == 0, "Ruim", "Bom"),
    treat = ifelse(treat == "A", "Ativo", "Placebo")
  ) %>% 
  mutate(outcome = as.factor(outcome))

#### desfecho x estado inicial -------
df3 <- 
  respiratory4 %>% 
  group_by(outcome, baseline) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(baseline) %>% 
  mutate(pct = round((n / sum(n)), 2) ) %>% 
  mutate(
    outcome = ifelse(outcome == 0, "Ruim", "Bom"),
    baseline = ifelse(baseline == 0, "Ruim", "Bom")
  ) %>% 
  mutate(outcome = as.factor(outcome))

#### desfecho x sexo -------
df4 <- 
  respiratory4 %>% 
  group_by(outcome, sex) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(sex) %>% 
  mutate(pct = round((n / sum(n)), 2) ) %>% 
  mutate(
    outcome = ifelse(outcome == 0, "Ruim", "Bom"),
    sex = ifelse(sex == 'M', "Masculino", "Feminino")
  ) %>% 
  mutate(outcome = as.factor(outcome))

#### valores finais -------
df1 <- df1 %>% rename(categoria = center)
df2 <- df2 %>% rename(categoria = treat)
df3 <- df3 %>% rename(categoria = baseline)
df4 <- df4 %>% rename(categoria = sex)


df1$covariavel <- "Centro" 
df2$covariavel <- "Tratamento"
df3$covariavel <- "Estado inicial"
df4$covariavel <- "Sexo"

df <- 
  rbind(df3, df1, df4, df2) %>% 
  mutate(
    nomes = paste0(covariavel, ": ", categoria) 
  ) %>% 
  mutate(
    nomes = 
      factor(
        nomes,
        levels = 
          c(
            "Centro: 1",
            "Centro: 2",
            "Tratamento: Ativo",
            "Tratamento: Placebo",
            "Estado inicial: Bom",
            "Estado inicial: Ruim",
            "Sexo: Masculino",
            "Sexo: Feminino"
          )
        ),
    texto = 
      paste0(n, "(", round(pct*100,2),"%)")
    ) 

df$nomes <- factor(
  df$nomes,
  levels = c(
    "Estado inicial: Bom",
    "Estado inicial: Ruim",
    "Centro: 1",
    "Centro: 2",
    "Tratamento: Placebo",
    "Tratamento: Ativo",
    "Sexo: Feminino",
    "Sexo: Masculino"
  )
)


df



# Representatividade ------------------------------------------------------

gg_covariables_outcome <-
  ggplot(
    df,
    aes(
      y = forcats::fct_rev(categoria),
      x = pct,
      fill = outcome
    )
  ) +
  geom_col(
    position = "fill",
    width = 0.67,
    color = "grey20",
    linewidth = 0.2
  ) +
  shadowtext::geom_shadowtext(
    aes(label = texto),
    position = position_fill(vjust = 0.5),
    size = 1.2,
    size.unit = "pt",
    color = "white",      # Cor de dentro do texto
    bg.color = "grey20",  # Cor da "borda" (contorno)
    bg.r = 0.1            # Espessura do contorno (ajuste se precisar)
  ) +
  scale_x_continuous(
    labels = scales::percent
  ) +
  scale_fill_manual(
    values = c(
      "Bom" = "#27AE60",
      "Ruim" = "#C0392B"
    )
  ) +
  facet_grid(
    covariavel ~ .,
    scales = "free_y",
    space = "free_y",
    switch = "y"
  ) +
  labs(
    x = "Percentual",
    y = NULL,
    fill = "Desfecho"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_line(
      color = "grey95",
      # linetype = "dotted",
      linewidth = 0.05
    ),
    panel.grid.minor.x = element_line(
      color = "white",
      # linetype = "dotted",
      linewidth = 0.05
    ),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    
    # Covariável à esquerda
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text.y.left = element_text(
      color = "grey20",
      face = "bold",
      size = 7
    ),
    panel.border = element_rect(
      color = "grey70",
      fill = NA,
      linewidth = 0.5
    ),
    axis.text.x = element_text(
      color = "grey20",
      size = 6
    ),
    
    axis.text.y = element_text(
      color = "grey20",
      size = 6
    ),
    
    axis.title.x = element_text(
      color = "grey20",
      face = "bold",
      size = 6
    ),
    
    legend.text = element_text(
      color = "grey20",
      size = 5
    ),
    
    legend.title = element_text(
      color = "grey20",
      face = "bold",
      size = 6
    )
  )

gg_covariables_outcome


## 

ggbox_age
ggbox_age_outcome
gg_covariables_outcome

# saídas -----------------------------------------------------------------------

ggsave(
  "plots/1-plots-descritiva/ggbox_age.pdf",
  plot = ggbox_age,
  device = cairo_pdf,
  width = 14,
  height = 12,
  units = "cm"
)


ggsave(
  "plots/1-plots-descritiva/ggbox_age_outcome.pdf",
  plot = ggbox_age_outcome,
  device = cairo_pdf,
  width = 16,
  height = 10,
  units = "cm"
)


ggsave(
  "plots/1-plots-descritiva/grafico_covariavel_desfecho.pdf",
  plot = gg_covariables_outcome,
  device = cairo_pdf,
  width = 13,
  height = 8,
  units = "cm"
)
