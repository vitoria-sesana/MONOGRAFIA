# Gráficos Boxplot e Percentual 

# rotina ------------------------------------------------------------------
source("scripts/0-rotina.R", encoding = "UTF-8")

# boxplot -----------------------------------------------------------------

ggbox <- 
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
  scale_x_continuous(breaks = seq(0, 70, 10), limits = c(0,70)) +
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

ggbox

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
    center = ifelse(center == 1, "Centro 1", "Centro 2")
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

#### valores finais -------
df1 <- df1 %>% rename(categoria = center)
df2 <- df2 %>% rename(categoria = treat)
df3 <- df3 %>% rename(categoria = baseline)

df1$covariavel <- "Centro" 
df2$covariavel <- "Tratamento"
df3$covariavel <- "Estado inicial"

df <- 
  rbind(df1, df2, df3) %>% 
  mutate(
    nomes = paste0(covariavel, ": ", categoria) 
  ) %>% 
  mutate(
    nomes = 
      factor(
        nomes,
        levels = 
          c(
            "Centro: Centro 1",
            "Centro: Centro 2",
            "Tratamento: Ativo",
            "Tratamento: Placebo",
            "Estado inicial: Bom",
            "Estado inicial: Ruim"
          )
        ),
    texto = 
      paste0(n, "(", round(pct*100,2),"%)")
    ) 

df


# gráfico percentual ----------------------------------------------------------

gg_cov_desf <-
  ggplot(df, aes(
    y = forcats::fct_rev(nomes),
    x = pct,
    fill = outcome
  )) +
    geom_col(position = "fill") +
    geom_text(
      aes(label = texto),
      position = position_fill(vjust = 0.5),
      size = 2.5,
      color = "white"
    ) +
    scale_x_continuous(labels = scales::percent) +
    labs(
      x = "Percentual",
      y = "Categoria das covariáveis",
      fill = "Desfecho"
    ) +
    theme_minimal() +
    scale_fill_manual(
      values = c("#1F77B4", "#D62728")
    ) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.text   = element_text(color = "grey20"),
    legend.title  = element_text(color = "grey20",face = "bold"),
    axis.text.x   = element_text(color = "grey20",size = 7),
    axis.text.y   = element_text(color = "grey20",size = 7),
    axis.title.x  = element_text(color = "grey20",face = "bold"),
    axis.title.y  = element_text(color = "grey20",face = "bold")
  )

gg_cov_desf

# validando valores
respiratory4 %>% 
  group_by(treat, outcome) %>% 
  summarise(n=n())

# saídas -----------------------------------------------------------------------

ggsave(
  "plots/1-descritiva/ggbox.pdf",
  plot = ggbox,
  device = cairo_pdf,
  width = 14,
  height = 12,
  units = "cm"
)

ggsave(
  "plots/1-descritiva/grafico_covariavel_desfecho.pdf",
  plot = gg_cov_desf,
  device = cairo_pdf,
  width = 14,
  height = 12,
  units = "cm"
)
