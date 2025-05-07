library(rdmulti)
library(rdrobust)
library(rddensity)
library(tidyverse)
library(scales)
library(splines)

df <- read_csv("C:\\Users\\tdechelotte\\Desktop\\alldpe_group_metrics_scott.csv")

df$type_logement <- factor(df$type_logement)
df$periode_construction <- factor(df$periode_construction)
df$type_energie_chauffage <- factor(df$type_energie_chauffage)

# Create the energy_efficient variable (1 if ep_conso_5_usages_m2 <= 330, otherwise 0)
df <- df %>% mutate(pre_shopping = if_else(interval_dpe_remplacant <= 90, 1, 0, missing = 0),
                      post_shopping = if_else(interval_ancien_dpe <= 90, 1, 0, missing = 0),
                      diff_indicator = post_shopping - pre_shopping) %>%
             filter(prior_330 < 1,epsilon_330 <= 1,prior_250 < 1, epsilon_250 <= 1, prior_420 < 1, epsilon_420 <= 1) %>%
             select(ep_conso_5_usages_m2, pre_shopping, post_shopping, diff_indicator, prior_330, epsilon_330, prior_250, epsilon_250, prior_420, epsilon_420)

df <- df %>% 
  mutate(
    cutoff = cut(
      ep_conso_5_usages_m2,
      breaks = c(10, 290, 375, 800),
      labels = c(250, 330, 420),
      right  = FALSE
    )
  ) %>% 
  mutate(cutoff = as.numeric(as.character(cutoff))) %>%
  filter(!is.na(cutoff))

cutoffs <- c(250, 330, 420)
delta <- 10 # width of the donut
df_donut <- df %>%
  filter(abs(ep_conso_5_usages_m2 - 330) > delta,
         abs(ep_conso_5_usages_m2 - 250) > delta,
         abs(ep_conso_5_usages_m2 - 420) > delta)

# 1. Primary RD estimation (local linear, triangular kernel) ----

res_mc <- rdmc(
  Y          = df_donut$post_shopping,
  X          = df_donut$ep_conso_5_usages_m2,
  C          = df_donut$cutoff,
  pvec       = rep(1, length(cutoffs)),           # local-lin
  kernelvec  = rep("triangular", length(cutoffs)),# triangular kernel
  verbose    = TRUE
)

print(res_mc)  # pooled + cutoff-specific τ̂, SEs, CIs  [oai_citation:1‡CRAN](https://cran.r-project.org/web/packages/rdmulti/rdmulti.pdf)

#––– 2. Multi-cutoff RD plot
rdmcplot(
  Y          = df_donut$post_shopping,
  X          = df_donut$ep_conso_5_usages_m2,
  C          = df_donut$cutoff,
  pvec       = rep(1, length(cutoffs)),
  kernelvec  = rep("triangular", length(cutoffs)),
  ci         = 95
)