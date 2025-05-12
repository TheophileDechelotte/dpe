# Load packages
library(rdrobust)
library(rddensity)
library(tidyverse)
library(readr)
library(ggplot2)
library(scales)
library(dplyr)
library(splines)

df <- read_csv("/Users/theophiledechelotte/Library/CloudStorage/OneDrive-Personnel/dpe-data/alldpe_group_metrics_scott_new.csv")

df2$type_logement <- factor(df2$type_logement)
df2$periode_construction <- factor(df2$periode_construction)
df2$type_energie_chauffage <- factor(df2$type_energie_chauffage)

# Create the energy_efficient variable (1 if ep_conso_5_usages_m2 <= 330, otherwise 0)
df2 <- df2 %>% mutate(pre_shopping = if_else(interval_dpe_remplacant <= 90, 1, 0, missing = 0),
                      post_shopping = if_else(interval_ancien_dpe <= 90, 1, 0, missing = 0),
                      diff_indicator = post_shopping - pre_shopping) %>%
             filter(prior_330 < 1,
                    epsilon_330 <= 1) %>%
             select(id, ep_conso_5_usages_m2, desactive, pre_shopping, post_shopping, ancien_dpe_id, ancien_dpe_ep_conso_5_usages_m2, prior_330, epsilon_330)

delta <- 10 # width of the donut
df2_donut <- df2 %>%
  filter(abs(ep_conso_5_usages_m2 - 330) > delta)

df2_donut_filtered <- df2_donut %>%
  filter(ep_conso_5_usages_m2 >= 250,
         ep_conso_5_usages_m2 < 420)

# 1. Primary RD estimation (local linear, triangular kernel) ----

out_rd <- rdrobust(
  y = df2_donut$post_shopping,
  x = df2_donut$ep_conso_5_usages_m2,
  c = 330, p = 1, kernel = "triangular"
)
summary(out_rd)  # Displays the RD estimate and robust confidence interval

out_rd_plot <- rdplot(
  y = df2_filtered_donut$post_shopping, 
  x = round(df2_filtered_donut$ep_conso_5_usages_m2), 
  c = 330.001, p = 1, kernel = "triangular", 
  title = "Post-shopping RD estimate (330)",
  x.label = "Energy consumption (kWh/m²)",
  y.label = "Post-shopping"
)
ggsave("graphs/RD-post-shopping-estimate-donut.png", width = 8, height = 6)


# 2. Covariate balance check ('prior_330') ----

out_prior <- rdrobust(
  y = df_donut$prior_330, 
  x = df_donut$ep_conso_5_usages_m2, 
  c = 330, p = 1, kernel = "triangular"
)
summary(out_prior)

out_prior_plot <- rdplot(
  y = df2$prior_330, 
  x = round(df2$ep_conso_5_usages_m2), 
  c = 330, p = 1, kernel = "triangular", 
  h = 75,
  x.lim = c(250, 420),
  y.lim = c(0.6, 0.8),
  title = "Covariate balance check (prior_330)",
  x.label = "Energy consumption (kWh/m²)",
  y.label = "Prior belief (330)"
)

ggsave("graphs/covariate-balance-prior-330-donut.png", width = 8, height = 6)


# 3. Covariate balance check ('epsilon_330') ----

out_imprecision <- rdrobust(
  y = df_donut$epsilon_330, 
  x = df_donut$ep_conso_5_usages_m2, 
  c = 330, p = 1, kernel = "triangular"
)

summary(out_imprecision)

out_imprecision_plot <- rdplot(
  y = df2$epsilon_330, 
  x = round(df2$ep_conso_5_usages_m2), 
  c = 330, p = 1, kernel = "triangular", 
  x.lim = c(250, 420),
  y.lim = c(0.1, 0.2),
  title = "Covariate balance check (epsilon_330)",
  x.label = "Energy consumption (kWh/m²)",
  y.label = "Imprecision (ε) (330)"
)

ggsave("graphs/covariate-balance-epsilon-330-donut.png", width = 8, height = 6)


# 4. Difference in RD estimation (local linear, triangular kernel) ----

out_diff <- rdrobust(
  y      = df2$diff_indicator,
  x      = df2$ep_conso_5_usages_m2,
  c      = 330.001,
  p      = 1,               # local linear
  kernel = "triangular"
)
summary(out_diff)

# Graph of the difference-in-discontinuities (post-shopping − pre-shopping)
rdplot(
  y       = df2$diff_indicator,
  x       = round(df2$ep_conso_5_usages_m2),
  c       = 330.001,        # tiny offset avoids tying exactly at 330
  p       = 1,
  h = 48.643,
  x.lim = c(280, 380),
  y.lim = c(-0.04, 0.04),
  kernel  = "triangular",
  title   = "Diff-in-discontinuities at 330 kWh/m²",
  x.label = "Energy consumption (kWh/m²)",
  y.label = "Post-shopping − Pre-shopping"
)

ggsave("graphs/RD-diff-shopping-estimate.png", width = 8, height = 6)


# 5. Difference in RD estimation (local linear, triangular kernel) ----

out_diff_donut <- rdrobust(
  y      = df2_donut_filtered$diff_indicator,
  x      = df2_donut_filtered$ep_conso_5_usages_m2,
  c      = 330.001,
  p      = 1,               # local linear
  kernel = "triangular"
)
summary(out_diff_donut)

# Graph of the difference-in-discontinuities (post-shopping − pre-shopping)
rdplot(
  y       = df2_donut$diff_indicator,
  x       = round(df2_donut$ep_conso_5_usages_m2),
  c       = 330.001,        # tiny offset avoids tying exactly at 330
  p       = 1,
  h =        ,
  x.lim =    ,
  y.lim =    ,
  kernel  = "triangular",
  title   = "Diff-in-discontinuities at 330 kWh/m²",
  x.label = "Energy consumption (kWh/m²)",
  y.label = "Post-shopping − Pre-shopping"
)

ggsave("graphs/RD-diff-shopping-estimate-donut.png", width = 8, height = 6)
