# Load packages
library(rdrobust)
library(rddensity)
library(tidyverse)
library(ggplot2)
library(dplyr)

df <- read_csv("C:\\Users\\tdechelotte\\Desktop\\alldpe_group_metrics_scott_new.csv")

df$type_logement <- factor(df$type_logement)
df$periode_construction <- factor(df$periode_construction)
df$type_energie_chauffage <- factor(df$type_energie_chauffage)

# Create the energy_efficient variable (1 if ep_conso_5_usages_m2 <= 330, otherwise 0)
df <- df %>% mutate(pre_shopping = if_else(interval_dpe_remplacant <= 90, 1, 0, missing = 0)) %>%
             filter(prior_330 < 1,
                    epsilon_330 <= 1,
                    ep_conso_5_usages_m2 <= 750) %>%
             select(type_logement, periode_construction, type_energie_chauffage, ep_conso_5_usages_m2, pre_shopping, prior_330, epsilon_330)

# 1. Primary RD estimation (local linear, triangular kernel) ----

out_rd <- rdrobust(
  y = df$pre_shopping,
  x = df$ep_conso_5_usages_m2,
  c = 330, p = 1, kernel = "triangular"
)
summary(out_rd)  # Displays the RD estimate and robust confidence interval

out_rd_plot <- rdplot(
  y       = df$pre_shopping,
  x       = df$ep_conso_5_usages_m2,
  c       = 330, p = 1, kernel = "triangular",
  nbins   = 300, ci = 0.95,
  h = 26.068,
  x.lim   = c(300, 360), y.lim = c(0.07, 0.13),
  title   = "Shopping RD estimate (330)",
  x.label = "Energy consumption (kWh/m²)",
  y.label = "Shopping"
)

ggsave("graphs/RD-shopping-estimate.png", width = 8, height = 6)


# 2. Manipulation test (McCrary) ----

dens <- rddensity(X = df$ep_conso_5_usages_m2, c = 330)
summary(dens)
rdplotdensity(
  dens,
  df$ep_conso_5_usages_m2,
  title  = "McCrary density test",
  xlab  = "Energy consumption (kWh/m²)",
  ylab  = "Density of observations"
)

ggsave("graphs/running-variable-continuity.png", width = 8, height = 6)


# 3. Donut‑hole RD (trim ±delta kWh/m² around the cutoff) ---- 

delta <- 10 # width of the donut
df_donut <- df %>%
  filter(abs(ep_conso_5_usages_m2 - 330) > delta)

# 3.a) Re‑check density continuity on trimmed sample
dens_trim <- rddensity(X = df_donut$ep_conso_5_usages_m2, c = 330)
summary(dens_trim)

rdplotdensity(dens_trim, 
                df_donut$ep_conso_5_usages_m2, 
                CItype = "none", 
                title = "Density test on trimmed sample",
                xlab = "Energy consumption (kWh/m²)",
                ylab = "Density")

# 3.b) RD estimate using donut sample
out_rd_donut <- rdrobust(
  y = df_donut$pre_shopping,
  x = df_donut$ep_conso_5_usages_m2,
  c = 330, p = 1, kernel = "triangular"
)
summary(out_rd_donut)

out_rd_donut_plot <- rdplot(
  y = df_donut$pre_shopping, 
  x = df_donut$ep_conso_5_usages_m2, 
  c = 330, p = 1, kernel = "triangular", 
  nbins = 300, ci = 0.95,
  h = 62.263,
  x.lim = c(265, 395),
  y.lim = c(0.075, 0.125),
  title = "Shopping RD estimate (330)",
  x.label = "Energy consumption (kWh/m²)",
  y.label = "Shopping"
)

ggsave("graphs/RD-shopping-estimate-donut.png", width = 8, height = 6)


# 3.c)  Sensitivity: repeat for multiple donut widths
for (delta_alt in c(5, 8, 10)) {
  df_tmp <- df %>%
    filter(abs(ep_conso_5_usages_m2 - 330) > delta_alt)
  out_tmp <- rdrobust(
    y = df_tmp$pre_shopping,
    x = df_tmp$ep_conso_5_usages_m2,
    c = 330, p = 1, kernel = "triangular"
  )
  cat("\n--- Donut width =", delta_alt, "kWh/m² ---\n")
  print(summary(out_tmp))
}


# 4. Covariate balance check ('prior_330') ----

out_prior <- rdrobust(
  y = df_donut$prior_330, 
  x = df_donut$ep_conso_5_usages_m2, 
  c = 330, p = 1, kernel = "triangular"
)
summary(out_prior)

out_prior_plot <- rdplot(
  y = df_donut$prior_330, 
  x = df_donut$ep_conso_5_usages_m2, 
  c = 330, p = 1, kernel = "triangular", 
  nbins = 150, ci = 0.95,
  h = 101.949,
  x.lim = c(230, 430),
  y.lim = c(0.6, 0.8),
  title = "Covariate balance check (prior_330)",
  x.label = "Energy consumption (kWh/m²)",
  y.label = "Prior belief (330)"
)
ggsave("graphs/covariate-balance-prior-330-donut.png", width = 8, height = 6)


# 5. Covariate balance check ('epsilon_330') ----

out_imprecision <- rdrobust(
  y = df_donut$epsilon_330, 
  x = df_donut$ep_conso_5_usages_m2, 
  c = 330, p = 1, kernel = "triangular"
)

summary(out_imprecision)

out_imprecision_plot <- rdplot(
  y = df_donut$epsilon_330, 
  x = df_donut$ep_conso_5_usages_m2, 
  c = 330, p = 1, kernel = "triangular", 
  nbins = 150, ci = 0.95,
  h = 34.902,
  x.lim = c(290, 370),
  y.lim = c(0.15, 0.25),
  title = "Covariate balance check (epsilon_330)",
  x.label = "Energy consumption (kWh/m²)",
  y.label = "Imprecision (ε) (330)"
)
ggsave("graphs/covariate-balance-epsilon-330-donut.png", width = 8, height = 6)


# 6. Primary RD estimation with covariates (local linear, triangular kernel) ----

out_rd_cov <- rdrobust(
  y = df_donut$pre_shopping,
  x = df_donut$ep_conso_5_usages_m2,
  covs = cbind(df_donut$prior_330, df_donut$epsilon_330),
  c = 330, p = 1, kernel = "triangular"
)
summary(out_rd_cov)  # Displays the RD estimate and robust confidence interval

out_rd_cov_plot <- rdplot(
  y = df_donut$pre_shopping, 
  x = round(df_donut$ep_conso_5_usages_m2), 
  c = 330, p = 1, kernel = "triangular", 
  covs = cbind(df_donut$prior_330, df_donut$epsilon_330),
  h = 59.975,
  x.lim = c(270, 390),
  y.lim = c(0.07, 0.13),
  title = "Shopping RD estimate with covariates (330)",
  x.label = "Energy consumption (kWh/m²)",
  y.label = "Shopping"
)

ggsave("graphs/RD-shopping-estimate-cov-donut.png", width = 8, height = 6)

# 7. RD estimation across prior (π) heterogeneity ----

# 1. nest by unique prior_330
rd_by_prior <- df_donut %>%
  group_by(prior_330) %>%
  filter(
    sum(ep_conso_5_usages_m2 <  330) >= 50,
    sum(ep_conso_5_usages_m2 >= 330) >= 50
  ) %>%
  nest() %>%                     # one row per prior_330, with a data.frame in "data"
  mutate(
    fit = map(data,
      ~ rdrobust(
          y   = .x$pre_shopping,
          x   = .x$ep_conso_5_usages_m2,
          c   = 330,
          p   = 1,
          kernel = "triangular"
        )
    )
  ) %>%
  mutate(
    tau = map_dbl(fit, ~ .x$Estimate[1]),
    se  = map_dbl(fit, ~ .x$se[1])
  ) %>%
  select(prior_330, tau, se) %>%
  ungroup()

# 5. plot one point per prior_330
ggplot(rd_by_prior, aes(x = prior_330, y = tau)) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(
    ymin = tau - 1.96 * se,
    ymax = tau + 1.96 * se
  ), width = 0) +
  geom_smooth(method = "glm", se = TRUE) +
  ylim(NaN, 1) +
  labs(
    x     = "Prior belief (π)",
    y     = "RD jump estimate τ(π)",
    title = "Heterogeneous RD effect at each distinct π"
  ) +
  theme_bw()

ggsave("graphs/heterogeneous_RD_prior.png", width = 8, height = 6)

ols_unw <- lm(tau ~ prior_330, data = rd_by_prior)
summary(ols_unw)

rd_by_prior_clean <- rd_by_prior %>%
  filter(se > 0)

ols_w   <- lm(tau ~ prior_330,
              data    = rd_by_prior_clean,
              weights = 1 / (se^2))

summary(ols_w)


# 1. Base ggplot call on your df
p <- ggplot(rd_by_prior, aes(x = prior_330, y = tau)) +

  # 2. One geom_point mapping size to inverse variance
  geom_point(aes(size = 1/(se^2 + eps_small))) +

  # 3. A weighted linear fit via the weight aesthetic in geom_smooth
  geom_smooth(
    method = "lm",
    aes(weight = 1/(se^2 + eps_small)),
    se = TRUE
  ) +

  # 4. Clean up
  labs(
    x = "Prior belief (π)",
    y = "RD jump estimate τ(π)",
    title = "Heterogeneous RD effect by prior belief",
    size = "Precision\nweight"
  ) +
  theme_bw()

# 5. Print it
print(p)

# 6. Save it
ggsave("graphs/heterogeneous_RD_prior_weighted.png", width = 8, height = 6)


# 8. RD estimation across imprecision (ε) heterogeneity ----

# 1. nest by unique epsilon_330
rd_by_imprecision <- df_donut %>%
  # only keep prior‐values with at least, say, 50 obs on each side of the 330 cutoff
  group_by(epsilon_330) %>%
  filter(
    sum(ep_conso_5_usages_m2 <  330) >= 50,
    sum(ep_conso_5_usages_m2 >= 330) >= 50
  ) %>%
  nest() %>%                     # one row per epsilon_330, with a data.frame in "data"
  
  # 2. run rdrobust on each
  mutate(
    fit = map(data,
      ~ rdrobust(
          y   = .x$pre_shopping,
          x   = .x$ep_conso_5_usages_m2,
          c   = 330,
          p   = 1,
          kernel = "triangular"
        )
    )
  ) %>%
  
  # 3. extract the point‐estimate and se
  mutate(
    tau = map_dbl(fit, ~ .x$Estimate[1]),
    se  = map_dbl(fit, ~ .x$se[1])
  ) %>%
  
  # 4. drop the list‐columns and ungroup
  select(epsilon_330, tau, se) %>%
  ungroup()

# 5. plot one point per epsilon_330
ggplot(rd_by_imprecision, aes(x = epsilon_330, y = tau)) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(
    ymin = tau - 1.96 * se,
    ymax = tau + 1.96 * se
  ), width = 0) +
  geom_smooth(method = "glm", se = TRUE) +
  ylim(NaN, 1) +
  labs(
    x     = "Imprecision belief (ε)",
    y     = "RD jump estimate τ(ε)",
    title = "Heterogeneous RD effect at each distinct ε"
  ) +
  theme_bw()

ggsave("graphs/heterogeneous_RD_imprecision.png", width = 8, height = 6)

# 1. Unweighted OLS
ols_unw_eps <- lm(tau ~ epsilon_330, data = rd_by_imprecision)
summary(ols_unw_eps)

# 2. Precision‐weighted OLS (add small eps to avoid Inf)
rd_by_imprecision_clean <- rd_by_imprecision %>%
  filter(se > 0)

ols_w_eps   <- lm(tau ~ epsilon_330,
                  data    = rd_by_imprecision_clean,
                  weights = 1 / (se^2))
summary(ols_w_eps)

# 1. Base ggplot call on your df
p <- ggplot(rd_by_imprecision, aes(x = epsilon_330, y = tau)) +

  # 2. One geom_point mapping size to inverse variance
  geom_point(aes(size = 1/(se^2 + eps_small))) +

  # 3. A weighted linear fit via the weight aesthetic in geom_smooth
  geom_smooth(
    method = "lm",
    aes(weight = 1/(se^2 + eps_small)),
    se = TRUE
  ) +

  # 4. Clean up
  labs(
    x = "Imprecision belief (ε)",
    y = "RD jump estimate τ(ε)",
    title = "Heterogeneous RD effect by imprecision",
    size = "Precision\nweight"
  ) +
  theme_bw()

# 5. Print it
print(p)

# 6. Save it
ggsave("graphs/heterogeneous_RD_imprecision_weighted.png", width = 8, height = 6)
