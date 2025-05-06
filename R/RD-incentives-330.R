# Load packages
library(rdrobust)
library(rddensity)
library(tidyverse)
library(ggplot2)
library(dplyr)

df <- read_csv("C:\\Users\\tdechelotte\\Desktop\\alldpe_group_metrics_scott.csv")

df$type_logement <- factor(df$type_logement)
df$periode_construction <- factor(df$periode_construction)
df$type_energie_chauffage <- factor(df$type_energie_chauffage)

# Create the energy_efficient variable (1 if ep_conso_5_usages_m2 <= 330, otherwise 0)
df <- df %>% mutate(pre_shopping = if_else(interval_dpe_remplacant <= 90, 1, 0, missing = 0)) %>%
             filter(prior_330 < 1,
                    epsilon_330 <= 1) %>%
             select(ep_conso_5_usages_m2, pre_shopping, prior_330, epsilon_330)

# 1. Primary RD estimation (local linear, triangular kernel) ----

out_rd <- rdrobust(
  y = df$pre_shopping,
  x = df$ep_conso_5_usages_m2,
  c = 330, p = 1, kernel = "triangular"
)
summary(out_rd)  # Displays the RD estimate and robust confidence interval

out_rd_plot <- rdplot(
  y = df$pre_shopping, 
  x = round(df$ep_conso_5_usages_m2), 
  c = 330.001, p = 1, kernel = "triangular", 
  title = "Shopping RD estimate (330)",
  x.label = "Energy consumption (kWh/m²)",
  y.label = "Shopping"
)

ggsave("graphs/RD-shopping-estimate.png", width = 8, height = 6)


# 2. Manipulation test (McCrary) ----

dens <- rddensity(X = df$ep_conso_5_usages_m2, c = 330)
summary(dens)
rdplotdensity(dens, df$ep_conso_5_usages_m2)


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

out_rd_plot <- rdplot(
  y = df_donut$pre_shopping, 
  x = round(df_donut$ep_conso_5_usages_m2), 
  c = 330.001, p = 1, kernel = "triangular", 
  title = "Shopping RD estimate (330)",
  x.label = "Energy consumption (kWh/m²)",
  y.label = "Shopping"
)

ggsave("graphs/RD-shopping-estimate-donut.png", width = 8, height = 6)


# 3.c)  Sensitivity: repeat for multiple donut widths
for (delta_alt in c(3, 5, 8, 10)) {
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
  x = round(df_donut$ep_conso_5_usages_m2), 
  c = 330, p = 1, kernel = "triangular", 
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
  x = round(df_donut$ep_conso_5_usages_m2), 
  c = 330.001, p = 1, kernel = "triangular", 
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
  c = 330.001, p = 1, kernel = "triangular", 
  covs = cbind(df_donut$prior_330, df_donut$epsilon_330),
  title = "Shopping RD estimate with covariates (330)",
  x.label = "Energy consumption (kWh/m²)",
  y.label = "Shopping"
)

ggsave("graphs/RD-shopping-estimate-cov-donut.png", width = 8, height = 6)


# 7. RD estimation across prior (π) heterogeneity ----

K <- 20  # number of equal‑frequency bins; adjust as needed
q <- quantile(df$prior_330, probs = seq(0, 1, length.out = K + 1), na.rm = TRUE)

rd_bin <- map_dfr(1:K, function(k) {
  # rows whose π̂ falls into bin k
  in_bin <- df$prior_330 >= q[k] & df$prior_330 < q[k + 1] & !is.na(df$prior_330)
  if (sum(in_bin) < 200) return(NULL)  # skip tiny bins
  fit <- rdrobust(df$pre_shopping[in_bin], df$ep_conso_5_usages_m2[in_bin], c = 330.001, p = 1, kernel = "triangular")
  tibble(bin          = k,
         prior_mean   = mean(df$prior_330[in_bin], na.rm = TRUE),
         tau_hat      = fit$Estimate[1],
         se_hat       = fit$se[1])
})

# visualise τ̂(π̂)
ggplot(rd_bin,
       aes(x = prior_mean, y = tau_hat)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = tau_hat - 1.96*se_hat,
                    ymax = tau_hat + 1.96*se_hat),
                width = 0) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Mean prior belief in bin (π)",
       y = "RD estimate of shopping jump (τ)",
       title = "Heterogeneous RD effect across prior bins") +
  theme_bw()
ggsave("graphs/heterogeneous_RD_prior.png", width = 8, height = 6)


# 8. RD estimation across imprecision (ε) heterogeneity ----

K_eps <- 20  # number of equal‑frequency bins; adjust as needed
q_eps <- quantile(df$epsilon_330, probs = seq(0, 1, length.out = K_eps + 1), 
                  na.rm = TRUE)

rd_bin_eps <- map_dfr(1:K_eps, function(k) {
  # observations whose ε̂ falls into bin k
  in_bin <- df$epsilon_330 >= q_eps[k] & df$epsilon_330 < q_eps[k + 1] & 
            !is.na(df$epsilon_330)
  if (sum(in_bin) < 200) return(NULL)  # skip bins that are too small
  fit <- rdrobust(df$pre_shopping[in_bin],
                  df$ep_conso_5_usages_m2[in_bin],
                  c = 330.001, p = 1, kernel = "triangular")
  tibble(bin          = k,
         epsilon_mean = mean(df$epsilon_330[in_bin], na.rm = TRUE),
         tau_hat      = fit$Estimate[1],
         se_hat       = fit$se[1])
})

# visualise τ̂(ε̂)
ggplot(rd_bin_eps,
       aes(x = epsilon_mean, y = tau_hat)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = tau_hat - 1.96 * se_hat,
                    ymax = tau_hat + 1.96 * se_hat),
                width = 0) +
  xlim(NaN, 0.4) +
  ylim(0, NaN) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Mean imprecision in bin (ε)",
       y = "RD estimate of shopping jump (τ)",
       title = "Heterogeneous RD effect across imprecision bins") +
  theme_bw()
ggsave("graphs/heterogeneous_RD_imprecision.png", width = 8, height = 6)

