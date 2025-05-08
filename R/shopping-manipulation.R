# Charger les bibliothèques nécessaires
library(tidyverse)
library(scales)


# Lire les données
df <- read_csv("/Users/theophiledechelotte/Library/CloudStorage/OneDrive-Personnel/dpe-data/alldpe_v2.csv")

df <- df %>%
  mutate(
    is_ancien_dpe = !is.na(dpe_remplacant_id),
    is_dpe_remplacant = !is.na(ancien_dpe_id))

df_shopping <- df %>%
  filter(is_dpe_remplacant,
         !is_ancien_dpe,
         interval_ancien_dpe <= 90)

df_certif <- df %>%
  filter(!is_dpe_remplacant)

df_last_certif <- df %>%
  filter(!desactive)

simulation_results <- read_csv("/Users/theophiledechelotte/Library/CloudStorage/OneDrive-Personnel/dpe-data/simulation_scott.csv")
simulation_results <- simulation_results %>% filter(total <= 800)

breaks <- seq(0, 800, by = 1)
dpe_thresholds <- c(70, 110, 180, 250, 330, 420)

# helper to compute mids and densities
get_hist <- function(x){
  h <- hist(x, breaks = breaks, plot = FALSE, freq = FALSE)
  tibble(
    x = h$mids,
    density = h$density
  )
}

# compute per‐group histograms
h_baseline       <- get_hist(simulation_results$total)
h_first_certif   <- get_hist(df_certif$ep_conso_5_usages_m2)
h_post_shopping  <- get_hist(df_shopping$ep_conso_5_usages_m2)

# join and compute differences
df_diff <- h_baseline %>%
  rename(d_baseline = density) %>%
  left_join(h_first_certif   %>% rename(d_first   = density), by = "x") %>%
  left_join(h_post_shopping  %>% rename(d_post    = density), by = "x") %>%
  replace_na(list(d_first = 0, d_post = 0)) %>%
  transmute(
    x,
    `Post shopping – Baseline`    = d_post  - d_baseline,
    `1st certification – Baseline` = d_first - d_baseline
  ) %>%
  pivot_longer(-x, names_to = "comparison", values_to = "density_diff")


# now plot
ggplot(df_diff, aes(x = x, y = density_diff, fill = comparison)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_col(width = 4, alpha = 0.7, position = "identity") +
  geom_vline(xintercept = dpe_thresholds, linetype = "dotted", color = "black", linewidth = 0.2) +
  scale_fill_manual(values = c(
    "1st certification – Baseline" = "orange",
    "Post shopping – Baseline"     = "red"
  )) +
  labs(
    x     = "Energy consumption (kWh/m²)",
    y     = "Density difference",
    fill = NULL
  ) +
  theme_bw()

ggsave("graphs/deformation_hist.png", width = 8, height = 6)


h_post_vs_first <- h_first_certif %>% 
  rename(d_first = density) %>% 
  left_join(h_post_shopping  %>% rename(d_post  = density), by = "x") %>% 
  replace_na(list(d_post = 0)) %>% 
  transmute(
    x,
    density_diff = d_post - d_first
  )

ggplot(h_post_vs_first, aes(x = x, y = density_diff)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_col(width = 4, fill = "blue", alpha = 0.7) +
  geom_vline(xintercept = dpe_thresholds, linetype = "dotted", color = "black", linewidth = 0.2) +
  xlim(0, 600) +
  labs(
    x     = "Energy consumption (kWh/m²)",
    y     = "Density difference (Post shopping – 1st certification)",
    title = "Post-shopping vs 1st-certification density shift"
  ) +
  theme_bw()

ggsave("graphs/deformation_post_vs_first.png", width = 8, height = 6)


# Compute % surclassés E→F with simulation_results as baseline ----

# 1. Build histograms on the same breaks
h_obs      <- get_hist(df_shopping$ep_conso_5_usages_m2)      # observed
h_baseline <- get_hist(df_certif$ep_conso_5_usages_m2) 
#h_baseline <- get_hist(simulation_results$total)              # theoretical

# 2. Compute density difference (observed – baseline)
df_cmp <- h_obs %>%
  rename(d_obs = density) %>%
  left_join(h_baseline %>% rename(d_base = density), by = "x") %>%
  replace_na(list(d_base = 0)) %>%
  transmute(
    x,
    density_diff = d_obs - d_base,
    d_obs,
    d_base
  )

# 3. Bin width
bin_width <- diff(breaks)[1]

# 4. Define thresholds
ef_threshold <- dpe_thresholds[5]  # 330
de_threshold <- dpe_thresholds[4]  # 250
fg_threshold <- dpe_thresholds[6]  # 420
cd_threshold <- dpe_thresholds[3]  # 180
gg_threshold <- 800

threshold <- de_threshold
prev_threshold <- cd_threshold
next_threshold <- ef_threshold

# 5. Excess mass just below E/F (250 ≤ x < 330)
excess_mass <- df_cmp %>%
  filter(x >=  prev_threshold, x < threshold) %>%
  pull(density_diff) %>% 
  pmax(0) %>% 
  sum() * bin_width

# 6. Total observed mass in the F class (330 ≤ x < 420)
total_mass <- df_cmp %>%
  filter(x >=  threshold, x < next_threshold) %>%
  pull(d_obs) %>%
  sum() * bin_width

# 7. % surclassés E→F
pct_surclasse <- excess_mass / total_mass * 100

# 8. Report
cat(sprintf("Threshold = %d kWh/m²\n", threshold))
cat(sprintf("Excess mass just below threshold:  %.4f (%.2f%% of total certificates)\n",
            excess_mass, excess_mass * 100)) 
cat(sprintf("Total observed mass previous class:    %.4f (%.2f%% of total certificates)\n",
            total_mass, total_mass * 100))
cat(sprintf("Estimated %% surclassés at threshold: %.2f%%\n", pct_surclasse))

