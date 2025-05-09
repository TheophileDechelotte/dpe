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

df_shopping30 <- df %>%
  filter(is_dpe_remplacant,
         !is_ancien_dpe,
         interval_ancien_dpe <= 30)

df_certif <- df %>%
  filter(!is_dpe_remplacant)

df_last_certif <- df %>%
  filter(!desactive)

certif_simulation <- read_csv("/Users/theophiledechelotte/Library/CloudStorage/OneDrive-Personnel/dpe-data/simulation_scott.csv")
certif_simulation <- certif_simulation %>% filter(total <= 800)

post_shopping_simulation <- read_csv("/Users/theophiledechelotte/Library/CloudStorage/OneDrive-Personnel/dpe-data/simulation_scott_post_shopping.csv")
post_shopping_simulation <- post_shopping_simulation %>% filter(total <= 800)

post_shopping_simulation30 <- read_csv("/Users/theophiledechelotte/Library/CloudStorage/OneDrive-Personnel/dpe-data/simulation_scott_post_shopping30.csv")
post_shopping_simulation30 <- post_shopping_simulation30 %>% filter(total <= 800)

last_certif_simulation <- read_csv("/Users/theophiledechelotte/Library/CloudStorage/OneDrive-Personnel/dpe-data/simulation_scott_last_certif.csv")
last_certif_simulation <- last_certif_simulation %>% filter(total <= 800)

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

bin_width <- diff(breaks)[1]

# 1. Build histograms on the same breaks
h_first_certif   <- get_hist(df_certif$ep_conso_5_usages_m2)
h_post_shopping  <- get_hist(df_shopping$ep_conso_5_usages_m2)
h_last_certif    <- get_hist(df_last_certif$ep_conso_5_usages_m2)
h_post_shopping30 <- get_hist(df_shopping30$ep_conso_5_usages_m2)

h_first_certif_baseline   <- get_hist(certif_simulation$total)
h_post_shopping_baseline  <- get_hist(post_shopping_simulation$total)
h_last_certif_baseline    <- get_hist(last_certif_simulation$total)
h_post_shopping30_baseline <- get_hist(post_shopping_simulation30$total)

df_obs <- df_last_certif
h_obs <- h_last_certif

h_baseline <- h_last_certif_baseline 

# 1) Build 'observed – baseline' density gap once ----------------
build_cmp <- function(h_obs, h_baseline) {
  h_obs  %>% rename(d_obs  = density) %>%           # observed density
  left_join(h_baseline %>% rename(d_base = density),
            by = "x") %>%                          # baseline density
  replace_na(list(d_base = 0)) %>%                 # pad missing bins
  mutate(density_diff = d_obs - d_base)
}

# 2) Compute manipulation stats at one class boundary -----------
one_boundary <- function(df_cmp, df_obs,
                         prev_thr, thr, next_thr,
                         bin_width) {

  # “extra” mass *just before* the boundary (prev_thr ≤ x < thr)
  excess_before <- df_cmp %>%
    filter(x >= prev_thr, x < thr) %>%
    pull(density_diff) %>% pmax(0) %>%
    sum() * bin_width

  # “extra” mass *just after* the boundary (thr ≤ x < next_thr)
  excess_after <- df_cmp %>%
    filter(x >= thr, x < next_thr) %>%
    pull(density_diff) %>% pmax(0) %>%
    sum() * bin_width
  
  excess <- df_cmp %>%
    pull(density_diff) %>% pmax(0) %>%
    sum() * bin_width

  # total *observed* mass in the class right after the boundary
  total_after  <- df_cmp %>%
    filter(x >= thr, x < next_thr) %>%
    pull(d_obs) %>% sum() * bin_width

  # % of certificates shifted upward across the boundary
  pct_shift <- 100 * excess_before /
               (excess_before + total_after - excess_after)
    
  tibble(
    boundary         = glue::glue("{prev_thr}/{thr}"),
    pct_shift,
    nrow(df_obs),
    net_shift_count  = excess_before * nrow(df_obs),
    excess * 100,
    net_excess = excess * nrow(df_obs)
  )
}

# 3) Main one-liner you’ll call ----
manipulation_stats <- function(df_obs, h_obs, h_baseline,
                               # default to D/E, E/F, F/G
                               boundaries = list(
                                 `D/E` = c(180, 250, 330),
                                 `E/F` = c(250, 330, 420),
                                 `F/G` = c(330, 420, 800)
                               )) {

  # reusable pieces
  df_cmp    <- build_cmp(h_obs, h_baseline)
  bin_width <- diff(unique(h_obs$x))[1]

  purrr::imap_dfr(boundaries, function(thr_vec, name) {
    one_boundary(df_cmp, df_obs,
                 prev_thr = thr_vec[1],
                 thr      = thr_vec[2],
                 next_thr = thr_vec[3],
                 bin_width) %>%
      mutate(boundary = name, .before = 1)
  })
}


manipulation_stats(df_obs, h_obs, h_baseline)



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

# 5. Excess mass just below E/F (250 ≤ x < 330)
excess_mass <- df_cmp %>%
  filter(x >=  prev_threshold, x < threshold) %>%
  pull(density_diff) %>% 
  pmax(0) %>% 
  sum() * bin_width

excess_mass_after <- df_cmp %>%
  filter(x >= threshold, x < next_threshold) %>%
  pull(density_diff) %>% 
  pmax(0) %>% 
  sum() * bin_width 

# 6. Total observed mass in the F class (330 ≤ x < 420)
total_mass <- df_cmp %>%
  filter(x >=  threshold, x < next_threshold) %>%
  pull(d_obs) %>%
  sum() * bin_width

# 7. % surclassés E→F
pct_surclasse <- 100 * excess_mass / (excess_mass + total_mass - excess_mass_after)

# 8. Report
cat(sprintf("Threshold = %d kWh/m²\n", threshold))
cat(sprintf("Excess mass just below threshold:  %.4f (%.2f%% of total certificates)\n",
            excess_mass, excess_mass * 100)) 
cat(sprintf("Total observed mass previous class:    %.4f (%.2f%% of total certificates)\n",
            total_mass, total_mass * 100))
cat(sprintf("Estimated %% surclassés at threshold: %.2f%%\n", pct_surclasse))

# 8b. Compute net count by pct × N_obs in class ---

# 1. Count of observed certificates in [prev_threshold, threshold)
n_prev <- sum(
  df_obs$ep_conso_5_usages_m2 >= prev_threshold &
  df_obs$ep_conso_5_usages_m2 <  threshold,
  na.rm = TRUE
)

# 2. Net manipulated = pct_surclasse (%) × n_prev
#    (pct_surclasse is on the 0–100 scale)
net_excess_by_pct <- pct_surclasse / 100 * n_prev

# 3. Report alongside your density‐based counts
cat(sprintf("N certificates in [%d, %d[:   %d\n",
            prev_threshold, threshold, n_prev))
cat(sprintf("Net manipulated (by pct × N): %.0f\n",
            net_excess_by_pct))







# 1. Compute “adjusted” density differences for each series
certif_diff <- h_first_certif_baseline %>%
  rename(d_base = density) %>%
  left_join(h_first_certif %>% rename(d_first = density), by = "x") %>%
  replace_na(list(d_first = 0, d_base = 0)) %>%
  transmute(
    x,
    diff_first = d_first - d_base
  )

shopping_diff <- h_post_shopping_baseline %>%
  rename(d_base = density) %>%
  left_join(h_post_shopping %>% rename(d_post = density), by = "x") %>%
  replace_na(list(d_post = 0, d_base = 0)) %>%
  transmute(
    x,
    diff_post = d_post - d_base
  )

# 2. Join on x only, then compute final difference
shopping_certif_diff <- full_join(certif_diff, shopping_diff, by = "x") %>%
  replace_na(list(diff_first = 0, diff_post = 0)) %>%
  transmute(
    x,
    density_diff = diff_post - diff_first
  )

# 3. Plot
ggplot(shopping_certif_diff, aes(x = x, y = density_diff)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_col(width = 4, fill = "blue", alpha = 0.7) +
  geom_vline(xintercept = dpe_thresholds, linetype = "dotted", color = "black", linewidth = 0.2) +
  coord_cartesian(xlim = c(0, 600)) +
  labs(
    x     = "Energy consumption (kWh/m²)",
    y     = "Density difference (Post shopping – 1st certification)",
    title = "Post-shopping vs 1st-certification manipulation shift"
  ) +
  theme_bw()

ggsave("graphs/deformation_post_vs_first.png", width = 8, height = 6)

