# Charger les bibliothèques nécessaires
library(tidyverse)
library(scales)


# Lire les données
df <- read_csv("C:\\Users\\tdechelotte\\Desktop\\alldpe_v2.csv")

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

simulation_results <- read_csv("C:\\Users\\tdechelotte\\Desktop\\alldpe_simulation_scott.csv")


breaks <- seq(0, 800, by = 1)

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
    `1st certification – Baseline` = d_first - d_baseline,
    `Post shopping – Baseline`    = d_post  - d_baseline
  ) %>%
  pivot_longer(-x, names_to = "comparison", values_to = "density_diff")

# now plot
ggplot(df_diff, aes(x = x, y = density_diff, color = comparison)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_line(linewidth = 0.7) +
  geom_vline(xintercept = dpe_thresholds, linetype = "dotted", color = "black", linewidth = 0.2) +
  scale_color_manual(values = c(
    "1st certification – Baseline" = "orange",
    "Post shopping – Baseline"     = "red"
  )) +
  labs(
    x     = "Energy consumption (kWh/m²)",
    y     = "Density difference",
    color = NULL
  ) +
  theme_bw()

ggsave("graphs/deformations_plot.png", width = 8, height = 6)


