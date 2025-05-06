# Charger les bibliothèques nécessaires
library(tidyverse)
library(scales)


# Lire les données
df <- read_csv("/Users/theophiledechelotte/Library/CloudStorage/OneDrive-UniversitéParisSciencesetLettres/dpe-data/alldpe_v2.csv")

simulation_results_CSV <- read_csv("/Users/theophiledechelotte/Library/CloudStorage/OneDrive-UniversitéParisSciencesetLettres/dpe-data/alldpe_simulation_scott.csv")

dpe_thresholds <- c(70, 110, 180, 250, 330, 420)

df <- df %>%
  mutate(
    is_ancien_dpe = !is.na(dpe_remplacant_id),
    is_dpe_remplacant = !is.na(ancien_dpe_id))

df_filtered <- df %>%
  filter(!is_dpe_remplacant)

ggplot() +
  geom_vline(xintercept = dpe_thresholds, linetype = "dashed", color = "black", linewidth = 0.2) +
  geom_histogram(data = df_filtered,
                 aes(x = ep_conso_5_usages_m2, y = after_stat(density), fill = "Real EPCs"),
                 binwidth = 1,
                 position = "identity") +
  geom_freqpoly(data = simulation_results_CSV, 
                aes(x = total, y= after_stat(density), color = "Virtual EPCs"),
                binwidth = 1, 
                linewidth = 0.5) +
  labs(title = paste0("Simulated EPC distribution (n = ", nrow(df_filtered), ")"),
       x = "Energy consumption (kWh/m²)",
       y = "Density") +
  scale_fill_manual(
      name   = "Distribution",
      values = c("Real EPCs" = "orange")
    ) +
    scale_color_manual(
      name   = "Distribution",
      values = c("Virtual EPCs" = "#0073ff")
    ) +
  xlim(0, 800) +
  theme_bw()

