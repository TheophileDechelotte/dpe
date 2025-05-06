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


dpe_thresholds <- c(70, 110, 180, 250, 330, 420)

ggplot() +
  geom_vline(xintercept = dpe_thresholds, linetype = "dashed", color = "black", linewidth = 0.2) +
  geom_freqpoly(data = df_certif,
                 aes(x = ep_conso_5_usages_m2, y = after_stat(density), color = "1st certification"),
                 binwidth = 2,
                 linewidth = 0.5) +
  geom_freqpoly(data = df_shopping,
                 aes(x = ep_conso_5_usages_m2, y = after_stat(density), color = "Post shopping"),
                 binwidth = 2,
                 linewidth = 0.5) +
  geom_freqpoly(data = simulation_results, 
                aes(x = total, y= after_stat(density), color = "Baseline"),
                binwidth = 2, 
                linewidth = 0.5) +
  labs(x = "Energy consumption (kWh/m²)",
       y = "Density") +
  scale_color_manual(
      name   = NULL,
      values = c("Baseline" = "#0073ff",
                "1st certification" = "orange",
                "Post shopping" = "red")
    ) +
  xlim(0, 800) +
  theme_bw()

ggsave("graphs/deformations_plot.png", width = 8, height = 6)


