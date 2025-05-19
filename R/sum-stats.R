library(tidyverse)
library(readr)
library(ggplot2)
library(scales)
library(dplyr)
library(vtable)

df <- read_csv("/Users/theophiledechelotte/Library/CloudStorage/OneDrive-Personnel/dpe-data/alldpe_v2.csv")

print(summary(df))

dpe_thresholds <- c(70, 110, 180, 250, 330, 420)

# Regroupement des types d'énergie
df <- df %>%
  mutate(
    type_energie_chauffage = case_when(
      chauffage_principal %in% c("Bois – Bûches", "Bois – Plaquettes forestières",
                                 "Bois – Plaquettes d’industrie",
                                 "Bois – Granulés (pellets) ou briquettes") ~ "Bois",
      chauffage_principal == "Fioul domestique" ~ "Fioul",
      chauffage_principal %in% c("Gaz naturel", "GPL", "Propane","Butane") ~ "Gaz",
      chauffage_principal %in% c("Réseau de Chauffage urbain", "Réseau de Froid Urbain") ~ "Reseau chaleur",
      chauffage_principal %in% c("Électricité", 
                                 "Électricité d'origine renouvelable utilisée dans le bâtiment") ~ "Electricite",
      chauffage_principal == "Charbon" ~ "Charbon"),
    is_ancien_dpe = !is.na(dpe_remplacant_id),
    is_dpe_remplacant = !is.na(ancien_dpe_id),
    type_logement = as.factor(type_logement),
    periode_construction = as.factor(periode_construction),
    type_energie_chauffage = as.factor(type_energie_chauffage),
    version = as.factor(version)) %>% 
    mutate(pre_shopping = if_else(interval_dpe_remplacant <= 90, 1, 0, missing = 0),
                      post_shopping = if_else(interval_ancien_dpe <= 90, 1, 0, missing = 0))

st(df, fit.page = '\\textwidth', out = 'latex')

ggplot(df, aes(x = interval_dpe_remplacant)) +
  geom_histogram(binwidth = 5) +
  labs(title = "Histogram of interval_dpe_remplacant",
       x = "Time elapsed between an EPC and its next replacement",
       y = "Count (log scale)") +
  scale_y_log10() +
  theme_bw()

ggsave("graphs/interval_dpe_remplacant_histogram.png", width = 8, height = 6)

ggplot(df, aes(x = conso_chauffage)) +
  geom_density(bw = 1) +
  labs(
    title = "Primary energy consumption distribution: heating energy",
    x     = "Heating energy consumption (kWh/m²)",
    y     = "Density"
  ) +
  theme_bw()

ggsave("graphs/conso_chauffage_density.png", width = 8, height = 6)

ggplot(df, aes(x = conso_ecs)) +
  geom_density(bw = 0.1) +
  xlim(0, 300) +
  labs(
    title = "Primary energy consumption distribution: ECS",
    x     = "ECS consumption (kWh/m²)",
    y     = "Density"
  ) +
  theme_bw()

ggsave("graphs/conso_ecs_density.png", width = 8, height = 6)

ggplot(df, aes(x = conso_eclairage)) +
  geom_density(bw = 0.01) +
  xlim(0, 7) +
  labs(
    title = "Primary energy consumption distribution: lighting energy",
    x     = "Lighting energy consumption (kWh/m²)",
    y     = "Density"
  ) +
  theme_bw()

ggsave("graphs/conso_eclairage_density.png", width = 8, height = 6)

ggplot(df, aes(x = conso_froid)) +
  geom_density(bw = 0.05) +
  xlim(0.000001, 50) +
  labs(
    title = "Primary energy consumption distribution: cooling energy",
    x     = "Cooling energy consumption (kWh/m²)",
    y     = "Density"
  ) +
  theme_bw()

ggsave("graphs/conso_froid_density.png", width = 8, height = 6)

ggplot(df, aes(x = conso_aux)) +
  geom_density(bw = 0.01) +
  xlim(0, 40) +
  labs(
    title = "Primary energy consumption distribution: auxiliary energy",
    x     = "Auxiliary energy consumption (kWh/m²)",
    y     = "Density"
  ) +
  theme_bw()

ggsave("graphs/conso_aux_density.png", width = 8, height = 6)

# Density of auxiliary energy consumption by heating energy type
ggplot(df, aes(x = conso_aux, color = type_energie_chauffage)) +
  geom_density(bw = 0.05) +
  xlim(0.00001, 40) +
  labs(
    title = "Primary energy consumption distribution: auxiliary energy by heating energy type",
    x     = "Auxiliary energy consumption (kWh/m²)",
    y     = "Density",
    color = "Heating energy type"
  ) +
  theme_bw()

# Save the plot
ggsave("graphs/conso_aux_density_by_type_energie_chauffage.png", width = 8, height = 6)


# Density of primary energy consumption by heating energy type
ggplot(df, aes(x = ep_conso_5_usages_m2)) +
  geom_vline(xintercept = dpe_thresholds, linetype = "dashed", color = "black", linewidth = 0.5) +
  geom_density(bw = 2, linewidth = 1) +
  labs(
    title = "Primary energy consumption distribution by heating energy type",
    x     = "Primary energy consumption (kWh/m²)",
    y     = "Density",
  ) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),  # supprime les grilles majeures
    panel.grid.minor = element_blank()   # supprime les grilles mineures
  ) + 
  facet_wrap(~ type_energie_chauffage)


ggsave("graphs/ep_conso_5_usages_m2_density_by_type_energie_chauffage.png", width = 8, height = 6)

# Density of primary energy consumption by construction period
ggplot(df, aes(x = ep_conso_5_usages_m2)) +
  geom_vline(xintercept = dpe_thresholds, linetype = "dashed", color = "black", linewidth = 0.5) +
  geom_density(bw = 2, linewidth = 1) +
  labs(
    title = "Primary energy consumption distribution by construction period",
    x     = "Primary energy consumption (kWh/m²)",
    y     = "Density",
  ) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),  # supprime les grilles majeures
    panel.grid.minor = element_blank()   # supprime les grilles mineures
  ) + 
  facet_wrap(~ periode_construction)

ggsave("graphs/ep_conso_5_usages_m2_density_by_periode_construction.png", width = 8, height = 6)

# Density of primary energy consumption by construction period
ggplot(df, aes(x = ep_conso_5_usages_m2)) +
  geom_vline(xintercept = dpe_thresholds, linetype = "dashed", color = "black", linewidth = 0.5) +
  geom_density(bw = 2, linewidth = 1) +
  labs(
    title = "Primary energy consumption distribution by dwelling type",
    x     = "Primary energy consumption (kWh/m²)",
    y     = "Density",
  ) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),  # supprime les grilles majeures
    panel.grid.minor = element_blank()   # supprime les grilles mineures
  ) + 
  facet_wrap(~ type_logement)

ggsave("graphs/ep_conso_5_usages_m2_density_by_type_logement.png", width = 8, height = 6)
